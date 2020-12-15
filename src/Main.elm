port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Dict.Extra
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html exposing (Html, a)
import Json.Decode as Decode
import Json.Encode as Encode
import Svg exposing (Svg)
import Svg.Attributes
import SvgParser


port addSimpleChar : () -> Cmd msg


port getSimpleChar : (Encode.Value -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { chars : List MyChar
    , simpleCharSvgs : SimpleCharSvgs
    , boxSize : Int
    , borderSize : Int
    , gridSize : Int
    , thumbnailGridSize : Int
    }


type MyChar
    = SimpleChar
        { char : Char
        , width : Int
        , height : Int
        , x : Int
        , y : Int
        }
    | CompoundChar
        { char : Char
        , components : List MyChar
        }


type MyCharType
    = SimpleCharType
    | CompoundCharType


type alias SimpleCharSvgs =
    Dict Char (Svg Msg)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chars = []
      , simpleCharSvgs = Dict.empty
      , boxSize = 34
      , borderSize = 1
      , gridSize = 20
      , thumbnailGridSize = 7
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddChar MyCharType
    | GetSimpleChar Encode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boxSize, borderSize } as model) =
    case msg of
        AddChar myCharType ->
            case myCharType of
                SimpleCharType ->
                    ( model, addSimpleChar () )

                CompoundCharType ->
                    ( model, Cmd.none )

        GetSimpleChar svgs ->
            case Decode.decodeValue decodeSimpleCharSvgs svgs of
                Ok newSimpleCharSvgs ->
                    ( { model
                        | chars =
                            Dict.foldl
                                (\char _ chars ->
                                    SimpleChar
                                        { char = char
                                        , width = boxSize - borderSize
                                        , height = boxSize - borderSize
                                        , x = borderSize
                                        , y = borderSize
                                        }
                                        :: chars
                                )
                                model.chars
                                newSimpleCharSvgs
                        , simpleCharSvgs =
                            Dict.merge
                                (\key a -> Dict.insert key a)
                                (\key a b -> Dict.insert key a)
                                (\key b -> Dict.insert key b)
                                newSimpleCharSvgs
                                model.simpleCharSvgs
                                Dict.empty
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    E.layout
        [ E.padding spacing.large ]
    <|
        E.column
            [ E.spacing spacing.large ]
            [ charsPanel SimpleCharType model
            , charsPanel CompoundCharType model
            ]


charsPanel : MyCharType -> Model -> E.Element Msg
charsPanel myCharType model =
    E.column
        [ E.width E.fill
        , E.spacing spacing.medium
        ]
        [ E.row
            [ E.spacing spacing.small
            , Font.size fontSize.title
            ]
            [ E.text <|
                (case myCharType of
                    SimpleCharType ->
                        "Simple"

                    CompoundCharType ->
                        "Compound"
                )
                    ++ " Characters"
            , addButton fontSize.title <| AddChar myCharType
            ]
        , E.wrappedRow
            [ E.width E.fill
            , E.spacing spacing.medium
            ]
          <|
            List.filterMap
                (\myChar ->
                    if isMyCharType myCharType myChar then
                        Just <| charCard model myChar

                    else
                        Nothing
                )
                model.chars
        ]


charCard : Model -> MyChar -> E.Element Msg
charCard { thumbnailGridSize, boxSize, simpleCharSvgs } myChar =
    E.column
        [ E.width <| E.px <| boxSize * thumbnailGridSize
        , Background.color palette.lightBg
        , Border.rounded spacing.medium
        ]
        [ E.el
            [ Font.size fontSize.large
            , Font.bold
            ]
          <|
            E.text (String.fromChar <| charFromMyChar myChar)
        , E.html <| renderChar thumbnailGridSize simpleCharSvgs myChar
        ]


charFromMyChar : MyChar -> Char
charFromMyChar myChar =
    case myChar of
        SimpleChar { char } ->
            char

        CompoundChar { char } ->
            char


isMyCharType : MyCharType -> MyChar -> Bool
isMyCharType myCharType myChar =
    case ( myCharType, myChar ) of
        ( SimpleCharType, SimpleChar _ ) ->
            True

        ( CompoundCharType, CompoundChar _ ) ->
            True

        _ ->
            False


renderChar : Int -> SimpleCharSvgs -> MyChar -> Svg Msg
renderChar gridSize simpleCharSvgs myChar =
    case myChar of
        SimpleChar { char, width, height, x, y } ->
            case Dict.get char simpleCharSvgs of
                Just svg ->
                    Svg.svg
                        [ Svg.Attributes.width <| String.fromInt (width * gridSize)
                        , Svg.Attributes.height <| String.fromInt (height * gridSize)
                        ]
                    <|
                        [ Svg.svg
                            [ Svg.Attributes.x <| String.fromInt x
                            , Svg.Attributes.y <| String.fromInt y
                            ]
                            [ svg ]
                        ]

                Nothing ->
                    -- impossible
                    Svg.text <| "Error rendering " ++ String.fromChar char

        CompoundChar { char, components } ->
            Svg.svg [] <|
                List.map (renderChar gridSize simpleCharSvgs) components


addButton : Float -> Msg -> E.Element Msg
addButton iconSize msg =
    Input.button
        []
        { label =
            E.html
                (FeatherIcons.plusCircle
                    |> FeatherIcons.withSize iconSize
                    |> FeatherIcons.toHtml []
                )
        , onPress =
            Just msg
        }


compoundCharsPanel : Model -> E.Element Msg
compoundCharsPanel model =
    E.column [] []



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    getSimpleChar GetSimpleChar


decodeSimpleCharSvgs : Decode.Decoder SimpleCharSvgs
decodeSimpleCharSvgs =
    Decode.map
        (Dict.Extra.mapKeys
            (\name ->
                case String.uncons name of
                    Just ( char, _ ) ->
                        char

                    Nothing ->
                        '?'
            )
        )
    <|
        Decode.dict decodeSimpleCharSvg


decodeSimpleCharSvg : Decode.Decoder (Svg Msg)
decodeSimpleCharSvg =
    Decode.map
        (SvgParser.parse >> Result.withDefault (Svg.g [] []))
        Decode.string



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


palette =
    { lightBg =
        E.rgb255 246 234 190
    }


spacing =
    { small =
        10
    , medium = 20
    , large = 30
    }


fontSize =
    { small =
        16
    , medium =
        20
    , large =
        30
    , title =
        40
    }
