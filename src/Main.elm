port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Dict.Extra
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
    { simpleChars : List SimpleChar
    , compoundChars : List CompoundChar
    , simpleCharSvgs : SimpleCharSvgs
    , boxSize : Int
    , borderSize : Int
    , gridSize : Int
    , thumbnailGridSize : Int
    }


type alias SimpleChar =
    { char : Char
    , width : Int
    , height : Int
    , x : Int
    , y : Int
    }


type alias CompoundChar =
    List SimpleChar


type alias SimpleCharSvgs =
    Dict Char (Svg Msg)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { simpleChars = []
      , compoundChars = []
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
    = AddSimpleChar
    | GetSimpleChar Encode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boxSize, borderSize } as model) =
    case msg of
        AddSimpleChar ->
            ( model, addSimpleChar () )

        GetSimpleChar svgs ->
            case Decode.decodeValue decodeSimpleCharSvgs svgs of
                Ok newSimpleCharSvgs ->
                    ( { model
                        | simpleChars =
                            Dict.foldl
                                (\char _ chars ->
                                    { char = char
                                    , width = boxSize - borderSize
                                    , height = boxSize - borderSize
                                    , x = borderSize
                                    , y = borderSize
                                    }
                                        :: chars
                                )
                                model.simpleChars
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
        [ E.padding spacing.large ] <|
        E.row []
            [ simpleCharsPanel model
            , compoundCharsPanel model
            ]


simpleCharsPanel : Model -> E.Element Msg
simpleCharsPanel ({ boxSize, thumbnailGridSize } as model) =
    E.column
        [ E.width E.fill
        , E.spacing spacing.medium
        ]
        [ E.row
            [ E.spacing spacing.small
            , Font.size fontSize.title
            ]
            [ E.text "Simple Characters"
            , addButton AddSimpleChar
            ]
        , E.wrappedRow
            [ E.width E.fill
            , E.spacing spacing.medium
            ]
          <|
            List.foldl
                (\{ char, width, height } list ->
                    case Dict.get char model.simpleCharSvgs of
                        Just svg ->
                            charCard
                                { char = char
                                , svg =
                                    Svg.svg
                                        [ Svg.Attributes.width <| String.fromInt (width * thumbnailGridSize)
                                        , Svg.Attributes.height <| String.fromInt (height * thumbnailGridSize)
                                        ]
                                        [ svg ]
                                , thumbnailGridSize = thumbnailGridSize
                                , boxSize = boxSize
                                }
                                :: list

                        Nothing ->
                            -- impossible
                            list
                )
                []
                model.simpleChars
        ]


charCard :
    { char : Char
    , svg : Html Msg
    , thumbnailGridSize : Int
    , boxSize : Int
    }
    -> E.Element Msg
charCard { char, svg, thumbnailGridSize, boxSize } =
    E.column
        [ E.width <| E.px <| boxSize * thumbnailGridSize
        , Background.color palette.lightBg
        , Border.rounded spacing.medium
        ]
        [ E.el
            [ Font.size fontSize.large
            , Font.bold
            ] <|
            E.text (String.fromChar char)
        , E.html svg
        ]


addButton : Msg -> E.Element Msg
addButton msg =
    Input.button []
        { label =
            E.text "Add"
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