module Main exposing (..)

import Browser
import Color
import Dict exposing (Dict, size)
import Dict.Extra
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import File exposing (File)
import File.Select as Select
import Html exposing (Html, a)
import Json.Decode as Decode
import Json.Encode as Encode
import SvgParser
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgTypes



---- MODEL ----


type alias Model =
    { chars : List MyChar
    , selectedChar : Maybe MyChar
    , simpleCharSvgs : SimpleCharSvgs
    , boxUnits : Int
    , borderUnits : Int
    , unitSize : Int
    , thumbnailUnitSize : Int
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
      , selectedChar = Nothing
      , simpleCharSvgs = Dict.empty
      , boxUnits = 34
      , borderUnits = 1
      , unitSize = 20
      , thumbnailUnitSize = 7
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddChar MyCharType
    | SvgsSelected File (List File)
    | SvgsLoaded SimpleCharSvgs
    | EditChar MyChar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boxUnits, borderUnits } as model) =
    case msg of
        AddChar myCharType ->
            case myCharType of
                SimpleCharType ->
                    ( model, Select.files [ "image/svg+xml" ] SvgsSelected )

                CompoundCharType ->
                    ( model, Cmd.none )

        SvgsSelected first rest ->
            ( model
            , Task.perform SvgsLoaded <|
                Task.map Dict.fromList <|
                    Task.sequence <|
                        List.map
                            (\file ->
                                Task.map
                                    (\svgString ->
                                        ( case String.uncons <| File.name file of
                                            Just ( char, _ ) ->
                                                char

                                            Nothing ->
                                                '?'
                                        , case SvgParser.parse svgString of
                                            Ok svg ->
                                                svg

                                            Err err ->
                                                let
                                                    _ =
                                                        Debug.log "Error parsing svg" err
                                                in
                                                Svg.g [] []
                                        )
                                    )
                                <|
                                    File.toString file
                            )
                            (first :: rest)
            )

        SvgsLoaded svgs ->
            ( { model
                | chars =
                    Dict.foldl
                        (\char _ chars ->
                            SimpleChar
                                { char = char
                                , width = boxUnits - borderUnits * 2
                                , height = boxUnits - borderUnits * 2
                                , x = borderUnits
                                , y = borderUnits
                                }
                                :: chars
                        )
                        model.chars
                        svgs
                , simpleCharSvgs =
                    Dict.merge
                        (\key a -> Dict.insert key a)
                        (\key a b -> Dict.insert key a)
                        (\key b -> Dict.insert key b)
                        svgs
                        model.simpleCharSvgs
                        Dict.empty
              }
            , Cmd.none
            )

        EditChar myChar ->
            ( { model
                | selectedChar =
                    Just myChar
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    E.layout
        [ E.padding spacing.large ]
    <|
        E.row
            [ E.width E.fill
            , E.height E.fill
            , E.spacing spacing.large
            ]
            [ charPanels model
            , editor model
            ]


editor : Model -> E.Element Msg
editor ({ selectedChar, simpleCharSvgs, boxUnits, unitSize, borderUnits } as model) =
    E.el
        [ E.inFront <|
            case selectedChar of
                Just char ->
                    E.html <| renderChar unitSize boxUnits simpleCharSvgs char

                Nothing ->
                    E.none
        ]
    <|
        E.html <|
            gridBackground { boxUnits = boxUnits, unitSize = unitSize, borderUnits = borderUnits }


gridBackground :
    { boxUnits : Int
    , borderUnits : Int
    , unitSize : Int
    }
    -> Svg Msg
gridBackground { boxUnits, borderUnits, unitSize } =
    let
        boxSize =
            boxUnits * unitSize

        borderSize =
            borderUnits * unitSize

        strokeWidth =
            { normal =
                2
            , thick =
                3
            }
    in
    Svg.svg
        [ SvgAttributes.width <| SvgTypes.px <| toFloat <| boxSize
        , SvgAttributes.height <| SvgTypes.px <| toFloat <| boxSize
        , SvgAttributes.stroke <| SvgTypes.Paint <| Color.lightBlue
        ]
    <|
        [ gridOutline
            { x = strokeWidth.thick
            , y = strokeWidth.thick
            , strokeWidth =
                strokeWidth.thick
            , size =
                boxSize - strokeWidth.thick * 2
            }
        , gridOutline
            { x = borderSize
            , y = borderSize
            , strokeWidth =
                strokeWidth.thick
            , size =
                boxSize - borderSize * 2
            }
        ]
            ++ List.map
                (\units ->
                    Svg.g
                        (if units == (round <| toFloat (boxUnits - 4) / 2) then
                            [ SvgAttributes.strokeWidth <| SvgTypes.px strokeWidth.thick ]

                         else
                            []
                        )
                        [ Svg.line
                            [ SvgAttributes.x1 <| SvgTypes.px <| toFloat <| (2 + units) * unitSize
                            , SvgAttributes.y1 <| SvgTypes.px <| toFloat <| unitSize
                            , SvgAttributes.x2 <| SvgTypes.px <| toFloat <| (2 + units) * unitSize
                            , SvgAttributes.y2 <| SvgTypes.px <| toFloat <| (boxUnits - 1) * unitSize
                            ]
                            []
                        , Svg.line
                            [ SvgAttributes.x1 <| SvgTypes.px <| toFloat <| unitSize
                            , SvgAttributes.y1 <| SvgTypes.px <| toFloat <| (2 + units) * unitSize
                            , SvgAttributes.x2 <| SvgTypes.px <| toFloat <| (boxUnits - 1) * unitSize
                            , SvgAttributes.y2 <| SvgTypes.px <| toFloat <| (2 + units) * unitSize
                            ]
                            []
                        ]
                )
                (List.range 0 (boxUnits - 4))


gridOutline : { x : Int, y : Int, strokeWidth : Float, size : Int } -> Svg Msg
gridOutline { x, y, strokeWidth, size } =
    Svg.rect
        [ SvgAttributes.width <| SvgTypes.px <| toFloat size
        , SvgAttributes.height <| SvgTypes.px <| toFloat size
        , SvgAttributes.x <| SvgTypes.px <| toFloat x
        , SvgAttributes.y <| SvgTypes.px <| toFloat y
        , SvgAttributes.fill <| SvgTypes.PaintNone
        , SvgAttributes.strokeWidth <| SvgTypes.px strokeWidth
        ]
        []


charPanels : Model -> E.Element Msg
charPanels model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        , E.spacing spacing.large
        ]
        [ charPanel SimpleCharType model
        , charPanel CompoundCharType model
        ]


charPanel : MyCharType -> Model -> E.Element Msg
charPanel myCharType model =
    let
        cards =
            List.filterMap
                (\myChar ->
                    if isMyCharType myCharType myChar then
                        Just <| charCard model myChar

                    else
                        Nothing
                )
                model.chars
    in
    E.column
        [ E.spacing spacing.medium
        , E.height
            (if List.length cards <= 3 then
                E.shrink

             else
                E.fill
            )
        , E.centerY
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
            , E.height E.fill
            , E.height
                (if List.length cards == 0 then
                    E.shrink

                 else
                    E.fill
                        |> E.minimum 500
                        |> E.maximum 1000
                )
            , E.scrollbarY
            , E.spacing spacing.medium
            ]
          <|
            cards
        ]


charCard : Model -> MyChar -> E.Element Msg
charCard { thumbnailUnitSize, boxUnits, simpleCharSvgs } myChar =
    E.column
        [ E.width <| E.px <| boxUnits * thumbnailUnitSize
        , Background.color palette.lightBg
        , Border.rounded spacing.medium
        , Events.onClick <| EditChar myChar
        ]
        [ E.el
            [ Font.size fontSize.large
            , Font.bold
            ]
          <|
            E.text (String.fromChar <| charFromMyChar myChar)
        , E.html <| renderChar thumbnailUnitSize boxUnits simpleCharSvgs myChar
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


renderChar : Int -> Int -> SimpleCharSvgs -> MyChar -> Svg Msg
renderChar unitSize boxUnits simpleCharSvgs myChar =
    case myChar of
        SimpleChar { char, width, height, x, y } ->
            case Dict.get char simpleCharSvgs of
                Just svg ->
                    Svg.svg
                        [ SvgAttributes.width <| SvgTypes.px <| toFloat (boxUnits * unitSize)
                        , SvgAttributes.height <| SvgTypes.px <| toFloat (boxUnits * unitSize)
                        ]
                        [ Svg.svg
                            [ SvgAttributes.x <| SvgTypes.px <| toFloat (x * unitSize)
                            , SvgAttributes.y <| SvgTypes.px <| toFloat (y * unitSize)
                            ]
                            [ Svg.svg
                                [ SvgAttributes.width <| SvgTypes.px <| toFloat (width * unitSize)
                                , SvgAttributes.height <| SvgTypes.px <| toFloat (height * unitSize)
                                ]
                                [ svg ]
                            ]
                        ]

                Nothing ->
                    -- impossible
                    TypedSvg.Core.text <| "Error rendering " ++ String.fromChar char

        CompoundChar { char, components } ->
            Svg.svg [] <|
                List.map (renderChar unitSize boxUnits simpleCharSvgs) components


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
    Sub.none


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
