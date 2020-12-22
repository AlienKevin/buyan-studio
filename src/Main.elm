module Main exposing (..)

import Browser
import Color
import Dict exposing (Dict, size)
import Dict.Extra
import Draggable
import Draggable.Events
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
import Html.Attributes
import Html5.DragDrop as DragDrop
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Math.Vector2 as Vector2 exposing (Vec2)
import SvgParser
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgTypes



---- MODEL ----


type alias Model =
    { chars : Dict Char MyChar
    , selectedChar : Maybe Char
    , simpleCharSvgs : SimpleCharSvgs
    , boxUnits : Int
    , borderUnits : Int
    , unitSize : Int
    , thumbnailUnitSize : Int
    , strokeWidth : Float
    , popUp : PopUp
    , newCompoundChar : String
    , showInputError : Bool
    , dragDropChar : DragDrop.Model Char ()
    , dragDropCharData : { char : Char }
    , drag : Draggable.State Id
    , activeComponentId : Maybe Id
    }


type alias Id =
    Int


type MyChar
    = SimpleChar
        { char : Char
        , id : Id
        , width : Float
        , height : Float
        , position : Vec2
        }
    | CompoundChar
        { char : Char
        , id : Id
        , width : Float
        , height : Float
        , position : Vec2
        , components : List MyChar
        }


emptyMyChar =
    SimpleChar
        { char = '?'
        , id = -1
        , width = 0
        , height = 0
        , position = Vector2.vec2 0 0
        }


type MyCharType
    = SimpleCharType
    | CompoundCharType


type alias SimpleCharSvgs =
    Dict Char (Svg Msg)


type PopUp
    = AddCompoundCharPopUp
    | NoPopUp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chars = Dict.empty
      , selectedChar = Nothing
      , simpleCharSvgs = Dict.empty
      , boxUnits = 34
      , borderUnits = 1
      , unitSize = 20
      , thumbnailUnitSize = 7
      , strokeWidth = 70
      , popUp = NoPopUp
      , newCompoundChar = ""
      , showInputError = False
      , dragDropChar = DragDrop.init
      , dragDropCharData = { char = '?' }
      , drag = Draggable.init
      , activeComponentId = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddChar MyCharType
    | SvgsSelected File (List File)
    | SvgsLoaded SimpleCharSvgs
    | EditChar MyChar
    | UpdateNewCompoundChar String
    | AddNewCompoundChar
    | ShowInputError
    | HideInputError
    | ClosePopUp
    | DragDropChar (DragDrop.Msg Char ())
    | OnDragBy Vec2
    | StartDragging Id
    | StopDragging
    | DragMsg (Draggable.Msg Id)
    | SetActiveComponentId Id


dragConfig : Draggable.Config Id Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vector2.vec2 dx dy |> OnDragBy)
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onClick SetActiveComponentId
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boxUnits, borderUnits, unitSize, chars, activeComponentId } as model) =
    case msg of
        AddChar myCharType ->
            case myCharType of
                SimpleCharType ->
                    ( model, Select.files [ "image/svg+xml" ] SvgsSelected )

                CompoundCharType ->
                    ( { model
                        | popUp =
                            AddCompoundCharPopUp
                      }
                    , Cmd.none
                    )

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
                        (\char _ ->
                            Dict.insert
                                char
                                (SimpleChar
                                    { char = char
                                    , id = -1
                                    , width = 100
                                    , height = 100
                                    , position = Vector2.vec2 0 0
                                    }
                                )
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
                    Just <| charFromMyChar myChar
                , activeComponentId =
                    Nothing
              }
            , Cmd.none
            )

        UpdateNewCompoundChar string ->
            ( { model
                | newCompoundChar =
                    string
              }
            , Cmd.none
            )

        AddNewCompoundChar ->
            let
                newChar =
                    case String.uncons model.newCompoundChar of
                        Just ( char, _ ) ->
                            char

                        Nothing ->
                            '?'

                newCompoundChar =
                    CompoundChar
                        { char = newChar
                        , id = -1
                        , width = 100
                        , height = 100
                        , position = Vector2.vec2 0 0
                        , components = []
                        }
            in
            ( { model
                | chars =
                    Dict.insert newChar newCompoundChar model.chars
                , selectedChar =
                    Just <| charFromMyChar newCompoundChar
                , popUp =
                    NoPopUp
              }
            , Cmd.none
            )

        ShowInputError ->
            let
                _ =
                    Debug.log "showInputError" ""
            in
            ( { model
                | showInputError = True
              }
            , Cmd.none
            )

        HideInputError ->
            ( { model
                | showInputError = False
              }
            , Cmd.none
            )

        ClosePopUp ->
            ( { model
                | popUp = NoPopUp
              }
            , Cmd.none
            )

        DragDropChar msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDropChar
            in
            ( case result of
                Nothing ->
                    { model
                        | dragDropChar =
                            model_
                        , dragDropCharData =
                            model.dragDropCharData
                        , chars =
                            model.chars
                    }

                Just ( componentChar, _, _ ) ->
                    { model
                        | dragDropChar =
                            DragDrop.init
                        , dragDropCharData =
                            { char = componentChar }
                        , chars =
                            case model.selectedChar of
                                Just selectedChar ->
                                    Dict.update
                                        selectedChar
                                        (Maybe.map
                                            (\myChar ->
                                                case myChar of
                                                    SimpleChar _ ->
                                                        myChar

                                                    CompoundChar ({ components } as compoundChar) ->
                                                        let
                                                            width =
                                                                50

                                                            height =
                                                                50

                                                            position =
                                                                Vector2.vec2 25 25

                                                            id =
                                                                List.length components

                                                            _ =
                                                                Debug.log "id" id
                                                        in
                                                        CompoundChar
                                                            { compoundChar
                                                                | components =
                                                                    components
                                                                        ++ [ case Dict.get componentChar model.chars of
                                                                                Just c ->
                                                                                    case c of
                                                                                        SimpleChar _ ->
                                                                                            SimpleChar
                                                                                                { char = componentChar
                                                                                                , id = id
                                                                                                , width = width
                                                                                                , height = height
                                                                                                , position = position
                                                                                                }

                                                                                        CompoundChar compound ->
                                                                                            CompoundChar
                                                                                                { char = componentChar
                                                                                                , id = id
                                                                                                , width = width
                                                                                                , height = height
                                                                                                , position = position
                                                                                                , components = compound.components
                                                                                                }

                                                                                Nothing ->
                                                                                    -- impossible
                                                                                    SimpleChar
                                                                                        { char = componentChar
                                                                                        , id = id
                                                                                        , width = width
                                                                                        , height = height
                                                                                        , position = position
                                                                                        }
                                                                           ]
                                                            }
                                            )
                                        )
                                        model.chars

                                Nothing ->
                                    model.chars
                    }
            , Cmd.none
            )

        OnDragBy delta ->
            ( { model
                | chars =
                    case model.selectedChar of
                        Just selectedChar ->
                            let
                                _ =
                                    Debug.log "activeComponentId" activeComponentId
                            in
                            Dict.update
                                selectedChar
                                (Maybe.map
                                    (updateComponent <|
                                        List.Extra.updateAt
                                            -- impossible
                                            (Maybe.withDefault -1 activeComponentId)
                                            (updatePosition
                                                (Vector2.add <|
                                                    Vector2.scale (100 / toFloat (boxUnits * unitSize))
                                                        delta
                                                )
                                            )
                                    )
                                )
                                chars

                        Nothing ->
                            chars
              }
            , Cmd.none
            )

        StartDragging id ->
            ( { model
                | activeComponentId =
                    Just id
              }
            , Cmd.none
            )

        StopDragging ->
            ( { model
                | activeComponentId =
                    Nothing
              }
            , Cmd.none
            )

        SetActiveComponentId id ->
            ( { model
                | activeComponentId =
                    Just id
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


updateComponent : (List MyChar -> List MyChar) -> MyChar -> MyChar
updateComponent func myChar =
    case myChar of
        SimpleChar _ ->
            myChar

        CompoundChar c ->
            CompoundChar
                { c
                    | components =
                        func c.components
                }


updatePosition : (Vec2 -> Vec2) -> MyChar -> MyChar
updatePosition func myChar =
    case myChar of
        SimpleChar c ->
            SimpleChar
                { c
                    | position =
                        func c.position
                }

        CompoundChar c ->
            CompoundChar
                { c
                    | position =
                        func c.position
                }



-- Requires: char to be in chars


getCharType : Dict Char MyChar -> Char -> MyCharType
getCharType chars char =
    case Dict.get char chars of
        Just myChar ->
            case myChar of
                SimpleChar _ ->
                    SimpleCharType

                CompoundChar _ ->
                    CompoundCharType

        Nothing ->
            -- impossible
            SimpleCharType



---- VIEW ----


view : Model -> Html Msg
view model =
    E.layout
        [ E.padding spacing.large
        , E.inFront <| popUp model
        ]
    <|
        E.row
            [ E.width E.fill
            , E.height E.fill
            , E.spacing spacing.large
            ]
            [ charPanels model
            , editor model
            ]


popUp : Model -> E.Element Msg
popUp ({ activeComponentId, boxUnits, thumbnailUnitSize, newCompoundChar, showInputError } as model) =
    case model.popUp of
        AddCompoundCharPopUp ->
            E.column
                ([ E.centerX
                 , E.centerY
                 , Background.color palette.lightBg
                 , E.width <| E.px <| boxUnits * thumbnailUnitSize
                 , E.height <| E.px <| boxUnits * thumbnailUnitSize
                 , E.spacing spacing.small
                 , Font.size fontSize.medium
                 , E.inFront <|
                    E.el
                        [ E.padding spacing.tiny ]
                    <|
                        iconButton
                            { icon =
                                FeatherIcons.x
                            , size =
                                fontSize.medium
                            , onPress =
                                Just ClosePopUp
                            }
                 ]
                    ++ highlightBorder 6 Border.dashed
                )
                [ Input.text
                    [ E.width <| E.px <| fontSize.medium * 5
                    , E.centerX
                    , E.centerY
                    , E.onRight <|
                        let
                            inputLength =
                                String.length newCompoundChar

                            isValidNewChar =
                                inputLength == 1
                        in
                        E.el
                            ([ E.paddingXY spacing.small 0 ]
                                ++ (if isValidNewChar then
                                        []

                                    else
                                        [ Events.onMouseEnter ShowInputError
                                        , Events.onMouseLeave HideInputError
                                        ]
                                   )
                            )
                        <|
                            iconButton
                                { icon =
                                    if isValidNewChar then
                                        FeatherIcons.checkCircle

                                    else
                                        FeatherIcons.alertTriangle
                                , size =
                                    fontSize.title
                                , onPress =
                                    if isValidNewChar then
                                        Just AddNewCompoundChar

                                    else
                                        Nothing
                                }
                    , E.below <|
                        if showInputError then
                            E.el
                                [ E.centerX
                                , E.padding spacing.small
                                ]
                            <|
                                E.text "Accept only 1 character"

                        else
                            E.none
                    ]
                    { onChange =
                        UpdateNewCompoundChar
                    , text =
                        newCompoundChar
                    , placeholder =
                        Nothing
                    , label =
                        Input.labelAbove []
                            (E.text "Character")
                    }
                ]

        NoPopUp ->
            E.none


editor : Model -> E.Element Msg
editor ({ activeComponentId, selectedChar, chars, simpleCharSvgs, boxUnits, unitSize, borderUnits, strokeWidth } as model) =
    let
        dropId =
            DragDrop.getDropId model.dragDropChar

        droppablePosition =
            DragDrop.getDroppablePosition model.dragDropChar

        highlight =
            case dropId of
                Just _ ->
                    case droppablePosition of
                        Nothing ->
                            []

                        Just _ ->
                            [ Background.color palette.lightBg ]

                Nothing ->
                    []
    in
    E.el
        ([ E.inFront <|
            case selectedChar of
                Just char ->
                    E.html <|
                        renderChar
                            { unitSize = unitSize
                            , boxUnits = boxUnits
                            , borderUnits = borderUnits
                            , simpleCharSvgs = simpleCharSvgs
                            , activeComponentId = activeComponentId
                            , strokeWidth = strokeWidth
                            , isThumbnail = False
                            }
                            (Maybe.withDefault emptyMyChar <|
                                -- impossible
                                Dict.get char chars
                            )

                Nothing ->
                    E.none
         ]
            ++ highlight
            ++ (List.map E.htmlAttribute <| DragDrop.droppable DragDropChar ())
        )
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
charPanel myCharType ({ boxUnits, thumbnailUnitSize } as model) =
    let
        cards =
            List.filterMap
                (\myChar ->
                    if isMyCharType myCharType myChar then
                        Just <| charCard model myChar

                    else
                        Nothing
                )
                (Dict.values model.chars)
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
            , iconButton
                { icon =
                    FeatherIcons.plusCircle
                , size =
                    fontSize.title
                , onPress =
                    Just <| AddChar myCharType
                }
            ]
        , E.wrappedRow
            [ E.width E.fill
            , E.height E.fill
            , E.height
                (if List.length cards == 0 then
                    E.shrink

                 else
                    E.fill
                        |> E.minimum (boxUnits * thumbnailUnitSize + fontSize.medium + spacing.medium)
                )
            , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
            , E.htmlAttribute <| Html.Attributes.style "overflow-x" "hidden"
            , E.spacing spacing.medium
            ]
          <|
            cards
        ]


charCard : Model -> MyChar -> E.Element Msg
charCard { activeComponentId, unitSize, thumbnailUnitSize, boxUnits, borderUnits, strokeWidth, simpleCharSvgs, selectedChar } myChar =
    let
        char =
            charFromMyChar myChar
    in
    E.column
        (([ E.width <| E.px <| boxUnits * thumbnailUnitSize
          , Background.color palette.lightBg
          , Border.rounded spacing.medium
          , Events.onClick <| EditChar myChar
          , E.pointer
          ]
            ++ (List.map E.htmlAttribute <|
                    DragDrop.draggable DragDropChar char
               )
         )
            ++ (case selectedChar of
                    Just selected ->
                        if selected == char then
                            highlightBorder 0 Border.solid

                        else
                            []

                    Nothing ->
                        []
               )
        )
        [ E.el
            [ Font.size fontSize.large
            , Font.bold
            ]
          <|
            E.text <|
                String.fromChar <|
                    char
        , E.html <|
            renderChar
                { unitSize = thumbnailUnitSize
                , boxUnits = boxUnits
                , borderUnits = borderUnits
                , strokeWidth = strokeWidth * toFloat thumbnailUnitSize / toFloat unitSize
                , simpleCharSvgs = simpleCharSvgs
                , activeComponentId =
                    Maybe.andThen
                        (\selected ->
                            if selected == char then
                                activeComponentId

                            else
                                Nothing
                        )
                        selectedChar
                , isThumbnail = True
                }
                myChar
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


renderChar :
    { isThumbnail : Bool
    , unitSize : Int
    , boxUnits : Int
    , borderUnits : Int
    , strokeWidth : Float
    , simpleCharSvgs : SimpleCharSvgs
    , activeComponentId : Maybe Id
    }
    -> MyChar
    -> Svg Msg
renderChar { isThumbnail, unitSize, boxUnits, borderUnits, strokeWidth, simpleCharSvgs, activeComponentId } myChar =
    let
        size =
            toFloat ((boxUnits - 2 * borderUnits) * unitSize) - strokeWidth

        offset =
            toFloat (borderUnits * unitSize) + strokeWidth / 2

        charClassName =
            "char-with-size-" ++ (String.fromInt <| round strokeWidth)
    in
    Svg.svg
        [ SvgAttributes.width <| SvgTypes.px <| toFloat (boxUnits * unitSize)
        , SvgAttributes.height <| SvgTypes.px <| toFloat (boxUnits * unitSize)
        , SvgAttributes.class [ charClassName ]
        ]
        [ Svg.defs []
            [ Svg.style []
                [ TypedSvg.Core.text <|
                    "."
                        ++ charClassName
                        ++ """ * {
                        fill: none;
                        stroke: #000;
                        stroke-linecap: round;
                        stroke-miterlimit: 10;
                        stroke-width: """
                        ++ String.fromFloat strokeWidth
                        ++ """ !important;
                        stroke-linejoin: round;
                        vector-effect: non-scaling-stroke;
                    }
                    #active-component-border {
                        stroke-width: 2px !important;
                        stroke: """
                        ++ (Color.toCssString <| toColor palette.darkFg)
                    ++ """;
                    }
                    svg {
                        overflow: visible
                    }
                    """
                ]
            ]
        , Svg.svg
            [ SvgAttributes.width <| SvgTypes.px size
            , SvgAttributes.height <| SvgTypes.px size
            , SvgAttributes.x <| SvgTypes.px offset
            , SvgAttributes.y <| SvgTypes.px offset
            ]
            [ renderCharHelper
                { unitSize = unitSize
                , boxUnits = boxUnits
                , simpleCharSvgs = simpleCharSvgs
                , activeComponentId = activeComponentId
                , isThumbnail = isThumbnail
                }
                0
                myChar
            ]
        ]


renderCharHelper :
    { unitSize : Int
    , boxUnits : Int
    , simpleCharSvgs : SimpleCharSvgs
    , activeComponentId : Maybe Id
    , isThumbnail : Bool
    }
    -> Int
    -> MyChar
    -> Svg Msg
renderCharHelper { unitSize, boxUnits, simpleCharSvgs, activeComponentId, isThumbnail } level myChar =
    let
        id =
            getId myChar

        levelwiseId =
            id + (level - 1) * 10

        constraint width height position contents =
            Svg.svg
                ([ SvgAttributes.x <| SvgTypes.Percent <| Vector2.getX position
                 , SvgAttributes.y <| SvgTypes.Percent <| Vector2.getY position
                 , SvgAttributes.width <| SvgTypes.Percent width
                 , SvgAttributes.height <| SvgTypes.Percent height
                 ]
                    ++ (if isThumbnail || level > 1 then
                            []

                        else
                            [ Draggable.mouseTrigger levelwiseId DragMsg ]
                       )
                )
            <|
                if Just levelwiseId == activeComponentId then
                    Svg.rect
                        [ SvgAttributes.id "active-component-border"
                        , SvgAttributes.width <| SvgTypes.Percent 100
                        , SvgAttributes.height <| SvgTypes.Percent 100
                        , SvgAttributes.fill <| SvgTypes.PaintNone
                        , SvgAttributes.stroke <| SvgTypes.Paint <| toColor palette.lightFg
                        ]
                        []
                        :: contents

                else
                    contents
    in
    case myChar of
        SimpleChar { char, width, height, position } ->
            case Dict.get char simpleCharSvgs of
                Just svg ->
                    constraint width height position [ svg ]

                Nothing ->
                    -- impossible
                    TypedSvg.Core.text <| "Error rendering " ++ String.fromChar char

        CompoundChar { char, width, height, position, components } ->
            constraint width height position <|
                List.map
                    (renderCharHelper
                        { unitSize = unitSize
                        , boxUnits = boxUnits
                        , simpleCharSvgs = simpleCharSvgs
                        , activeComponentId = activeComponentId
                        , isThumbnail = isThumbnail
                        }
                        (level + 1)
                    )
                    components


getId : MyChar -> Id
getId myChar =
    case myChar of
        SimpleChar { id } ->
            id

        CompoundChar { id } ->
            id


iconButton : { icon : FeatherIcons.Icon, size : Float, onPress : Maybe Msg } -> E.Element Msg
iconButton { icon, size, onPress } =
    Input.button
        []
        { label =
            E.html
                (icon
                    |> FeatherIcons.withSize size
                    |> FeatherIcons.toHtml []
                )
        , onPress =
            onPress
        }


highlightBorder : Int -> E.Attribute Msg -> List (E.Attribute Msg)
highlightBorder borderWidth borderStyle =
    [ Border.rounded spacing.medium
    , Border.color palette.lightFg
    , Border.width borderWidth
    , borderStyle
    , Border.glow palette.lightFg 10
    ]


compoundCharsPanel : Model -> E.Element Msg
compoundCharsPanel model =
    E.column [] []



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions ({ drag } as model) =
    Draggable.subscriptions DragMsg drag


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
    , lightFg =
        toElmUiColor Color.lightBlue
    , darkFg =
        toElmUiColor Color.lightPurple
    }


spacing =
    { tiny = 5
    , small = 10
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


toElmUiColor : Color.Color -> E.Color
toElmUiColor color =
    E.fromRgb <| Color.toRgba color


toColor : E.Color -> Color.Color
toColor color =
    Color.fromRgba <| E.toRgb color


toCssString : E.Color -> String
toCssString color =
    Color.toCssString <| Color.fromRgba <| E.toRgb color
