port module Main exposing (..)

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
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html5.DragDrop as DragDrop
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Math.Vector2 as Vector2 exposing (Vec2)
import SvgParser
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgTypes


port addSimpleCharsPort : () -> Cmd msg


port getSimpleCharsPort : (Encode.Value -> msg) -> Sub msg


port saveModelPort : Value -> Cmd msg


port getModelPort : (Value -> msg) -> Sub msg


port pageUnloadingPort : (() -> msg) -> Sub msg



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
    , isInputErrorShown : Bool
    , dragDropChar : DragDrop.Model Char ()
    , dragDropCharData : { char : Char }
    , drag : Draggable.State Id
    , activeComponentId : Maybe Id
    }


type alias SavedModel =
    { chars : Dict Char MyChar
    , strokeWidth : Float
    }


type alias Id =
    Int


type MyChar
    = SimpleChar MyCharRef
    | CompoundChar MyCharRef (List MyCharRef)


type alias MyCharRef =
    { char : Char
    , id : Id
    , width : Float
    , height : Float
    , position : Vec2
    }


emptyMyChar : MyChar
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
      , isInputErrorShown = False
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
    | GetSimpleChars Value
    | SelectChar MyChar
    | UpdatePendingCompoundChar String
    | AddPendingCompoundChar
    | ShowInputError
    | HideInputError
    | ClosePopUp
    | DragDropChar (DragDrop.Msg Char ())
    | OnDragBy Vec2
    | StartDragging Id
    | StopDragging
    | DragMsg (Draggable.Msg Id)
    | SetActiveComponentId Id
    | GotModel Value
    | SaveModel ()


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
            addChar myCharType model

        GetSimpleChars svgsJson ->
            getSimpleChars svgsJson model

        SelectChar myChar ->
            selectChar myChar model

        UpdatePendingCompoundChar charInput ->
            updatePendingCompoundChar charInput model

        AddPendingCompoundChar ->
            addPendingCompoundChar model

        ShowInputError ->
            showInputError model

        HideInputError ->
            hideInputError model

        ClosePopUp ->
            closePopUp model

        DragDropChar msg_ ->
            dragDropChar msg_ model

        OnDragBy delta ->
            onDragBy delta model

        StartDragging id ->
            startDragging id model

        StopDragging ->
            stopDragging model

        SetActiveComponentId id ->
            setActiveComponentId id model

        DragMsg msg_ ->
            dragMsg msg_ model

        GotModel savedModelJson ->
            gotModel savedModelJson model

        SaveModel _ ->
            saveModel model


saveModel : Model -> ( Model, Cmd Msg )
saveModel model =
    ( model, saveModelPort <| encodeModel model )


encodeModel : Model -> Value
encodeModel { chars, simpleCharSvgs, strokeWidth } =
    Encode.object
        [ ( "chars", Encode.dict String.fromChar encodeMyChar chars )
        , ( "strokeWidth", Encode.float strokeWidth )
        ]


encodeChar : Char -> Value
encodeChar =
    Encode.string << String.fromChar


encodeMyChar : MyChar -> Value
encodeMyChar myChar =
    case myChar of
        SimpleChar ref ->
            Encode.object
                [ ( "type", Encode.string "SimpleChar" )
                , ( "reference", encodeMyCharRef ref )
                ]

        CompoundChar ref components ->
            Encode.object
                [ ( "type", Encode.string "CompoundChar" )
                , ( "reference", encodeMyCharRef ref )
                , ( "components", Encode.list encodeMyCharRef components )
                ]


encodeMyCharRef : MyCharRef -> Value
encodeMyCharRef { char, id, width, height, position } =
    -- { char : Char
    -- , id : Id
    -- , width : Float
    -- , height : Float
    -- , position : Vec2
    -- }
    Encode.object
        [ ( "char", encodeChar char )
        , ( "id", encodeId id )
        , ( "width", Encode.float width )
        , ( "height", Encode.float height )
        , ( "position", encodeVec2 position )
        ]


encodeVec2 : Vec2 -> Value
encodeVec2 vec =
    Encode.object
        [ ( "x", Encode.float <| Vector2.getX vec )
        , ( "y", Encode.float <| Vector2.getY vec )
        ]


encodeId : Id -> Value
encodeId =
    Encode.int


gotModel : Value -> Model -> ( Model, Cmd Msg )
gotModel savedModelJson model =
    ( case Decode.decodeValue decodeSavedModel savedModelJson of
        Ok { chars, strokeWidth } ->
            { model
                | chars =
                    chars
                , strokeWidth =
                    strokeWidth
            }

        Err err ->
            let
                _ =
                    Debug.log "err" err
            in
            model
    , Cmd.none
    )


decodeSavedModel : Decoder SavedModel
decodeSavedModel =
    Decode.map2 SavedModel
        (Decode.field "chars"
            (Decode.map (Dict.Extra.mapKeys charFromString) <|
                Decode.dict decodeMyChar
            )
        )
        (Decode.field "strokeWidth" Decode.float)


decodeChar : Decoder Char
decodeChar =
    Decode.map charFromString Decode.string


charFromString : String -> Char
charFromString string =
    case String.uncons string of
        Just ( firstChar, _ ) ->
            firstChar

        Nothing ->
            '?'


decodeMyChar : Decoder MyChar
decodeMyChar =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\myCharType ->
                case myCharType of
                    "SimpleChar" ->
                        Decode.map SimpleChar
                            (Decode.field "reference" decodeMyCharRef)

                    "CompoundChar" ->
                        Decode.map2 CompoundChar
                            (Decode.field "reference" decodeMyCharRef)
                            (Decode.field "components" (Decode.list decodeMyCharRef))

                    _ ->
                        Decode.fail <|
                            "Trying to decode MyChar, but "
                                ++ myCharType
                                ++ " is not supported."
            )


decodeMyCharRef : Decoder MyCharRef
decodeMyCharRef =
    -- { char : Char
    -- , id : Id
    -- , width : Float
    -- , height : Float
    -- , position : Vec2
    -- }
    Decode.map5 MyCharRef
        (Decode.field "char" decodeChar)
        (Decode.field "id" decodeId)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)
        (Decode.field "position" decodeVec2)


decodeVec2 : Decoder Vec2
decodeVec2 =
    Decode.map2 Vector2.vec2
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


decodeId : Decoder Id
decodeId =
    Decode.int


dragMsg : Draggable.Msg Id -> Model -> ( Model, Cmd Msg )
dragMsg msg model =
    Draggable.update dragConfig msg model


setActiveComponentId : Id -> Model -> ( Model, Cmd Msg )
setActiveComponentId id model =
    ( { model
        | activeComponentId =
            Just id
      }
    , Cmd.none
    )


stopDragging : Model -> ( Model, Cmd Msg )
stopDragging model =
    ( { model
        | activeComponentId =
            Nothing
      }
    , Cmd.none
    )


startDragging : Id -> Model -> ( Model, Cmd Msg )
startDragging id model =
    ( { model
        | activeComponentId =
            Just id
      }
    , Cmd.none
    )


onDragBy : Vec2 -> Model -> ( Model, Cmd Msg )
onDragBy delta ({ activeComponentId, boxUnits, unitSize, chars } as model) =
    ( { model
        | chars =
            case model.selectedChar of
                Just selectedChar ->
                    Dict.update
                        selectedChar
                        (Maybe.map
                            (updateComponent <|
                                let
                                    factor =
                                        100 / toFloat (boxUnits * unitSize)
                                in
                                List.Extra.updateAt
                                    -- impossible
                                    (Maybe.withDefault -1 activeComponentId)
                                    (updatePosition
                                        (Vector2.add <| Vector2.scale factor delta)
                                    )
                            )
                        )
                        chars

                Nothing ->
                    chars
      }
    , Cmd.none
    )


dragDropChar : DragDrop.Msg Char () -> Model -> ( Model, Cmd Msg )
dragDropChar msg_ model =
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
                    Maybe.map
                        (\selectedChar ->
                            Dict.update
                                selectedChar
                                (Maybe.map <|
                                    addComponentToMyChar model.chars componentChar
                                )
                                model.chars
                        )
                        model.selectedChar
                        |> Maybe.withDefault model.chars
            }
    , Cmd.none
    )


addComponentToMyChar : Dict Char MyChar -> Char -> MyChar -> MyChar
addComponentToMyChar chars componentChar myChar =
    case myChar of
        SimpleChar _ ->
            myChar

        CompoundChar compoundChar components ->
            let
                width =
                    50

                height =
                    50

                position =
                    Vector2.vec2 25 25

                id =
                    List.length components

                newComponent =
                    { char = componentChar
                    , id = id
                    , width = width
                    , height = height
                    , position = position
                    }
            in
            CompoundChar
                compoundChar
                (components ++ [ newComponent ])


closePopUp : Model -> ( Model, Cmd Msg )
closePopUp model =
    ( { model
        | popUp = NoPopUp
      }
    , Cmd.none
    )


hideInputError : Model -> ( Model, Cmd Msg )
hideInputError model =
    ( { model
        | isInputErrorShown = False
      }
    , Cmd.none
    )


showInputError : Model -> ( Model, Cmd Msg )
showInputError model =
    ( { model
        | isInputErrorShown = True
      }
    , Cmd.none
    )


addPendingCompoundChar : Model -> ( Model, Cmd Msg )
addPendingCompoundChar model =
    let
        newChar =
            charFromString model.newCompoundChar

        newCompoundChar =
            CompoundChar
                { char = newChar
                , id = -1
                , width = 100
                , height = 100
                , position = Vector2.vec2 0 0
                }
                []
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


updatePendingCompoundChar : String -> Model -> ( Model, Cmd Msg )
updatePendingCompoundChar charInput model =
    ( { model
        | newCompoundChar =
            charInput
      }
    , Cmd.none
    )


selectChar : MyChar -> Model -> ( Model, Cmd Msg )
selectChar myChar model =
    ( { model
        | selectedChar =
            Just <| charFromMyChar myChar
        , activeComponentId =
            Nothing
      }
    , Cmd.none
    )


getSimpleChars : Value -> Model -> ( Model, Cmd Msg )
getSimpleChars svgsJson model =
    ( case Decode.decodeValue decodeSimpleCharSvgs svgsJson of
        Ok svgs ->
            { model
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

        Err err ->
            let
                _ =
                    Debug.log "err" err
            in
            model
    , Cmd.none
    )


addChar : MyCharType -> Model -> ( Model, Cmd Msg )
addChar myCharType model =
    case myCharType of
        SimpleCharType ->
            ( model, addSimpleCharsPort () )

        CompoundCharType ->
            ( { model
                | popUp =
                    AddCompoundCharPopUp
              }
            , Cmd.none
            )


updateComponent : (List MyCharRef -> List MyCharRef) -> MyChar -> MyChar
updateComponent func myChar =
    case myChar of
        SimpleChar _ ->
            myChar

        CompoundChar c components ->
            CompoundChar c (func components)


updatePosition : (Vec2 -> Vec2) -> MyCharRef -> MyCharRef
updatePosition func myCharRef =
    { myCharRef
        | position =
            func myCharRef.position
    }



-- Requires: char to be in chars


getCharType : Dict Char MyChar -> Char -> MyCharType
getCharType chars char =
    case Dict.get char chars of
        Just myChar ->
            case myChar of
                SimpleChar _ ->
                    SimpleCharType

                CompoundChar _ _ ->
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
popUp ({ activeComponentId, boxUnits, thumbnailUnitSize, newCompoundChar, isInputErrorShown } as model) =
    case model.popUp of
        AddCompoundCharPopUp ->
            let
                inputLength =
                    String.length newCompoundChar

                isValidNewChar =
                    inputLength == 1
            in
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
                    , onEnter <|
                        if isValidNewChar then
                            Just AddPendingCompoundChar

                        else
                            Nothing
                    , E.onRight <|
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
                                        Just AddPendingCompoundChar

                                    else
                                        Nothing
                                }
                    , E.below <|
                        if isInputErrorShown then
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
                        UpdatePendingCompoundChar
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


onEnter : Maybe Msg -> E.Attribute Msg
onEnter =
    -- only used a dummy default attribute
    Maybe.withDefault (E.htmlAttribute (Html.Attributes.selected True))
        << Maybe.map
            (\msg ->
                E.htmlAttribute
                    (Html.Events.on "keyup"
                        (Decode.field "key" Decode.string
                            |> Decode.andThen
                                (\key ->
                                    if key == "Enter" then
                                        Decode.succeed msg

                                    else
                                        Decode.fail "Not the enter key"
                                )
                        )
                    )
            )


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
                            , chars = chars
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
charCard { chars, activeComponentId, unitSize, thumbnailUnitSize, boxUnits, borderUnits, strokeWidth, simpleCharSvgs, selectedChar } myChar =
    let
        char =
            charFromMyChar myChar
    in
    E.column
        (([ E.width <| E.px <| boxUnits * thumbnailUnitSize
          , Background.color palette.lightBg
          , Border.rounded spacing.medium
          , Events.onClick <| SelectChar myChar
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
            , E.paddingXY spacing.small spacing.small
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
                , chars = chars
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

        CompoundChar { char } _ ->
            char


isMyCharType : MyCharType -> MyChar -> Bool
isMyCharType myCharType myChar =
    case ( myCharType, myChar ) of
        ( SimpleCharType, SimpleChar _ ) ->
            True

        ( CompoundCharType, CompoundChar _ _ ) ->
            True

        _ ->
            False


renderChar :
    { isThumbnail : Bool
    , unitSize : Int
    , boxUnits : Int
    , borderUnits : Int
    , strokeWidth : Float
    , chars : Dict Char MyChar
    , simpleCharSvgs : SimpleCharSvgs
    , activeComponentId : Maybe Id
    }
    -> MyChar
    -> Svg Msg
renderChar { isThumbnail, unitSize, boxUnits, borderUnits, strokeWidth, chars, simpleCharSvgs, activeComponentId } myChar =
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
                , chars = chars
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
    , chars : Dict Char MyChar
    , simpleCharSvgs : SimpleCharSvgs
    , activeComponentId : Maybe Id
    , isThumbnail : Bool
    }
    -> Int
    -> MyChar
    -> Svg Msg
renderCharHelper { unitSize, boxUnits, chars, simpleCharSvgs, activeComponentId, isThumbnail } level myChar =
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

        CompoundChar { char, width, height, position } components ->
            constraint width height position <|
                List.map
                    (renderCharHelper
                        { unitSize = unitSize
                        , boxUnits = boxUnits
                        , chars = chars
                        , simpleCharSvgs = simpleCharSvgs
                        , activeComponentId = activeComponentId
                        , isThumbnail = isThumbnail
                        }
                        (level + 1)
                        << myCharFromMyCharRef chars
                    )
                    components


myCharFromMyCharRef : Dict Char MyChar -> MyCharRef -> MyChar
myCharFromMyCharRef chars ref =
    let
        myChar =
            -- impossible
            Maybe.withDefault emptyMyChar <|
                Dict.get ref.char chars

        updateAttributes r =
            { r
                | id = ref.id
                , width = ref.width
                , height = ref.height
                , position = ref.position
            }
    in
    case myChar of
        SimpleChar r ->
            SimpleChar (updateAttributes r)

        CompoundChar r components ->
            CompoundChar (updateAttributes r) components


getId : MyChar -> Id
getId myChar =
    case myChar of
        SimpleChar { id } ->
            id

        CompoundChar { id } _ ->
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
    Sub.batch
        [ Draggable.subscriptions DragMsg drag
        , getModelPort GotModel
        , pageUnloadingPort SaveModel
        , getSimpleCharsPort GetSimpleChars
        ]


decodeSimpleCharSvgs : Decode.Decoder SimpleCharSvgs
decodeSimpleCharSvgs =
    Decode.map
        (Dict.Extra.mapKeys charFromString)
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
