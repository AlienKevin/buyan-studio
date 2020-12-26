port module Main exposing (..)

import Array
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
import TypedSvg.Events
import TypedSvg.Types as SvgTypes


port addSimpleCharsPort : () -> Cmd msg


port getSimpleCharsPort : (Encode.Value -> msg) -> Sub msg


port saveModelPort : Value -> Cmd msg


port getModelPort : (Value -> msg) -> Sub msg


port deleteSimpleCharPort : String -> Cmd msg


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
    , strokeLineCap : StrokeLineCap
    , popUp : PopUp
    , newCompoundChar : String
    , isInputErrorShown : Bool
    , dragDropChar : DragDrop.Model Char ()
    , dragDropCharData : { char : Char }
    , drag : Draggable.State ( Id, Scale )
    , activeComponentId : Maybe Id
    , activeScale : Scale
    , isAspectRatioLocked : Bool
    , isSnapComponentToGrid : Bool
    }


type alias SavedModel =
    { chars : Dict Char MyChar
    , strokeWidth : Float
    }


type StrokeLineCap
    = StrokeLineCapRound
    | StrokeLineCapSquare


type alias Id =
    Int


type Scale
    = ScaleTopLeft
    | ScaleTopRight
    | ScaleBottomLeft
    | ScaleBottomRight
    | NoScale


type MyChar
    = SimpleChar MyCharRef
    | CompoundChar MyCharRef (List MyCharRef)


type alias MyCharRef =
    { char : Char
    , id : Id
    , dimension : Vec2
    , position : Vec2
    }


emptyMyChar : MyChar
emptyMyChar =
    SimpleChar
        { char = '?'
        , id = -1
        , dimension = Vector2.vec2 0 0
        , position = Vector2.vec2 0 0
        }


type MyCharType
    = SimpleCharType
    | CompoundCharType


type alias SimpleCharSvgs =
    Dict Char SimpleCharSvg


type alias SimpleCharSvg =
    ( Svg Msg, Maybe Vec2 )


type PopUp
    = AddCompoundCharPopUp
    | ConfirmDeleteSelectedCharPopUp
    | NoPopUp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { chars = Dict.empty
      , selectedChar = Nothing
      , simpleCharSvgs = Dict.empty
      , boxUnits = 34
      , borderUnits = 1
      , unitSize = 20
      , thumbnailUnitSize = 4
      , strokeWidth = 10
      , strokeLineCap = StrokeLineCapRound
      , popUp = NoPopUp
      , newCompoundChar = ""
      , isInputErrorShown = False
      , dragDropChar = DragDrop.init
      , dragDropCharData = { char = '?' }
      , drag = Draggable.init
      , activeComponentId = Nothing
      , activeScale = NoScale
      , isAspectRatioLocked = True
      , isSnapComponentToGrid = True
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddChar MyCharType
    | GetSimpleChars Value
    | SelectChar MyChar
    | RequestDeleteSelectedChar
    | DeleteSelectedChar
    | UpdatePendingCompoundChar String
    | AddPendingCompoundChar
    | ShowInputError
    | HideInputError
    | ClosePopUp
    | DragDropChar (DragDrop.Msg Char ())
    | OnDragBy Vec2
    | StartDragging ( Id, Scale )
    | StopDragging
    | DragMsg (Draggable.Msg ( Id, Scale ))
    | SetActiveComponentId Id
    | GotModel Value
    | SaveModel ()
    | UpdateStrokeWidth Float
    | UpdateStrokeLineCap StrokeLineCap
    | ToggleIsAspectRatioLocked


dragConfig : Draggable.Config ( Id, Scale ) Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vector2.vec2 dx dy |> OnDragBy)
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onClick (\( id, _ ) -> SetActiveComponentId id)
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

        RequestDeleteSelectedChar ->
            requestDeleteSelectedChar model

        DeleteSelectedChar ->
            deleteSelectedChar model

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

        StartDragging target ->
            startDragging target model

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

        UpdateStrokeWidth newStrokeWidth ->
            updateStrokeWidth newStrokeWidth model

        UpdateStrokeLineCap newStrokeLineCap ->
            updateStrokeLineCap newStrokeLineCap model

        ToggleIsAspectRatioLocked ->
            toggleIsAspectRatioLocked model


requestDeleteSelectedChar : Model -> ( Model, Cmd Msg )
requestDeleteSelectedChar model =
    ( { model
        | popUp =
            ConfirmDeleteSelectedCharPopUp
      }
    , Cmd.none
    )


deleteSelectedChar : Model -> ( Model, Cmd Msg )
deleteSelectedChar model =
    ( { model
        | selectedChar =
            Nothing
        , chars =
            case model.selectedChar of
                Just char ->
                    Dict.remove char model.chars

                Nothing ->
                    model.chars
        , popUp =
            NoPopUp
      }
    , case model.selectedChar of
        Just char ->
            deleteSimpleCharPort (String.fromChar char)

        Nothing ->
            Cmd.none
    )


updateStrokeLineCap : StrokeLineCap -> Model -> ( Model, Cmd Msg )
updateStrokeLineCap newStrokeLineCap model =
    ( { model
        | strokeLineCap =
            newStrokeLineCap
      }
    , Cmd.none
    )


toggleIsAspectRatioLocked : Model -> ( Model, Cmd Msg )
toggleIsAspectRatioLocked model =
    ( { model
        | isAspectRatioLocked =
            not model.isAspectRatioLocked
      }
    , Cmd.none
    )


updateStrokeWidth : Float -> Model -> ( Model, Cmd Msg )
updateStrokeWidth newStrokeWidth model =
    ( { model
        | strokeWidth =
            newStrokeWidth
      }
    , Cmd.none
    )


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
encodeMyCharRef { char, id, dimension, position } =
    Encode.object
        [ ( "char", encodeChar char )
        , ( "id", encodeId id )
        , ( "dimension", encodeVec2 dimension )
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
            -- let
            -- _ =
            --     Debug.log "err" err
            -- in
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
    Decode.map4 MyCharRef
        (Decode.field "char" decodeChar)
        (Decode.field "id" decodeId)
        (Decode.field "dimension" decodeVec2)
        (Decode.field "position" decodeVec2)


decodeVec2 : Decoder Vec2
decodeVec2 =
    Decode.map2 Vector2.vec2
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


decodeId : Decoder Id
decodeId =
    Decode.int


dragMsg : Draggable.Msg ( Id, Scale ) -> Model -> ( Model, Cmd Msg )
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


startDragging : ( Id, Scale ) -> Model -> ( Model, Cmd Msg )
startDragging ( id, scale ) model =
    ( { model
        | activeComponentId =
            Just id
        , activeScale =
            scale
      }
    , Cmd.none
    )


type OffsetType
    = DimensionOffset
    | PositionOffset


onDragBy : Vec2 -> Model -> ( Model, Cmd Msg )
onDragBy delta ({ activeComponentId, activeScale, boxUnits, borderUnits, unitSize, chars, isAspectRatioLocked } as model) =
    ( { model
        | chars =
            case model.selectedChar of
                Just selectedChar ->
                    Dict.update
                        selectedChar
                        (Maybe.map
                            (updateMyCharComponent <|
                                let
                                    factor =
                                        100 / toFloat (boxUnits * unitSize)

                                    deltaX =
                                        Vector2.getX delta

                                    deltaY =
                                        Vector2.getY delta

                                    offsetDimension xDir yDir =
                                        offset DimensionOffset xDir yDir

                                    offsetPosition xDir yDir =
                                        offset PositionOffset xDir yDir

                                    offset offsetType xDir yDir =
                                        Vector2.scale factor <|
                                            if isAspectRatioLocked then
                                                let
                                                    averagedDelta =
                                                        (abs deltaX + abs deltaY) / 2

                                                    averagedDeltaX =
                                                        sign deltaX * averagedDelta

                                                    averagedDeltaY =
                                                        sign
                                                            (case offsetType of
                                                                DimensionOffset ->
                                                                    deltaX

                                                                PositionOffset ->
                                                                    deltaY
                                                            )
                                                            * averagedDelta
                                                in
                                                Vector2.vec2 (xDir * averagedDeltaX)
                                                    ((case offsetType of
                                                        DimensionOffset ->
                                                            xDir

                                                        PositionOffset ->
                                                            yDir
                                                     )
                                                        * averagedDeltaY
                                                    )

                                            else
                                                -- let
                                                --     _ = Debug.log "deltaX" <| xDir * deltaX * factor
                                                --     _ = Debug.log "deltaY" <| yDir * deltaY * factor
                                                -- in
                                                Vector2.vec2 (xDir * deltaX) (yDir * deltaY)
                                in
                                List.Extra.updateAt
                                    -- impossible
                                    (Maybe.withDefault -1 activeComponentId)
                                    (case activeScale of
                                        NoScale ->
                                            updateMyCharRefPosition
                                                (Vector2.add <| Vector2.scale factor delta)

                                        ScaleTopLeft ->
                                            updateMyCharRefDimension (Vector2.add (offsetDimension -1 -1))
                                                << updateMyCharRefPosition (Vector2.add (offsetPosition 1 1))

                                        ScaleTopRight ->
                                            updateMyCharRefDimension (Vector2.add (offsetDimension 1 -1))
                                                << updateMyCharRefPosition (Vector2.add (offsetPosition 0 1))

                                        ScaleBottomLeft ->
                                            updateMyCharRefDimension (Vector2.add (offsetDimension -1 1))
                                                << updateMyCharRefPosition (Vector2.add (offsetPosition 1 0))

                                        ScaleBottomRight ->
                                            updateMyCharRefDimension (Vector2.add (offsetDimension 1 1))
                                                << updateMyCharRefPosition (Vector2.add (offsetPosition 0 0))
                                    )
                            )
                        )
                        chars

                Nothing ->
                    chars
      }
    , Cmd.none
    )


sign : number -> number
sign n =
    if n < 0 then
        -1

    else
        1


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
        -- impossible
        SimpleChar _ ->
            myChar

        CompoundChar compoundChar components ->
            -- Prevent infinite recursive self-reference
            if componentChar == compoundChar.char then
                myChar

            else
                let
                    dimension =
                        case Dict.get componentChar chars of
                            Just component ->
                                Vector2.scale 0.5 <| .dimension (calculateMyCharDimension component)

                            -- impossible
                            Nothing ->
                                Vector2.vec2 50 50

                    position =
                        calculateCenterPosition dimension

                    id =
                        List.length components

                    newComponent =
                        { char = componentChar
                        , id = id
                        , dimension = dimension
                        , position = position
                        }
                in
                CompoundChar
                    compoundChar
                    (components ++ [ newComponent ])


calculateCenterPosition : Vec2 -> Vec2
calculateCenterPosition dimension =
    Vector2.scale 0.5 <|
        Vector2.sub (Vector2.vec2 100 100) dimension


calculateMyCharDimension : MyChar -> { position : Vec2, dimension : Vec2 }
calculateMyCharDimension myChar =
    case myChar of
        SimpleChar { dimension } ->
            { position = Vector2.vec2 0 0
            , dimension = dimension
            }

        CompoundChar _ components ->
            (\{ minX, minY, maxX, maxY } ->
                -- let
                --     _ =
                --         Debug.log "minX" minX
                --     _ =
                --         Debug.log "maxX" maxX
                --     _ =
                --         Debug.log "minY" minY
                --     _ =
                --         Debug.log "maxY" maxY
                -- in
                { position =
                    Vector2.vec2 minX minY
                , dimension =
                    Vector2.vec2 (maxX - minX) (maxY - minY)
                }
            )
            <|
                List.foldl
                    (\current extreme ->
                        { minX =
                            min current.minX extreme.minX
                        , minY =
                            min current.minY extreme.minY
                        , maxX =
                            max current.maxX extreme.maxX
                        , maxY =
                            max current.maxY extreme.maxY
                        }
                    )
                    { minX = 100
                    , minY = 100
                    , maxX = 0
                    , maxY = 0
                    }
                <|
                    List.map
                        (\{ char, position, dimension } ->
                            let
                                -- _ =
                                --     Debug.log "char" char
                                -- _ =
                                --     Debug.log "topLeftPoint" topLeftPoint
                                -- _ =
                                --     Debug.log "bottomRightPoint" bottomRightPoint
                                topLeftPoint =
                                    position

                                bottomRightPoint =
                                    Vector2.add position dimension
                            in
                            { minX = Vector2.getX topLeftPoint
                            , minY = Vector2.getY topLeftPoint
                            , maxX = Vector2.getX bottomRightPoint
                            , maxY = Vector2.getY bottomRightPoint
                            }
                        )
                        components


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
                , dimension = Vector2.vec2 100 100
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
                        (\char ( _, maybeDimension ) ->
                            let
                                fullDimension =
                                    Vector2.vec2 100 100

                                dimension =
                                    case maybeDimension of
                                        Just d ->
                                            let
                                                width =
                                                    Vector2.getX d

                                                height =
                                                    Vector2.getY d

                                                -- _ =
                                                --     Debug.log "width" width
                                                -- _ =
                                                --     Debug.log "height" height
                                                f =
                                                    lerp 0 (max width height) 0 100
                                            in
                                            Vector2.vec2 (f width) (f height)

                                        Nothing ->
                                            fullDimension

                                position =
                                    calculateCenterPosition dimension
                            in
                            Dict.insert
                                char
                                (SimpleChar
                                    { char = char
                                    , id = -1
                                    , dimension = dimension
                                    , position = position
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
            -- let
            --     _ =
            --         Debug.log "err" err
            -- in
            model
    , Cmd.none
    )


lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp inMin inMax outMin outMax n =
    (n - inMin) / (inMax - inMin) * (outMax - outMin) + outMin


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


updateMyCharComponent : (List MyCharRef -> List MyCharRef) -> MyChar -> MyChar
updateMyCharComponent func myChar =
    case myChar of
        SimpleChar _ ->
            myChar

        CompoundChar c components ->
            CompoundChar c (func components)


updateMyCharRefPosition : (Vec2 -> Vec2) -> MyCharRef -> MyCharRef
updateMyCharRefPosition func myCharRef =
    { myCharRef
        | position =
            func myCharRef.position
    }


snapToGrid : Int -> Int -> Vec2 -> Vec2
snapToGrid boxUnits borderUnits position =
    let
        unitPercent =
            100 / toFloat (boxUnits - borderUnits * 2)

        roundToGrid pos =
            (*) unitPercent <| toFloat <| round <| pos / unitPercent
    in
    Vector2.vec2
        (roundToGrid <| Vector2.getX position)
        (roundToGrid <| Vector2.getY position)


updateMyCharDimension : (Vec2 -> Vec2) -> MyChar -> MyChar
updateMyCharDimension func myChar =
    case myChar of
        SimpleChar c ->
            SimpleChar
                { c
                    | dimension =
                        func c.dimension
                }

        CompoundChar c components ->
            CompoundChar
                { c
                    | dimension =
                        func c.dimension
                }
                components


updateMyCharRefDimension : (Vec2 -> Vec2) -> MyCharRef -> MyCharRef
updateMyCharRefDimension func myCharRef =
    { myCharRef
        | dimension =
            func myCharRef.dimension
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
            , E.column
                [ E.spacing spacing.medium ]
                [ editor model
                , preferences model
                ]
            ]


popUp : Model -> E.Element Msg
popUp model =
    case model.popUp of
        AddCompoundCharPopUp ->
            addCompoundCharPopUp model

        ConfirmDeleteSelectedCharPopUp ->
            confirmDeleteSelectedCharPopUp model

        NoPopUp ->
            E.none


confirmDeleteSelectedCharPopUp : Model -> E.Element Msg
confirmDeleteSelectedCharPopUp { activeComponentId, boxUnits, thumbnailUnitSize, selectedChar } =
    let
        borderWidth =
            6
    in
    E.column
        ([ E.centerX
         , E.centerY
         , Background.color palette.lightBg
         , E.width <| E.px <| boxUnits * thumbnailUnitSize + 3 * borderWidth
         , E.height <| E.px <| boxUnits * thumbnailUnitSize + fontSize.title + 3 * borderWidth
         , E.spaceEvenly
         , E.padding spacing.small
         , Font.size fontSize.medium
         ]
            ++ highlightBorder
                { color = palette.danger
                , width = borderWidth
                , style = Border.dashed
                , glowWidth = borderWidth
                }
        )
        [ E.el
            [ E.centerX ]
            (E.paragraph []
                [ E.text "Do you want to delete "
                , E.el [ Font.bold, Font.size fontSize.large ] (E.text (String.fromChar (Maybe.withDefault '?' selectedChar)))
                , E.text " ?\n"
                , E.el [ Font.size fontSize.small ] (E.text "This can't be undone.")
                ]
            )
        , E.row
            [ E.width E.fill
            ]
            [ E.el
                [ E.alignLeft
                , Font.color palette.lightFg
                ]
                (iconButton
                    { icon =
                        FeatherIcons.xCircle
                    , size =
                        fontSize.large
                    , onPress =
                        Just ClosePopUp
                    }
                )
            , E.el
                [ E.alignRight
                , Font.color palette.danger
                ]
                (iconButton
                    { icon =
                        FeatherIcons.trash2
                    , size =
                        fontSize.large
                    , onPress =
                        Just DeleteSelectedChar
                    }
                )
            ]
        ]


addCompoundCharPopUp : Model -> E.Element Msg
addCompoundCharPopUp { activeComponentId, boxUnits, thumbnailUnitSize, newCompoundChar, isInputErrorShown } =
    let
        inputLength =
            String.length newCompoundChar

        isValidNewChar =
            inputLength == 1

        borderWidth =
            6
    in
    E.column
        ([ E.centerX
         , E.centerY
         , Background.color palette.lightBg
         , E.width <| E.px <| boxUnits * thumbnailUnitSize + 3 * borderWidth
         , E.height <| E.px <| boxUnits * thumbnailUnitSize + fontSize.title + 3 * borderWidth
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
                        fontSize.small
                    , onPress =
                        Just ClosePopUp
                    }
         ]
            ++ highlightBorder
                { color = palette.lightFg
                , width = borderWidth
                , style = Border.dashed
                , glowWidth = borderWidth
                }
        )
        [ Input.text
            [ E.width <| E.px <| fontSize.medium * 5
            , E.centerX
            , onEnter <|
                if isValidNewChar then
                    Just AddPendingCompoundChar

                else
                    Nothing
            ]
            { onChange =
                UpdatePendingCompoundChar
            , text =
                newCompoundChar
            , placeholder =
                Nothing
            , label =
                Input.labelAbove
                    [ E.paddingEach { top = spacing.medium, bottom = 0, left = 0, right = 0 } ]
                    (E.text "Character")
            }
        , E.el
            ([ E.centerX
             , E.below <|
                if isInputErrorShown then
                    E.el
                        [ E.centerX
                        , Font.size fontSize.small
                        ]
                    <|
                        E.text "Accept only\n 1 character"

                else
                    E.none
             ]
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
        ]


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


preferences : Model -> E.Element Msg
preferences model =
    E.column
        [ E.spacing spacing.small ]
        [ E.el [ Font.size fontSize.title ] <|
            E.text "Preferences"
        , Input.slider
            [ E.height (E.px fontSize.small)
            , E.width (E.px <| fontSize.small * 7)
            , E.behindContent
                (E.el
                    [ E.width E.fill
                    , E.height (E.px <| fontSize.small // 3)
                    , E.centerY
                    , Background.color palette.darkFg
                    , Border.rounded (fontSize.small // 3)
                    ]
                    E.none
                )
            ]
            { onChange = UpdateStrokeWidth
            , label =
                Input.labelLeft []
                    (E.text <| "Stroke width is " ++ String.fromInt (round model.strokeWidth))
            , min = 10
            , max = 70
            , step = Just 1
            , value = model.strokeWidth
            , thumb = Input.defaultThumb
            }
        , Input.radio
            [ E.spacing spacing.tiny
            , E.padding spacing.small
            ]
            { onChange = UpdateStrokeLineCap
            , selected = Just model.strokeLineCap
            , label = Input.labelLeft [] (E.text "Stroke linecap is ")
            , options =
                [ Input.optionWith StrokeLineCapRound
                    (radioOption (E.text "Round"))
                , Input.optionWith StrokeLineCapSquare
                    (radioOption (E.text "Square"))
                ]
            }
        ]


radioOption : E.Element msg -> Input.OptionState -> E.Element msg
radioOption optionLabel status =
    E.row
        [ E.spacing 10
        , E.alignLeft
        , E.width E.shrink
        ]
        [ E.el
            [ E.width (E.px 14)
            , E.height (E.px 14)
            , Border.rounded 7
            , Border.width <|
                case status of
                    Input.Idle ->
                        1

                    Input.Focused ->
                        1

                    Input.Selected ->
                        5
            , Border.color <|
                case status of
                    Input.Idle ->
                        palette.darkFg

                    Input.Focused ->
                        palette.darkFg

                    Input.Selected ->
                        palette.darkFg
            ]
            E.none
        , E.el [ E.width E.fill ] optionLabel
        ]


editor : Model -> E.Element Msg
editor ({ activeComponentId, selectedChar, chars, simpleCharSvgs, boxUnits, unitSize, borderUnits, strokeWidth, strokeLineCap, isAspectRatioLocked, isSnapComponentToGrid } as model) =
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
                            , strokeLineCap = strokeLineCap
                            , isThumbnail = False
                            , isAspectRatioLocked = isAspectRatioLocked
                            , isSnapComponentToGrid = isSnapComponentToGrid
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
        , E.height E.fill
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
            , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
            , E.htmlAttribute <| Html.Attributes.style "overflow-x" "hidden"
            , E.spacing spacing.medium
            ]
          <|
            cards
        ]


charCard : Model -> MyChar -> E.Element Msg
charCard { chars, activeComponentId, unitSize, thumbnailUnitSize, boxUnits, borderUnits, strokeWidth, strokeLineCap, simpleCharSvgs, selectedChar, isAspectRatioLocked, isSnapComponentToGrid } myChar =
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
                            highlightBorder
                                { color = palette.lightFg
                                , width = 0
                                , style = Border.solid
                                , glowWidth = spacing.small
                                }

                        else
                            []

                    Nothing ->
                        []
               )
        )
        [ E.row
            [ Font.size fontSize.large
            , Font.bold
            , E.paddingXY spacing.small spacing.small
            , E.width E.fill
            ]
            [ E.el
                [ E.alignLeft ]
                (E.text <|
                    String.fromChar <|
                        char
                )
            , if selectedChar == Just char then
                E.el
                    [ E.alignRight
                    , Font.color palette.danger
                    ]
                    (iconButton
                        { icon =
                            FeatherIcons.trash2
                        , size =
                            fontSize.large
                        , onPress =
                            Just RequestDeleteSelectedChar
                        }
                    )

              else
                E.none
            ]
        , E.html <|
            renderChar
                { unitSize = thumbnailUnitSize
                , boxUnits = boxUnits
                , borderUnits = borderUnits
                , strokeWidth = strokeWidth * toFloat thumbnailUnitSize / toFloat unitSize
                , strokeLineCap = strokeLineCap
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
                , isAspectRatioLocked = isAspectRatioLocked
                , isSnapComponentToGrid = isSnapComponentToGrid
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
    , strokeLineCap : StrokeLineCap
    , chars : Dict Char MyChar
    , simpleCharSvgs : SimpleCharSvgs
    , activeComponentId : Maybe Id
    , isAspectRatioLocked : Bool
    , isSnapComponentToGrid : Bool
    }
    -> MyChar
    -> Svg Msg
renderChar { isThumbnail, unitSize, boxUnits, borderUnits, strokeWidth, strokeLineCap, chars, simpleCharSvgs, activeComponentId, isAspectRatioLocked, isSnapComponentToGrid } myChar =
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
                        stroke-linecap: """
                        ++ (case strokeLineCap of
                                StrokeLineCapRound ->
                                    "round"

                                StrokeLineCapSquare ->
                                    "square"
                           )
                        ++ """!important;
                        stroke-miterlimit: 10;
                        stroke-width: """
                        ++ String.fromFloat strokeWidth
                        ++ """ !important;
                        stroke-linejoin: round !important;
                        vector-effect: non-scaling-stroke;
                    }
                    #active-component-border {
                        stroke-width: 2px !important;
                        stroke: """
                        ++ (Color.toCssString <| toColor palette.darkFg)
                        ++ """;
                    }
                    #aspect-ratio-lock * {
                        stroke-width: 2px !important;
                        stroke: """
                        ++ (Color.toCssString <| Color.white)
                        ++ """;
                    }
                    .scale-handle, #aspect-ratio-lock-bg {
                        fill: """
                        ++ (Color.toCssString <| toColor palette.darkFg)
                        ++ """;
                        stroke: none;
                    }
                    svg {
                        overflow: visible;
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
                , isAspectRatioLocked = isAspectRatioLocked
                , isSnapComponentToGrid =
                    isSnapComponentToGrid
                        && (case myChar of
                                CompoundChar _ _ ->
                                    True

                                SimpleChar _ ->
                                    False
                           )
                , tightDimension =
                    { position = Vector2.vec2 0 0, dimension = Vector2.vec2 100 100 }
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
    , isAspectRatioLocked : Bool
    , isSnapComponentToGrid : Bool
    , tightDimension : { position : Vec2, dimension : Vec2 }
    }
    -> Int
    -> MyChar
    -> Svg Msg
renderCharHelper { unitSize, boxUnits, chars, simpleCharSvgs, activeComponentId, isThumbnail, isAspectRatioLocked, isSnapComponentToGrid, tightDimension } level myChar =
    let
        id =
            getId myChar

        levelwiseId =
            id + (level - 1) * 10

        isDraggable =
            not (isThumbnail || level > 1)

        constraint dimension position contents =
            let
                tightPosition =
                    (if isSnapComponentToGrid then
                        snapToGrid boxUnits 1

                     else
                        identity
                    )
                    <|
                        Vector2.sub position tightDimension.position

                snappedTightDimension =
                    (if isSnapComponentToGrid then
                        snapToGrid boxUnits 1

                     else
                        identity
                    )
                    <|
                        Vector2.vec2
                            (100 / Vector2.getX tightDimension.dimension * Vector2.getX dimension)
                            (100 / Vector2.getY tightDimension.dimension * Vector2.getY dimension)
            in
            Svg.svg
                ([ SvgAttributes.x <| SvgTypes.Percent <| 100 / Vector2.getX tightDimension.dimension * Vector2.getX tightPosition
                 , SvgAttributes.y <| SvgTypes.Percent <| 100 / Vector2.getY tightDimension.dimension * Vector2.getY tightPosition
                 , SvgAttributes.width <| SvgTypes.Percent <| Vector2.getX snappedTightDimension
                 , SvgAttributes.height <| SvgTypes.Percent <| Vector2.getY snappedTightDimension
                 ]
                    ++ dragTrigger isDraggable ( levelwiseId, NoScale )
                )
            <|
                if Just levelwiseId == activeComponentId then
                    contents
                        ++ [ Svg.rect
                                [ SvgAttributes.id "active-component-border"
                                , SvgAttributes.width <| SvgTypes.Percent 100
                                , SvgAttributes.height <| SvgTypes.Percent 100
                                , SvgAttributes.fill <| SvgTypes.PaintNone
                                , SvgAttributes.stroke <| SvgTypes.Paint <| toColor palette.lightFg
                                ]
                                []
                           , scaleAspectRatioLock isAspectRatioLocked
                           , scaleHandle ( levelwiseId, ScaleTopLeft ) 0 0 unitSize isDraggable
                           , scaleHandle ( levelwiseId, ScaleTopRight ) 100 0 unitSize isDraggable
                           , scaleHandle ( levelwiseId, ScaleBottomLeft ) 0 100 unitSize isDraggable
                           , scaleHandle ( levelwiseId, ScaleBottomRight ) 100 100 unitSize isDraggable
                           ]

                else
                    contents
    in
    case myChar of
        SimpleChar { char, dimension, position } ->
            case Dict.get char simpleCharSvgs of
                Just ( svg, _ ) ->
                    constraint dimension position [ svg ]

                Nothing ->
                    -- impossible
                    TypedSvg.Core.text <| "Error rendering " ++ String.fromChar char

        CompoundChar ({ char, dimension, position } as compoundChar) components ->
            constraint dimension position <|
                List.map
                    (renderCharHelper
                        { unitSize = unitSize
                        , boxUnits = boxUnits
                        , chars = chars
                        , simpleCharSvgs = simpleCharSvgs
                        , activeComponentId = activeComponentId
                        , isThumbnail = isThumbnail
                        , isAspectRatioLocked = isAspectRatioLocked
                        , isSnapComponentToGrid = isSnapComponentToGrid
                        , tightDimension =
                            if level >= 1 then
                                calculateMyCharDimension myChar

                            else
                                { position = Vector2.vec2 0 0, dimension = Vector2.vec2 100 100 }
                        }
                        (level + 1)
                        << myCharFromMyCharRef chars
                    )
                    components


scaleAspectRatioLock : Bool -> Svg Msg
scaleAspectRatioLock isAspectRatioLocked =
    Svg.svg
        [ SvgAttributes.id "aspect-ratio-lock"
        , SvgAttributes.x <| SvgTypes.px -35
        , SvgAttributes.y <| SvgTypes.percent 50
        , SvgAttributes.width <| SvgTypes.px fontSize.large
        , SvgAttributes.height <| SvgTypes.px fontSize.large
        ]
        [ Svg.rect
            [ SvgAttributes.width <| SvgTypes.percent 100
            , SvgAttributes.height <| SvgTypes.percent 100
            , SvgAttributes.rx <| SvgTypes.percent 20
            , SvgAttributes.ry <| SvgTypes.percent 20
            , SvgAttributes.id "aspect-ratio-lock-bg"
            , TypedSvg.Events.onClick ToggleIsAspectRatioLocked
            ]
            []
        , (if isAspectRatioLocked then
            FeatherIcons.lock

           else
            FeatherIcons.unlock
          )
            |> FeatherIcons.withSize 100
            |> FeatherIcons.withSizeUnit "%"
            |> FeatherIcons.toHtml []
        ]


scaleHandle : ( Id, Scale ) -> Float -> Float -> Int -> Bool -> Svg Msg
scaleHandle ( id, scale ) x y size isDraggable =
    Svg.circle
        ([ SvgAttributes.cx (SvgTypes.percent x)
         , SvgAttributes.cy (SvgTypes.percent y)
         , SvgAttributes.r (SvgTypes.px <| toFloat size / 2)
         , SvgAttributes.class [ "scale-handle" ]
         ]
            ++ dragTrigger isDraggable ( id, scale )
        )
        []


dragTrigger : Bool -> ( Id, Scale ) -> List (Html.Attribute Msg)
dragTrigger isDraggable ( id, scale ) =
    if isDraggable then
        [ Draggable.mouseTrigger ( id, scale ) DragMsg ]

    else
        []


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
                , dimension = ref.dimension
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


highlightBorder :
    { color : E.Color
    , width : Int
    , glowWidth : Float
    , style : E.Attribute Msg
    }
    -> List (E.Attribute Msg)
highlightBorder { color, width, glowWidth, style } =
    [ Border.rounded spacing.medium
    , Border.color color
    , Border.width width
    , style
    , Border.glow color glowWidth
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


decodeSimpleCharSvg : Decode.Decoder SimpleCharSvg
decodeSimpleCharSvg =
    Decode.map
        (SvgParser.parseToNode
            >> Result.withDefault
                (SvgParser.SvgElement
                    { name = "g"
                    , attributes = []
                    , children = []
                    }
                )
            >> (\node ->
                    case node of
                        SvgParser.SvgElement element ->
                            let
                                attributes =
                                    Dict.fromList element.attributes
                            in
                            ( SvgParser.SvgElement
                                { element
                                    | attributes =
                                        element.attributes
                                            ++ [ ( "preserveAspectRatio", "none" ) ]
                                }
                            , Maybe.map
                                (\( width, height ) -> Vector2.vec2 width height)
                                (Maybe.andThen
                                    (String.words
                                        >> Array.fromList
                                        >> (\arr ->
                                                Maybe.map2 Tuple.pair
                                                    (Maybe.andThen String.toFloat <| Array.get 2 arr)
                                                    (Maybe.andThen String.toFloat <| Array.get 3 arr)
                                           )
                                    )
                                    (Dict.get "viewBox" attributes)
                                )
                            )

                        _ ->
                            ( node, Nothing )
               )
            >> Tuple.mapFirst SvgParser.nodeToSvg
        )
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
    , danger =
        E.rgb255 210 99 71
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
