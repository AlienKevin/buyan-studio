port module Main exposing (..)

import Array
import Browser
import Browser.Events
import Color
import Color.Manipulate
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
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Touch
import Http
import I18Next
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Math.Vector2 as Vector2 exposing (Vec2)
import String.Extra
import SvgParser
import Task
import Time
import Translations
import Translations.CharType
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types as SvgTypes


port addSimpleCharsPort : () -> Cmd msg


port gotSavedSimpleCharsPort : (Encode.Value -> msg) -> Sub msg


port gotNewSimpleCharsPort : (Encode.Value -> msg) -> Sub msg


port downloadCharPort : String -> Cmd msg


port saveModelPort : Value -> Cmd msg


port getModelPort : (Value -> msg) -> Sub msg


port deleteSimpleCharPort : String -> Cmd msg


port clearSimpleCharsPort : () -> Cmd msg



---- MODEL ----


type alias Model =
    { mode : Mode
    , chars : Dict Char MyChar
    , charExplainations : Dict Char Explaination
    , selectedChar : Maybe Char
    , simpleCharSvgs : SimpleCharSvgs
    , boxUnits : Int
    , borderUnits : Float
    , unitSize : Float
    , thumbnailUnitSize : Float
    , strokeWidth : Float
    , popUp : PopUp
    , newCompoundChar : String
    , newComponentChar : String
    , inputError : Maybe InputError
    , drag : Draggable.State DragData
    , dragDelta : Vec2
    , activeComponentIndex : Maybe Int
    , activeScale : Scale
    , isAspectRatioLocked : Bool
    , isSnapToGrid : Bool
    , isReferenceCharShown : Bool
    , paragraphForPreview : String
    , trs : I18Next.Translations
    , language : Language
    , device : E.Device
    , palette :
        Palette
    , spacing :
        Spacing
    , fontSize :
        FontSize
    }


type alias Explaination =
    { note : String
    , referenceImage :
        { image : DataUrl
        , caption : String
        , url : String
        }
    }


type alias DataUrl =
    String


emptyExplaination : Explaination
emptyExplaination =
    { note = ""
    , referenceImage =
        { image = ""
        , caption = ""
        , url = ""
        }
    }


type Mode
    = BrowseMode
    | EditMode


type InputError
    = InvalidInputLength Int
    | CharacterNotFound
    | ContainsSelfReference


type alias Palette =
    { lightBg : E.Color
    , lightFg : E.Color
    , darkFg : E.Color
    , danger : E.Color
    , black : E.Color
    , white : E.Color
    }


type alias Spacing =
    { tiny : Int
    , small : Int
    , medium : Int
    , large : Int
    }


type alias FontSize =
    { small : Int
    , medium : Int
    , large : Int
    , title : Int
    , thumb : Int
    }


type alias SavedModel =
    { chars : Dict Char MyChar
    , strokeWidth : Float
    , language : Language
    }


type Language
    = LanguageEn
    | LanguageZhHans
    | LanguageZhHant


type Scale
    = ScaleTopLeft
    | ScaleTopRight
    | ScaleBottomLeft
    | ScaleBottomRight
    | ScaleLeft
    | ScaleRight
    | ScaleTop
    | ScaleBottom
    | NoScale


type MyChar
    = SimpleChar MyCharRef
    | CompoundChar MyCharRef (List MyCharRef)


type alias MyCharRef =
    { char : Char
    , dimension : Vec2
    , position : Vec2
    }


emptyMyChar : MyChar
emptyMyChar =
    SimpleChar
        { char = '?'
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
    | ConfirmClearCharsPopUp MyCharType
    | PreviewInParagraphPopUp
    | AppPreferencesPopUp
    | AddComponentToSelectedCharPopUp
    | NoPopUp


minStrokeWidth : Float
minStrokeWidth =
    10


maxStrokeWidth : Float
maxStrokeWidth =
    70


minBorderUnits : Float
minBorderUnits =
    2


maxBorderUnits : Float
maxBorderUnits =
    3.5


minComponentSize : Float
minComponentSize =
    25


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { mode = BrowseMode
            , chars = Dict.empty
            , charExplainations = Dict.empty
            , selectedChar = Nothing
            , simpleCharSvgs = Dict.empty
            , boxUnits = 32
            , borderUnits = 3
            , unitSize = 18
            , thumbnailUnitSize = 4
            , strokeWidth = 10
            , popUp = NoPopUp
            , newCompoundChar = ""
            , newComponentChar = ""
            , inputError = Nothing
            , drag = Draggable.init
            , dragDelta = Vector2.vec2 0 0
            , activeComponentIndex = Nothing
            , activeScale = NoScale
            , isAspectRatioLocked = False
            , isSnapToGrid = True
            , isReferenceCharShown = True
            , paragraphForPreview = ""
            , trs = I18Next.initialTranslations
            , language = LanguageEn
            , device =
                { class = E.Desktop
                , orientation = E.Landscape
                }
            , palette =
                { lightBg =
                    E.rgb255 246 234 190
                , lightFg =
                    toElmUiColor Color.lightBlue
                , darkFg =
                    toElmUiColor Color.lightPurple
                , danger =
                    E.rgb255 210 99 71
                , black =
                    toElmUiColor Color.black
                , white =
                    toElmUiColor Color.white
                }
            , spacing =
                { tiny = 5
                , small = 10
                , medium = 20
                , large = 30
                }
            , fontSize =
                { small =
                    16
                , medium =
                    20
                , large =
                    30
                , title =
                    40
                , thumb =
                    30
                }
            }
    in
    case
        Decode.decodeValue
            (Decode.map4
                (\language translations windowWidth windowHeight ->
                    { language = language
                    , translations = translations
                    , windowWidth = windowWidth
                    , windowHeight = windowHeight
                    }
                )
                (Decode.field "language" decodeLanguage)
                (Decode.field "translations" I18Next.translationsDecoder)
                (Decode.field "windowWidth" Decode.int)
                (Decode.field "windowHeight" Decode.int)
            )
            flags
    of
        Ok { language, translations, windowWidth, windowHeight } ->
            updateDevice windowWidth windowHeight <|
                { model
                    | language =
                        language
                    , trs =
                        translations
                }

        Err err ->
            ( model, Cmd.none )



---- UPDATE ----


type Msg
    = AddChar MyCharType
    | GotNewSimpleChars Value
    | GotSavedSimpleChars Value
    | SelectChar MyChar
    | RequestAddComponentToSelectedChar
    | UpdatePendingComponentChar String
    | AddPendingComponentChar
    | RequestDeleteSelectedChar
    | DeleteSelectedChar
    | RequestClearChars MyCharType
    | ClearChars MyCharType
    | UpdatePendingCompoundChar String
    | AddPendingCompoundChar
    | ShowInputError InputError
    | HideInputError
    | ClosePopUp
    | OnDragBy Vec2
    | StartDragging DragData
    | StopDragging
    | DragMsg (Draggable.Msg DragData)
    | SetActiveComponent (Maybe Int)
    | CopyActiveComponent
    | DeleteActiveComponent
    | GotModel Value
    | SaveModel ()
    | UpdateStrokeWidth Float
    | ToggleIsAspectRatioLocked
    | PreviewInParagraph
    | UpdateParagraphForPreview String
    | ToggleIsSnapToGrid
    | ToggleIsReferenceCharShown
    | DownloadSelectedChar
    | UpdateLanguage Language
    | GotTranslations (Result Http.Error I18Next.Translations)
    | UpdateDevice Int Int
    | UpdateMode Mode
    | ShowAppPreferences
    | UpdateExplainationNote String
    | UploadReferenceImage
    | SelectedReferenceImage File
    | LoadedReferenceImage DataUrl


type alias DragData =
    { index : Int
    , scale : Scale
    }


dragConfig : Draggable.Config DragData Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vector2.vec2 dx dy |> OnDragBy)
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onClick (\{ index } -> SetActiveComponent (Just index))
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boxUnits, borderUnits, unitSize, chars, activeComponentIndex } as model) =
    case msg of
        AddChar myCharType ->
            addChar myCharType model

        GotNewSimpleChars svgsJson ->
            gotNewSimpleChars svgsJson model

        GotSavedSimpleChars svgsJson ->
            gotSavedSimpleChars svgsJson model

        SelectChar myChar ->
            selectChar myChar model

        RequestAddComponentToSelectedChar ->
            requestAddComponentToSelectedChar model

        UpdatePendingComponentChar charInput ->
            updatePendingComponentChar charInput model

        AddPendingComponentChar ->
            addPendingComponentChar model

        RequestDeleteSelectedChar ->
            requestDeleteSelectedChar model

        DeleteSelectedChar ->
            deleteSelectedChar model

        RequestClearChars myCharType ->
            requestClearChars myCharType model

        ClearChars myCharType ->
            clearChars myCharType model

        UpdatePendingCompoundChar charInput ->
            updatePendingCompoundChar charInput model

        AddPendingCompoundChar ->
            addPendingCompoundChar model

        ShowInputError error ->
            showInputError error model

        HideInputError ->
            hideInputError model

        ClosePopUp ->
            closePopUp model

        OnDragBy delta ->
            onDragBy delta model

        StartDragging target ->
            startDragging target model

        StopDragging ->
            stopDragging model

        SetActiveComponent index ->
            setActiveComponent index model

        CopyActiveComponent ->
            copyActiveComponent model

        DeleteActiveComponent ->
            deleteActiveComponent model

        DragMsg msg_ ->
            dragMsg msg_ model

        GotModel savedModelJson ->
            gotModel savedModelJson model

        SaveModel _ ->
            saveModel model

        UpdateStrokeWidth newStrokeWidth ->
            updateStrokeWidth newStrokeWidth model

        ToggleIsAspectRatioLocked ->
            toggleIsAspectRatioLocked model

        PreviewInParagraph ->
            previewInParagraph model

        UpdateParagraphForPreview paragraph ->
            updateParagraphForPreview paragraph model

        ToggleIsSnapToGrid ->
            toggleIsSnapToGrid model

        ToggleIsReferenceCharShown ->
            toggleIsReferenceCharShown model

        DownloadSelectedChar ->
            downloadSelectedChar model

        UpdateLanguage language ->
            updateLanguage language model

        GotTranslations trs ->
            gotTranslations trs model

        UpdateDevice width height ->
            updateDevice width height model

        UpdateMode mode ->
            updateMode mode model

        ShowAppPreferences ->
            showAppPreferences model

        UpdateExplainationNote note ->
            updateExplainationNote note model

        UploadReferenceImage ->
            uploadReferenceImage model

        SelectedReferenceImage file ->
            selectedReferenceImage file model

        LoadedReferenceImage dataUrl ->
            loadedReferenceImage dataUrl model


loadedReferenceImage : DataUrl -> Model -> ( Model, Cmd Msg )
loadedReferenceImage dataUrl model =
    ( { model
        | charExplainations =
            case model.selectedChar of
                Just char ->
                    Dict.update
                        char
                        (\explaination ->
                            Maybe.map
                                (\e ->
                                    let
                                        referenceImage =
                                            e.referenceImage
                                    in
                                    { e
                                        | referenceImage =
                                            { referenceImage
                                                | image =
                                                    dataUrl
                                            }
                                    }
                                )
                                explaination
                        )
                        model.charExplainations

                Nothing ->
                    model.charExplainations
      }
    , Cmd.none
    )


selectedReferenceImage : File -> Model -> ( Model, Cmd Msg )
selectedReferenceImage file model =
    ( model
    , Task.perform LoadedReferenceImage (File.toUrl file)
    )


uploadReferenceImage : Model -> ( Model, Cmd Msg )
uploadReferenceImage model =
    ( model
    , File.Select.file [ "image/jpeg", "image/png" ] SelectedReferenceImage
    )


updateExplainationNote : String -> Model -> ( Model, Cmd Msg )
updateExplainationNote note model =
    ( { model
        | charExplainations =
            case model.selectedChar of
                Just char ->
                    Dict.update
                        char
                        (\explaination ->
                            Maybe.map
                                (\e ->
                                    { e
                                        | note =
                                            note
                                    }
                                )
                                explaination
                        )
                        model.charExplainations

                Nothing ->
                    model.charExplainations
      }
    , Cmd.none
    )


toggleIsReferenceCharShown : Model -> ( Model, Cmd Msg )
toggleIsReferenceCharShown model =
    ( { model
        | isReferenceCharShown =
            not model.isReferenceCharShown
      }
    , Cmd.none
    )


addPendingComponentChar : Model -> ( Model, Cmd Msg )
addPendingComponentChar model =
    ( { model
        | chars =
            Maybe.map
                (\selectedChar ->
                    Dict.update
                        selectedChar
                        (Maybe.map <|
                            addComponentToMyChar model.chars (charFromString model.newComponentChar)
                        )
                        model.chars
                )
                model.selectedChar
                |> Maybe.withDefault model.chars
        , popUp =
            NoPopUp
      }
    , Cmd.none
    )


updatePendingComponentChar : String -> Model -> ( Model, Cmd Msg )
updatePendingComponentChar charInput model =
    ( { model
        | newComponentChar =
            charInput
      }
    , Cmd.none
    )


addComponentToSelectedChar : Model -> ( Model, Cmd Msg )
addComponentToSelectedChar model =
    Debug.todo "TODO"


requestAddComponentToSelectedChar : Model -> ( Model, Cmd Msg )
requestAddComponentToSelectedChar model =
    ( { model
        | popUp =
            AddComponentToSelectedCharPopUp
      }
    , Cmd.none
    )


showAppPreferences : Model -> ( Model, Cmd Msg )
showAppPreferences model =
    ( { model
        | popUp =
            AppPreferencesPopUp
      }
    , Cmd.none
    )


updateMode : Mode -> Model -> ( Model, Cmd Msg )
updateMode mode model =
    ( { model
        | mode =
            mode
      }
    , Cmd.none
    )


updateDevice : Int -> Int -> Model -> ( Model, Cmd Msg )
updateDevice width height model =
    let
        device =
            E.classifyDevice
                { width = width
                , height = height
                }

        minSize =
            toFloat <|
                min width height
                    - (case device.orientation of
                        E.Portrait ->
                            0

                        E.Landscape ->
                            model.fontSize.large + model.spacing.small
                      )

        unitSize =
            minSize / (toFloat model.boxUnits + model.borderUnits * 2)
    in
    ( { model
        | device =
            device
        , unitSize =
            unitSize
        , thumbnailUnitSize =
            case device.class of
                E.Phone ->
                    2

                E.Tablet ->
                    2.6

                E.Desktop ->
                    2.6

                E.BigDesktop ->
                    4
        , spacing =
            let
                tiny =
                    round << E.modular 5 1.25

                small =
                    round << E.modular 10 1.25

                medium =
                    round << E.modular 20 1.5

                large =
                    round << E.modular 30 1.5
            in
            case device.class of
                E.Phone ->
                    { tiny = tiny -3
                    , small = small -3
                    , medium = medium -3
                    , large = large -3
                    }

                E.Tablet ->
                    { tiny = tiny -2
                    , small = small -2
                    , medium = medium -2
                    , large = large -2
                    }

                E.Desktop ->
                    { tiny = tiny -1
                    , small = small -1
                    , medium = medium -1
                    , large = large -1
                    }

                E.BigDesktop ->
                    { tiny = tiny 1
                    , small = small 1
                    , medium = medium 1
                    , large = large 1
                    }
        , fontSize =
            let
                small =
                    round << E.modular 16 1.1

                medium =
                    round << E.modular 20 1.1

                large =
                    round << E.modular 30 1.25

                title =
                    round << E.modular 40 1.25
            in
            case device.class of
                E.Phone ->
                    { small = small -2
                    , medium = medium -1
                    , large = large -2
                    , title = title -2
                    , thumb = title -2
                    }

                E.Tablet ->
                    { small = small -1
                    , medium = medium -1
                    , large = large -1
                    , title = title -1
                    , thumb = title -1
                    }

                E.Desktop ->
                    { small = small -1
                    , medium = medium -1
                    , large = large -1
                    , title = title -1
                    , thumb = large -1
                    }

                E.BigDesktop ->
                    { small = small 1
                    , medium = medium 1
                    , large = large 1
                    , title = title 1
                    , thumb = large 1
                    }
      }
    , Cmd.none
    )


gotTranslations : Result Http.Error I18Next.Translations -> Model -> ( Model, Cmd Msg )
gotTranslations trs model =
    ( { model
        | trs =
            Result.withDefault model.trs trs
      }
    , Cmd.none
    )


updateLanguage : Language -> Model -> ( Model, Cmd Msg )
updateLanguage language model =
    ( { model
        | language =
            language
      }
    , Http.get
        { url =
            "translations/" ++ stringFromLanguage language ++ ".json"
        , expect =
            Http.expectJson GotTranslations I18Next.translationsDecoder
        }
    )


deleteActiveComponent : Model -> ( Model, Cmd Msg )
deleteActiveComponent ({ activeComponentIndex } as model) =
    ( { model
        | chars =
            Maybe.map
                (\selectedChar ->
                    Dict.update
                        selectedChar
                        (Maybe.map <|
                            updateMyCharComponents
                                (List.Extra.removeAt (Maybe.withDefault -1 activeComponentIndex))
                        )
                        model.chars
                )
                model.selectedChar
                |> Maybe.withDefault model.chars
        , activeComponentIndex =
            Nothing
      }
    , Cmd.none
    )


copyActiveComponent : Model -> ( Model, Cmd Msg )
copyActiveComponent ({ activeComponentIndex, isSnapToGrid, boxUnits, borderUnits } as model) =
    ( { model
        | chars =
            Maybe.map
                (\selectedChar ->
                    Dict.update
                        selectedChar
                        (Maybe.map <|
                            \myChar ->
                                case myChar of
                                    SimpleChar _ ->
                                        myChar

                                    CompoundChar c components ->
                                        let
                                            activeComponent =
                                                Maybe.andThen
                                                    (\index ->
                                                        Array.get index <| Array.fromList components
                                                    )
                                                    activeComponentIndex
                                        in
                                        case activeComponent of
                                            Just component ->
                                                let
                                                    delta =
                                                        5

                                                    copiedPosition =
                                                        (if isSnapToGrid then
                                                            snapToGrid boxUnits

                                                         else
                                                            identity
                                                        )
                                                        <|
                                                            Vector2.add (Vector2.vec2 delta delta) component.position

                                                    copiedComponent =
                                                        { char = component.char
                                                        , dimension = component.dimension
                                                        , position = copiedPosition
                                                        }
                                                in
                                                CompoundChar c (components ++ [ copiedComponent ])

                                            Nothing ->
                                                myChar
                        )
                        model.chars
                )
                model.selectedChar
                |> Maybe.withDefault model.chars
      }
    , Cmd.none
    )


requestClearChars : MyCharType -> Model -> ( Model, Cmd Msg )
requestClearChars myCharType model =
    ( { model
        | popUp =
            ConfirmClearCharsPopUp myCharType
      }
    , Cmd.none
    )


clearChars : MyCharType -> Model -> ( Model, Cmd Msg )
clearChars myCharType model =
    case myCharType of
        SimpleCharType ->
            ( { model
                | chars =
                    Dict.filter
                        (\char myChar ->
                            case myChar of
                                SimpleChar _ ->
                                    False

                                _ ->
                                    True
                        )
                        model.chars
                , popUp =
                    NoPopUp
              }
            , clearSimpleCharsPort ()
            )

        CompoundCharType ->
            ( { model
                | chars =
                    Dict.filter
                        (\char myChar ->
                            case myChar of
                                CompoundChar _ _ ->
                                    False

                                _ ->
                                    True
                        )
                        model.chars
                , popUp =
                    NoPopUp
              }
            , Cmd.none
            )


downloadSelectedChar : Model -> ( Model, Cmd Msg )
downloadSelectedChar model =
    ( model
    , downloadCharPort <|
        String.fromChar
            -- impossible
            (Maybe.withDefault '?'
                model.selectedChar
            )
    )


toggleIsSnapToGrid : Model -> ( Model, Cmd Msg )
toggleIsSnapToGrid model =
    let
        newIsSnapToGrid =
            not model.isSnapToGrid
    in
    ( { model
        | isSnapToGrid =
            newIsSnapToGrid
        , isAspectRatioLocked =
            if newIsSnapToGrid then
                False

            else
                model.isAspectRatioLocked
      }
    , Cmd.none
    )


updateParagraphForPreview : String -> Model -> ( Model, Cmd Msg )
updateParagraphForPreview paragraph model =
    ( { model
        | paragraphForPreview =
            paragraph
      }
    , Cmd.none
    )


previewInParagraph : Model -> ( Model, Cmd Msg )
previewInParagraph model =
    ( { model
        | popUp =
            PreviewInParagraphPopUp
      }
    , Cmd.none
    )


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
        , borderUnits =
            lerp minStrokeWidth maxStrokeWidth minBorderUnits maxBorderUnits newStrokeWidth
      }
    , Cmd.none
    )


saveModel : Model -> ( Model, Cmd Msg )
saveModel model =
    ( model, saveModelPort <| encodeModel model )


encodeModel : Model -> Value
encodeModel { chars, simpleCharSvgs, strokeWidth, language } =
    Encode.object
        [ ( "chars", Encode.dict String.fromChar encodeMyChar chars )
        , ( "strokeWidth", Encode.float strokeWidth )
        , ( "language", encodeLanguage language )
        ]


encodeLanguage : Language -> Value
encodeLanguage =
    Encode.string << stringFromLanguage


stringFromLanguage : Language -> String
stringFromLanguage language =
    case language of
        LanguageEn ->
            "LanguageEn"

        LanguageZhHans ->
            "LanguageZhHans"

        LanguageZhHant ->
            "LanguageZhHant"


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
encodeMyCharRef { char, dimension, position } =
    Encode.object
        [ ( "char", encodeChar char )
        , ( "dimension", encodeVec2 dimension )
        , ( "position", encodeVec2 position )
        ]


encodeVec2 : Vec2 -> Value
encodeVec2 vec =
    Encode.object
        [ ( "x", Encode.float <| Vector2.getX vec )
        , ( "y", Encode.float <| Vector2.getY vec )
        ]


gotModel : Value -> Model -> ( Model, Cmd Msg )
gotModel savedModelJson model =
    case Decode.decodeValue decodeSavedModel savedModelJson of
        Ok { chars, strokeWidth, language } ->
            let
                newModel =
                    { model
                        | chars =
                            chars
                        , strokeWidth =
                            strokeWidth
                        , language =
                            language
                    }
            in
            updateLanguage language newModel

        Err err ->
            -- let
            -- _ =
            --     Debug.log "err" err
            -- in
            ( model, Cmd.none )


decodeSavedModel : Decoder SavedModel
decodeSavedModel =
    Decode.map3 SavedModel
        (Decode.field "chars"
            (Decode.map (Dict.Extra.mapKeys charFromString) <|
                Decode.dict decodeMyChar
            )
        )
        (Decode.field "strokeWidth" Decode.float)
        (Decode.field "language" decodeLanguage)


decodeLanguage : Decoder Language
decodeLanguage =
    Decode.string
        |> Decode.andThen
            (\language ->
                case language of
                    "LanguageEn" ->
                        Decode.succeed LanguageEn

                    "LanguageZhHans" ->
                        Decode.succeed LanguageZhHans

                    "LanguageZhHant" ->
                        Decode.succeed LanguageZhHant

                    _ ->
                        Decode.fail <|
                            "Trying to decode Language, but "
                                ++ language
                                ++ " is not supported."
            )


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
    Decode.map3 MyCharRef
        (Decode.field "char" decodeChar)
        (Decode.field "dimension" decodeVec2)
        (Decode.field "position" decodeVec2)


decodeVec2 : Decoder Vec2
decodeVec2 =
    Decode.map2 Vector2.vec2
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


dragMsg : Draggable.Msg DragData -> Model -> ( Model, Cmd Msg )
dragMsg msg model =
    Draggable.update dragConfig msg model


setActiveComponent : Maybe Int -> Model -> ( Model, Cmd Msg )
setActiveComponent index model =
    ( { model
        | activeComponentIndex =
            index
      }
    , Cmd.none
    )


stopDragging : Model -> ( Model, Cmd Msg )
stopDragging model =
    ( { model
        | activeComponentIndex =
            Nothing
        , dragDelta =
            Vector2.vec2 0 0
      }
    , Cmd.none
    )


startDragging : DragData -> Model -> ( Model, Cmd Msg )
startDragging { index, scale } ({ isSnapToGrid, boxUnits, strokeWidth, unitSize } as model) =
    let
        unitPercent =
            100 / toFloat boxUnits
    in
    ( updateActiveComponent
        (if isSnapToGrid then
            updateMyCharRefPositionAndDimension unitPercent ScaleTopLeft (snapToGrid boxUnits) (snapToGrid boxUnits)

         else
            identity
        )
        { model
            | activeComponentIndex =
                Just index
            , activeScale =
                scale
            , dragDelta =
                Vector2.vec2 0 0
        }
    , Cmd.none
    )


type OffsetType
    = DimensionOffset
    | PositionOffset


onDragBy : Vec2 -> Model -> ( Model, Cmd Msg )
onDragBy delta ({ dragDelta, isSnapToGrid, activeComponentIndex, activeScale, boxUnits, unitSize, chars, isAspectRatioLocked, strokeWidth } as model) =
    let
        factor =
            100 / (toFloat boxUnits * unitSize)
    in
    ( if isSnapToGrid then
        let
            oldDeltaX =
                Vector2.getX dragDelta

            oldDeltaY =
                Vector2.getY dragDelta

            -- _ =
            --     Debug.log "newDragDelta" newDragDelta
            -- _ =
            --     Debug.log "deltaX" deltaX
            -- _ =
            --     Debug.log "deltaY" deltaY
            deltaX =
                Vector2.getX delta

            deltaY =
                Vector2.getY delta

            newDeltaX =
                if sign oldDeltaX /= 0 && sign deltaX /= 0 && sign oldDeltaX /= sign deltaX then
                    deltaX

                else
                    oldDeltaX + deltaX

            newDeltaY =
                if sign oldDeltaY /= 0 && sign deltaY /= 0 && sign oldDeltaY /= sign deltaY then
                    deltaY

                else
                    oldDeltaY + deltaY
        in
        if abs newDeltaX >= unitSize || abs newDeltaY >= unitSize then
            updateOnDrag factor
                (Vector2.vec2 (roundToUnitSize unitSize newDeltaX) (roundToUnitSize unitSize newDeltaY))
                { model
                    | dragDelta =
                        Vector2.vec2 0 0
                }

        else
            { model
                | dragDelta =
                    Vector2.vec2 newDeltaX newDeltaY
            }

      else
        updateOnDrag factor delta model
    , Cmd.none
    )


roundToUnitSize : Float -> Float -> Float
roundToUnitSize unitSize n =
    unitSize * (toFloat <| round (n / unitSize))


updateOnDrag : Float -> Vec2 -> Model -> Model
updateOnDrag factor delta ({ dragDelta, activeScale, boxUnits, unitSize, chars, isAspectRatioLocked } as model) =
    updateActiveComponent
        (\char ->
            let
                offsetDimension xDir yDir =
                    offsetDrag DimensionOffset isAspectRatioLocked char.dimension factor xDir yDir delta

                offsetPosition xDir yDir =
                    offsetDrag PositionOffset isAspectRatioLocked char.dimension factor xDir yDir delta

                unitPercent =
                    100 / toFloat boxUnits
            in
            (case activeScale of
                NoScale ->
                    updateMyCharRefPosition
                        (Vector2.add <| Vector2.scale factor delta)

                ScaleTopLeft ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleTopLeft
                        (Vector2.add (offsetPosition 1 1))
                        (Vector2.add (offsetDimension -1 -1))

                ScaleTopRight ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleTopRight
                        (Vector2.add (offsetPosition 0 1))
                        (Vector2.add (offsetDimension 1 -1))

                ScaleBottomLeft ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleBottomLeft
                        (Vector2.add (offsetPosition 1 0))
                        (Vector2.add (offsetDimension -1 1))

                ScaleBottomRight ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleBottomRight
                        (Vector2.add (offsetPosition 0 0))
                        (Vector2.add (offsetDimension 1 1))

                ScaleLeft ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleTopLeft
                        (Vector2.add (offsetPosition 1 0))
                        (Vector2.add (offsetDimension -1 0))

                ScaleRight ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleTopRight
                        (Vector2.add (offsetPosition 0 0))
                        (Vector2.add (offsetDimension 1 0))

                ScaleTop ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleTopLeft
                        (Vector2.add (offsetPosition 0 1))
                        (Vector2.add (offsetDimension 0 -1))

                ScaleBottom ->
                    updateMyCharRefPositionAndDimension
                        unitPercent
                        ScaleTopRight
                        (Vector2.add (offsetPosition 0 0))
                        (Vector2.add (offsetDimension 0 1))
            )
                char
        )
        model


updateActiveComponent : (MyCharRef -> MyCharRef) -> Model -> Model
updateActiveComponent func ({ activeComponentIndex } as model) =
    case model.selectedChar of
        Just selectedChar ->
            { model
                | chars =
                    Dict.update
                        selectedChar
                        (Maybe.map
                            (\myChar ->
                                case myChar of
                                    SimpleChar ref ->
                                        SimpleChar (func ref)

                                    CompoundChar _ _ ->
                                        (updateMyCharComponents <|
                                            List.Extra.updateAt
                                                -- impossible
                                                (Maybe.withDefault -1 activeComponentIndex)
                                                func
                                        )
                                            myChar
                            )
                        )
                        model.chars
            }

        Nothing ->
            model


offsetDrag : OffsetType -> Bool -> Vec2 -> Float -> Float -> Float -> Vec2 -> Vec2
offsetDrag offsetType isAspectRatioLocked dimension factor xDir yDir delta =
    let
        deltaX =
            Vector2.getX delta

        deltaY =
            Vector2.getY delta
    in
    Vector2.scale factor <|
        if isAspectRatioLocked then
            let
                averagedDelta =
                    (abs deltaX + abs deltaY) / 2

                ratioOfXOverY =
                    Vector2.getX dimension / Vector2.getY dimension

                averagedDeltaX =
                    sign deltaX * averagedDelta * ratioOfXOverY

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


sign : number -> number
sign n =
    if n < 0 then
        -1

    else if n > 0 then
        1

    else
        0


snapToGrid : Int -> Vec2 -> Vec2
snapToGrid boxUnits position =
    let
        unitPercent =
            100 / toFloat boxUnits

        roundToGrid pos =
            (*) unitPercent <| toFloat <| round <| pos / unitPercent
    in
    Vector2.vec2
        (roundToGrid <| Vector2.getX position)
        (roundToGrid <| Vector2.getY position)


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

                    newComponent =
                        { char = componentChar
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
        | inputError =
            Nothing
      }
    , Cmd.none
    )


showInputError : InputError -> Model -> ( Model, Cmd Msg )
showInputError error model =
    ( { model
        | inputError =
            Just error
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
                , dimension = Vector2.vec2 100 100
                , position = Vector2.vec2 0 0
                }
                []
    in
    ( { model
        | chars =
            Dict.insert newChar newCompoundChar model.chars
        , charExplainations =
            Dict.insert newChar emptyExplaination model.charExplainations
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
        , activeComponentIndex =
            Nothing
      }
    , Cmd.none
    )


gotSavedSimpleChars : Value -> Model -> ( Model, Cmd Msg )
gotSavedSimpleChars svgsJson model =
    ( case Decode.decodeValue decodeSimpleCharSvgs svgsJson of
        Ok svgs ->
            { model
                | simpleCharSvgs =
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


gotNewSimpleChars : Value -> Model -> ( Model, Cmd Msg )
gotNewSimpleChars svgsJson model =
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


updateMyCharComponents : (List MyCharRef -> List MyCharRef) -> MyChar -> MyChar
updateMyCharComponents func myChar =
    case myChar of
        SimpleChar _ ->
            myChar

        CompoundChar c components ->
            CompoundChar c (func components)


updateMyCharRefPositionAndDimension : Float -> Scale -> (Vec2 -> Vec2) -> (Vec2 -> Vec2) -> MyCharRef -> MyCharRef
updateMyCharRefPositionAndDimension unitPercent scale updatePos updateDim myCharRef =
    let
        oldDim =
            myCharRef.dimension

        oldPos =
            myCharRef.position

        updatedDim =
            updateDim oldDim

        updatedPos =
            updatePos oldPos

        minX =
            if Vector2.getX oldDim < unitPercent then
                Vector2.getX oldDim

            else
                minComponentSize

        minY =
            if Vector2.getY oldDim < unitPercent then
                Vector2.getY oldDim

            else
                minComponentSize

        scalePoint =
            (case scale of
                ScaleTopLeft ->
                    identity

                ScaleTopRight ->
                    Vector2.add (Vector2.vec2 (Vector2.getX updatedDim) 0)

                ScaleBottomLeft ->
                    Vector2.add (Vector2.vec2 0 (Vector2.getY updatedDim))

                ScaleBottomRight ->
                    Vector2.add updatedDim

                ScaleLeft ->
                    \_ -> Vector2.setX (Vector2.getX updatedPos) myCharRef.position

                ScaleRight ->
                    Vector2.add (Vector2.vec2 (Vector2.getX updatedDim) 0)

                ScaleTop ->
                    \_ -> Vector2.setY (Vector2.getY updatedPos) myCharRef.position

                ScaleBottom ->
                    Vector2.add (Vector2.vec2 0 (Vector2.getY updatedDim))

                NoScale ->
                    identity
            )
                updatedPos

        -- _ = Debug.log "positionClampResult" positionClampResult
        -- _ = Debug.log "dimensionClampResult" dimensionClampResult
        ( newPos, newDim ) =
            mapTuple
                (case ( clampVec2 0 100 0 100 scalePoint, clampVec2 minX 100 minY 100 updatedDim ) of
                    ( ClampSafe _, ClampSafe dim ) ->
                        ( \_ -> updatedPos, \_ -> dim )

                    ( ClampSafeX _, ClampSafeX dimX ) ->
                        ( Vector2.setX (Vector2.getX updatedPos), Vector2.setX dimX )

                    ( ClampSafeY _, ClampSafeY dimY ) ->
                        ( Vector2.setY (Vector2.getY updatedPos), Vector2.setY dimY )

                    _ ->
                        ( identity, identity )
                )
                ( myCharRef.position, myCharRef.dimension )
    in
    { myCharRef
        | position =
            newPos
        , dimension =
            newDim
    }


mapTuple : ( a -> x, b -> y ) -> ( a, b ) -> ( x, y )
mapTuple ( mapA, mapB ) ( a, b ) =
    ( mapA a, mapB b )


updateMyCharRefPosition : (Vec2 -> Vec2) -> MyCharRef -> MyCharRef
updateMyCharRefPosition func myCharRef =
    { myCharRef
        | position =
            case
                clampVec2 0
                    (100 - Vector2.getX myCharRef.dimension)
                    0
                    (100 - Vector2.getY myCharRef.dimension)
                    (func myCharRef.position)
            of
                ClampSafe newPos ->
                    newPos

                ClampSafeX posX ->
                    Vector2.setX posX myCharRef.position

                ClampSafeY posY ->
                    Vector2.setY posY myCharRef.position

                ClampUnsafe newPos ->
                    newPos
    }


clampVec2 : Float -> Float -> Float -> Float -> Vec2 -> ClampResult
clampVec2 minX maxX minY maxY vec =
    let
        x =
            Vector2.getX vec

        y =
            Vector2.getY vec

        isSafeX =
            minX <= x && x <= maxX

        isSafeY =
            minY <= y && y <= maxY
    in
    if isSafeX && isSafeY then
        ClampSafe vec

    else if isSafeX then
        ClampSafeX x

    else if isSafeY then
        ClampSafeY y

    else
        ClampUnsafe <| Vector2.vec2 (clamp minX maxX x) (clamp minY maxY y)


type ClampResult
    = ClampSafe Vec2
    | ClampSafeX Float
    | ClampSafeY Float
    | ClampUnsafe Vec2



-- Requires: char to be in chars


myCharTypeFromChar : Dict Char MyChar -> Char -> MyCharType
myCharTypeFromChar chars char =
    case Dict.get char chars of
        Just myChar ->
            myCharTypeFromMyChar myChar

        Nothing ->
            -- impossible
            SimpleCharType


myCharTypeFromMyChar : MyChar -> MyCharType
myCharTypeFromMyChar myChar =
    case myChar of
        SimpleChar _ ->
            SimpleCharType

        CompoundChar _ _ ->
            CompoundCharType



---- VIEW ----


view : Model -> Html Msg
view ({ mode, spacing, fontSize, device } as model) =
    E.layout
        [ E.padding spacing.large
        , E.inFront <| popUp model
        , E.width E.fill
        , E.height E.fill
        , Font.size fontSize.medium
        , Font.family [ Font.typeface "Source Han Sans TC", Font.sansSerif ]
        ]
    <|
        case mode of
            BrowseMode ->
                E.el
                    [ E.inFront <|
                        appHeader model
                    , E.width E.fill
                    , E.height E.fill
                    ]
                    (charPanels model)

            EditMode ->
                E.column
                    [ E.width E.fill
                    , E.height E.fill
                    ]
                    [ (case device.orientation of
                        E.Portrait ->
                            E.column

                        E.Landscape ->
                            E.row
                      )
                        [ E.spacing spacing.large ]
                        [ editor model
                        , editorSidePanel model
                        ]
                    ]


appHeader : Model -> E.Element Msg
appHeader ({ palette, spacing, fontSize } as model) =
    E.row
        [ E.alignRight
        , E.spacing spacing.medium
        ]
        [ iconButton
            { icon =
                FeatherIcons.eye
            , size =
                fontSize.large
            , onPress =
                Just <| PreviewInParagraph
            }
        , iconButton
            { icon =
                FeatherIcons.settings
            , size =
                fontSize.large
            , onPress =
                Just <| ShowAppPreferences
            }
        ]


popUp : Model -> E.Element Msg
popUp model =
    case model.popUp of
        AddCompoundCharPopUp ->
            addCompoundCharPopUp model

        ConfirmDeleteSelectedCharPopUp ->
            confirmDeleteSelectedCharPopUp model

        ConfirmClearCharsPopUp myCharType ->
            confirmClearCharsPopUp myCharType model

        PreviewInParagraphPopUp ->
            previewInParagraphPopUp model

        AppPreferencesPopUp ->
            appPreferencesPopUp model

        AddComponentToSelectedCharPopUp ->
            addComponentToSelectedCharPopUp model

        NoPopUp ->
            E.none


addComponentToSelectedCharPopUp : Model -> E.Element Msg
addComponentToSelectedCharPopUp ({ chars, selectedChar, trs, newComponentChar, inputError, boxUnits, thumbnailUnitSize, palette, spacing, fontSize } as model) =
    let
        newInputError =
            case String.uncons newComponentChar of
                Just ( char, _ ) ->
                    let
                        inputLength =
                            String.length newComponentChar
                    in
                    if inputLength /= 1 then
                        Just <| InvalidInputLength inputLength

                    else if
                        isCharPartOfMyChar chars
                            (unboxChar selectedChar)
                            (myCharFromChar chars char)
                    then
                        Just <| ContainsSelfReference

                    else if not (Dict.member char chars) then
                        Just <| CharacterNotFound

                    else
                        Nothing

                Nothing ->
                    Just <| InvalidInputLength 0

        width =
            toFloat boxUnits * thumbnailUnitSize * 2.3
    in
    popUpTemplate
        { borderColor =
            palette.lightFg
        , isCloseButtonShown =
            True
        }
        model
        [ Input.text
            [ E.width <| E.px <| fontSize.medium * 5
            , E.centerX
            , onEnter <|
                case newInputError of
                    Nothing ->
                        Just AddPendingComponentChar

                    Just _ ->
                        Nothing
            ]
            { onChange =
                UpdatePendingComponentChar
            , text =
                newComponentChar
            , placeholder =
                Nothing
            , label =
                Input.labelAbove
                    [ E.paddingEach { top = spacing.medium, bottom = 0, left = 0, right = 0 } ]
                    (E.text (Translations.character trs))
            }
        , E.el
            ([ E.centerX
             , E.below <|
                case inputError of
                    Just error ->
                        E.paragraph
                            [ E.centerX
                            , Font.size fontSize.small
                            , E.width <| E.px <| round width
                            , E.padding spacing.small
                            ]
                            [ E.text <|
                                case error of
                                    InvalidInputLength _ ->
                                        Translations.acceptOnlyOneCharacter trs

                                    ContainsSelfReference ->
                                        Translations.cannotContainSelfReference trs

                                    CharacterNotFound ->
                                        Translations.characterNotFound trs newComponentChar
                            ]

                    Nothing ->
                        E.none
             ]
                ++ (case newInputError of
                        Nothing ->
                            []

                        Just error ->
                            [ Events.onMouseEnter <| ShowInputError error
                            , Events.onMouseLeave HideInputError
                            ]
                   )
            )
          <|
            iconButton
                { icon =
                    case newInputError of
                        Nothing ->
                            FeatherIcons.checkCircle

                        Just _ ->
                            FeatherIcons.alertTriangle
                , size =
                    fontSize.title
                , onPress =
                    case newInputError of
                        Nothing ->
                            Just AddPendingComponentChar

                        Just _ ->
                            Nothing
                }
        ]


appPreferencesPopUp : Model -> E.Element Msg
appPreferencesPopUp ({ palette, spacing, fontSize } as model) =
    popUpTemplate
        { borderColor =
            palette.lightFg
        , isCloseButtonShown =
            True
        }
        model
        [ Input.radio
            [ E.spacing spacing.small
            , E.htmlAttribute <| Html.Attributes.style "align-self" "center"
            ]
            { onChange = UpdateLanguage
            , selected = Just model.language
            , label =
                Input.labelAbove [ E.alignTop, E.paddingXY 0 spacing.small ]
                    (E.text (Translations.language model.trs))
            , options =
                [ Input.optionWith LanguageEn
                    (radioOption palette.lightFg fontSize (E.text "English"))
                , Input.optionWith LanguageZhHans
                    (radioOption palette.lightFg fontSize (E.text "中文（简体）"))
                , Input.optionWith LanguageZhHant
                    (radioOption palette.lightFg fontSize (E.text "中文（繁體）"))
                ]
            }
        ]


popUpTemplate :
    { borderColor : E.Color
    , isCloseButtonShown : Bool
    }
    -> Model
    -> List (E.Element Msg)
    -> E.Element Msg
popUpTemplate { borderColor, isCloseButtonShown } { palette, spacing, fontSize, boxUnits, thumbnailUnitSize } =
    let
        borderWidth =
            6

        width =
            toFloat boxUnits * thumbnailUnitSize * 2.3
    in
    E.column
        ([ E.centerX
         , E.centerY
         , Background.color palette.lightBg
         , E.width <| E.px <| round <| width
         , E.height <| E.px <| round <| width + toFloat fontSize.title
         , E.spacing spacing.small
         , E.paddingXY spacing.small spacing.large
         , Font.size fontSize.medium
         , E.inFront <|
            if isCloseButtonShown then
                E.el
                    [ E.padding spacing.tiny ]
                <|
                    iconButton
                        { icon =
                            FeatherIcons.x
                        , size =
                            fontSize.large
                        , onPress =
                            Just ClosePopUp
                        }

            else
                E.none
         ]
            ++ highlightBorder
                { color = borderColor
                , width = borderWidth
                , spacing = spacing
                , style = Border.dashed
                , glowWidth = borderWidth
                }
        )


confirmClearCharsPopUp : MyCharType -> Model -> E.Element Msg
confirmClearCharsPopUp myCharType ({ trs } as model) =
    confirmDeletePopUpTemplate
        model
        (Translations.allCharsOfAType trs (stringFromMyCharType trs myCharType))
        (ClearChars myCharType)


confirmDeletePopUpTemplate : Model -> String -> Msg -> E.Element Msg
confirmDeletePopUpTemplate ({ trs, boxUnits, thumbnailUnitSize, palette, spacing, fontSize } as model) targetName onConfirm =
    popUpTemplate
        { borderColor =
            palette.danger
        , isCloseButtonShown =
            False
        }
        model
        [ E.el
            [ E.centerX ]
            (E.column
                [ E.spacing spacing.medium ]
                [ E.paragraph []
                    [ E.text (Translations.doYouWantToDelete trs)
                    , E.el [ Font.bold ] (E.text targetName)
                    , E.text " ?"
                    ]
                , E.paragraph [ Font.size fontSize.small ]
                    [ E.text (Translations.thisCannotBeUndone trs)
                    ]
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
                        Just onConfirm
                    }
                )
            ]
        ]


previewInParagraphPopUp : Model -> E.Element Msg
previewInParagraphPopUp ({ palette, spacing, fontSize } as model) =
    let
        previewFontSize =
            fontSize.title * 2

        maxParagraphInputWidth =
            fontSize.medium * 30

        maxControlWidth =
            maxParagraphInputWidth + 300
    in
    E.column
        [ E.centerX
        , E.centerY
        , Background.color palette.lightBg
        , E.width E.fill
        , E.height E.fill
        , E.spacing spacing.medium
        , E.padding spacing.large
        , Font.size fontSize.medium
        , E.inFront
            (E.el
                [ E.padding spacing.medium ]
             <|
                iconButton
                    { icon =
                        FeatherIcons.x
                    , size =
                        fontSize.large
                    , onPress =
                        Just ClosePopUp
                    }
            )
        ]
        [ E.el
            [ E.centerX ]
            (titleText fontSize (Translations.previewInParagraph model.trs))
        , E.row
            [ E.height E.fill
            , E.width
                (E.fill
                    |> E.maximum maxControlWidth
                )
            , E.centerX
            , E.spacing spacing.medium
            ]
            [ E.column
                [ E.width <| E.fillPortion 5
                , E.height E.fill
                , E.spacing spacing.medium
                ]
                [ E.el
                    [ E.alignRight
                    , E.width (E.fill |> E.maximum maxParagraphInputWidth)
                    , E.height <| E.fillPortion 1
                    , E.scrollbarY
                    ]
                    (Input.multiline
                        [ E.width <| E.fill
                        , E.height <| E.fill
                        ]
                        { onChange =
                            UpdateParagraphForPreview
                        , text =
                            model.paragraphForPreview
                        , placeholder =
                            Just <| Input.placeholder [ E.alignLeft ] (E.text <| Translations.writeTextHereToPreview model.trs)
                        , label =
                            Input.labelHidden <| Translations.writeTextHereToPreview model.trs
                        , spellcheck =
                            False
                        }
                    )
                ]
            , E.el [ E.width <| E.fillPortion 2, E.height E.fill, E.paddingXY 0 spacing.small ] <| strokeWidthPreference model
            ]
        , E.el
            [ E.width E.fill
            , E.height <| E.fillPortion 3
            , Font.size <| previewFontSize
            , E.scrollbarY
            ]
            (renderPreviewInParagraph previewFontSize model)
        ]


renderPreviewInParagraph : Int -> Model -> E.Element Msg
renderPreviewInParagraph displayFontSize ({ paragraphForPreview, chars, unitSize, boxUnits, borderUnits, strokeWidth, simpleCharSvgs, activeComponentIndex, isAspectRatioLocked, isSnapToGrid } as model) =
    let
        lines =
            String.split "\n" paragraphForPreview
    in
    E.column
        [ E.centerX
        , E.centerY
        ]
    <|
        List.map
            (E.wrappedRow
                [ E.width E.fill
                ]
                << List.map
                    (\char ->
                        case Dict.get char chars of
                            Just myChar ->
                                E.html <|
                                    renderChar
                                        { model
                                            | unitSize = toFloat displayFontSize / toFloat boxUnits
                                            , strokeWidth = strokeWidth * toFloat displayFontSize / (toFloat boxUnits * unitSize)
                                        }
                                        { isThumbnail = True }
                                        myChar

                            Nothing ->
                                E.text <| String.fromChar char
                    )
                << (\charsInLine ->
                        if List.isEmpty charsInLine then
                            [ ' ' ]

                        else
                            charsInLine
                   )
                << String.toList
            )
            lines


confirmDeleteSelectedCharPopUp : Model -> E.Element Msg
confirmDeleteSelectedCharPopUp ({ selectedChar } as model) =
    confirmDeletePopUpTemplate
        model
        (String.fromChar (unboxChar selectedChar))
        DeleteSelectedChar


addCompoundCharPopUp : Model -> E.Element Msg
addCompoundCharPopUp ({ trs, activeComponentIndex, newCompoundChar, inputError, palette, spacing, fontSize, boxUnits, thumbnailUnitSize } as model) =
    let
        inputLength =
            String.length newCompoundChar

        newInputError =
            if inputLength /= 1 then
                Just <| InvalidInputLength inputLength

            else
                Nothing

        width =
            toFloat boxUnits * thumbnailUnitSize * 2.3
    in
    popUpTemplate
        { borderColor =
            palette.lightFg
        , isCloseButtonShown =
            True
        }
        model
        [ Input.text
            [ E.width <| E.px <| fontSize.medium * 5
            , E.centerX
            , onEnter <|
                case newInputError of
                    Nothing ->
                        Just AddPendingCompoundChar

                    Just _ ->
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
                    (E.text (Translations.character trs))
            }
        , E.el
            ([ E.centerX
             , E.below <|
                case inputError of
                    Just _ ->
                        E.paragraph
                            [ E.centerX
                            , Font.size fontSize.small
                            , E.width <| E.px <| round width
                            , E.padding spacing.small
                            ]
                            [ E.text (Translations.acceptOnlyOneCharacter trs)
                            ]

                    Nothing ->
                        E.none
             ]
                ++ (case newInputError of
                        Nothing ->
                            []

                        Just error ->
                            [ Events.onMouseEnter <| ShowInputError error
                            , Events.onMouseLeave HideInputError
                            ]
                   )
            )
          <|
            iconButton
                { icon =
                    case newInputError of
                        Nothing ->
                            FeatherIcons.checkCircle

                        Just _ ->
                            FeatherIcons.alertTriangle
                , size =
                    fontSize.title
                , onPress =
                    case newInputError of
                        Nothing ->
                            Just AddPendingCompoundChar

                        Just _ ->
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


titleText : FontSize -> String -> E.Element Msg
titleText fontSize text =
    E.el [ Font.size fontSize.title ] <|
        E.text text


editorSidePanel : Model -> E.Element Msg
editorSidePanel ({ palette, spacing, fontSize } as model) =
    E.column
        [ E.spacing spacing.medium
        , E.width E.fill
        , E.height E.fill
        , E.htmlAttribute <| Html.Attributes.style "user-select" "none"
        ]
        [ strokeWidthPreference model
        , isSnapToGridPreference model
        , isReferenceShownPreference model
        , charExplaination model
        ]


charExplaination : Model -> E.Element Msg
charExplaination ({ fontSize, spacing, selectedChar, charExplainations } as model) =
    let
        explaination =
            Maybe.withDefault emptyExplaination <|
                Dict.get (unboxChar selectedChar) charExplainations
        hasReferenceImage =
            not <| String.isEmpty explaination.referenceImage.image
    in
    E.column
        [ E.paddingEach { top = spacing.large, bottom = 0, left = 0, right = 0 }
        , E.spacing spacing.medium
        , E.width <| E.px <| fontSize.medium * 15
        ]
        [ Input.multiline
            [ E.width E.fill
            , Font.alignLeft
            ]
            { onChange =
                UpdateExplainationNote
            , text =
                explaination.note
            , placeholder =
                Nothing
            , label =
                Input.labelAbove
                    [ E.alignLeft
                    , E.paddingEach { top = 0, bottom = spacing.small, left = 0, right = 0 }
                    ]
                    (E.text "Explaination")
            , spellcheck =
                False
            }
        , E.row
                [ E.spacing spacing.small
                , E.width E.fill
                ]
                [ E.text "Reference image"
                , if hasReferenceImage then
                    iconButton
                    { icon =
                        FeatherIcons.refreshCw
                    , size =
                        fontSize.title
                    , onPress =
                        Just UploadReferenceImage
                    }
                else
                 iconButton
                    { icon =
                        FeatherIcons.upload
                    , size =
                        fontSize.title
                    , onPress =
                        Just UploadReferenceImage
                    }
                ]
        , if hasReferenceImage then
            E.image
            [ E.width E.fill ]
            { src =
                explaination.referenceImage.image
            , description =
                "Reference image"
            }
        else
            E.none
        ]


isReferenceShownPreference : Model -> E.Element Msg
isReferenceShownPreference { palette, spacing, fontSize, isReferenceCharShown, trs } =
    Input.checkbox
        [ E.spacing spacing.small ]
        { onChange = \_ -> ToggleIsReferenceCharShown
        , icon = checkbox palette fontSize
        , checked = isReferenceCharShown
        , label =
            Input.labelLeft []
                (E.text (Translations.showReference trs))
        }


isSnapToGridPreference : Model -> E.Element Msg
isSnapToGridPreference { palette, spacing, fontSize, isSnapToGrid, trs } =
    Input.checkbox
        [ E.spacing spacing.small ]
        { onChange = \_ -> ToggleIsSnapToGrid
        , icon = checkbox palette fontSize
        , checked = isSnapToGrid
        , label =
            Input.labelLeft []
                (E.text (Translations.snapToGrid trs))
        }


strokeWidthPreference : Model -> E.Element Msg
strokeWidthPreference { palette, spacing, fontSize, strokeWidth, trs } =
    Input.slider
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
                (E.row
                    [ E.spacing spacing.small ]
                    [ E.text <| Translations.strokeWidth trs
                    , E.text <| String.fromInt (round strokeWidth)
                    ]
                )
        , min = minStrokeWidth
        , max = maxStrokeWidth
        , step = Just 1
        , value = strokeWidth
        , thumb = sliderThumb palette fontSize
        }


sliderThumb : Palette -> FontSize -> Input.Thumb
sliderThumb palette fontSize =
    Input.thumb
        [ E.width (E.px fontSize.thumb)
        , E.height (E.px fontSize.thumb)
        , Border.rounded (round <| toFloat fontSize.thumb / 2)
        , Border.width 2
        , Border.color palette.darkFg
        , Background.color palette.white
        ]


checkbox : Palette -> FontSize -> Bool -> E.Element msg
checkbox palette fontSize checked =
    E.el
        [ E.width (E.px fontSize.thumb)
        , E.height (E.px fontSize.thumb)
        , Font.color palette.white
        , E.centerY
        , Font.size 9
        , Font.center
        , Border.rounded 3
        , Border.color palette.darkFg
        , Background.color <|
            if checked then
                palette.darkFg

            else
                palette.white
        , Border.width <|
            if checked then
                0

            else
                2
        , E.inFront
            (if checked then
                E.html
                    (FeatherIcons.check
                        |> FeatherIcons.withSize (toFloat fontSize.thumb)
                        |> FeatherIcons.toHtml []
                    )

             else
                E.none
            )
        ]
        E.none


radioOption : E.Color -> FontSize -> E.Element msg -> Input.OptionState -> E.Element msg
radioOption borderColor fontSize optionLabel status =
    let
        radius =
            round <| toFloat fontSize.thumb / 2
    in
    E.row
        [ E.spacing 10
        , E.alignLeft
        , E.width E.shrink
        ]
        [ E.el
            [ E.width (E.px fontSize.thumb)
            , E.height (E.px fontSize.thumb)
            , Border.rounded radius
            , Border.width <|
                case status of
                    Input.Idle ->
                        2

                    Input.Focused ->
                        2

                    Input.Selected ->
                        radius
            , Border.color borderColor
            ]
            E.none
        , E.el [ E.width E.fill ] optionLabel
        ]


editor : Model -> E.Element Msg
editor ({ activeComponentIndex, selectedChar, chars, simpleCharSvgs, boxUnits, borderUnits, unitSize, strokeWidth, isAspectRatioLocked, isSnapToGrid, palette, spacing, fontSize } as model) =
    E.column
        []
        [ E.row
            [ E.width E.fill
            , E.paddingEach { top = 0, bottom = spacing.large, left = 0, right = 0 }
            ]
            [ E.el [ E.alignLeft ] <|
                iconButton
                    { icon =
                        FeatherIcons.chevronLeft
                    , size =
                        fontSize.title
                    , onPress =
                        Just <| UpdateMode BrowseMode
                    }
            , E.el [ E.centerX, Font.size fontSize.title, Font.bold ]
                (E.text <|
                    String.fromChar <|
                        unboxChar selectedChar
                )
            , if isMyCharType SimpleCharType (myCharFromChar chars (unboxChar selectedChar)) then
                E.none

              else
                E.el [ E.alignRight ] <|
                    iconButton
                        { icon =
                            FeatherIcons.plus
                        , size =
                            fontSize.title
                        , onPress =
                            Just <| RequestAddComponentToSelectedChar
                        }
            ]
        , E.el
            [ E.inFront <|
                case selectedChar of
                    Just char ->
                        E.html <|
                            renderChar
                                model
                                { isThumbnail = False }
                                (myCharFromChar chars char)

                    Nothing ->
                        E.none
            ]
            (E.html <| gridBackground model)
        ]


unboxChar : Maybe Char -> Char
unboxChar =
    Maybe.withDefault '?'


isCharPartOfMyChar : Dict Char MyChar -> Char -> MyChar -> Bool
isCharPartOfMyChar chars char myChar =
    case myChar of
        SimpleChar ref ->
            ref.char == char

        CompoundChar ref components ->
            (ref.char == char)
                || List.any
                    (\component ->
                        isCharPartOfMyChar chars char (myCharFromMyCharRef chars component)
                    )
                    components


gridBackground : Model -> Svg Msg
gridBackground { boxUnits, borderUnits, unitSize, palette, selectedChar, isReferenceCharShown } =
    let
        boxSize =
            toFloat boxUnits * unitSize

        minBorderSize =
            minBorderUnits * unitSize

        outerBoxSize =
            boxSize + 2 * minBorderSize

        borderSize =
            borderUnits * unitSize

        scaledBoxSize =
            outerBoxSize - 2 * borderSize

        scaledUnitSize =
            scaledBoxSize / boxSize * unitSize

        strokeWidth =
            { normal =
                2
            , thick =
                3
            }
    in
    Svg.svg
        [ SvgAttributes.width <| SvgTypes.px <| outerBoxSize
        , SvgAttributes.height <| SvgTypes.px <| outerBoxSize
        , SvgAttributes.stroke <| SvgTypes.Paint <| Color.lightBlue
        ]
    <|
        [ Svg.rect
            [ SvgAttributes.width <| SvgTypes.Percent 100
            , SvgAttributes.height <| SvgTypes.Percent 100
            , SvgAttributes.fill <| SvgTypes.Paint <| toColor palette.white
            , SvgAttributes.pointerEvents "fill"
            , TypedSvg.Events.onClick <| SetActiveComponent Nothing
            ]
            []
        , if isReferenceCharShown then
            Svg.text_
                [ SvgAttributes.x <| SvgTypes.px <| 0
                , SvgAttributes.y <| SvgTypes.px <| boxSize - unitSize / 2
                , SvgAttributes.fontSize (SvgTypes.px <| outerBoxSize)
                , SvgAttributes.fill <|
                    SvgTypes.Paint <|
                        Color.Manipulate.fadeOut 0.7 <|
                            toColor palette.black
                , SvgAttributes.pointerEvents "none"
                , Html.Attributes.style "user-select" "none"
                ]
                [ TypedSvg.Core.text <| String.fromChar <| unboxChar selectedChar
                ]

          else
            Svg.g [] []
        , gridOutline
            { x = 0
            , y = 0
            , strokeWidth =
                strokeWidth.thick
            , size =
                outerBoxSize
            }
        , gridOutline
            { x = borderSize
            , y = borderSize
            , strokeWidth =
                strokeWidth.thick
            , size =
                scaledBoxSize
            }
        , Svg.svg
            [ SvgAttributes.x <| SvgTypes.px <| borderSize
            , SvgAttributes.y <| SvgTypes.px <| borderSize
            , SvgAttributes.width <| SvgTypes.px <| scaledBoxSize
            , SvgAttributes.height <| SvgTypes.px <| scaledBoxSize
            ]
          <|
            List.map
                (\units ->
                    Svg.g
                        (if units == (round <| toFloat boxUnits / 2) then
                            [ SvgAttributes.strokeWidth <| SvgTypes.px strokeWidth.thick ]

                         else
                            []
                        )
                        [ Svg.line
                            [ SvgAttributes.x1 <| SvgTypes.px <| toFloat units * scaledUnitSize
                            , SvgAttributes.y1 <| SvgTypes.px <| 0
                            , SvgAttributes.x2 <| SvgTypes.px <| toFloat units * scaledUnitSize
                            , SvgAttributes.y2 <| SvgTypes.px <| toFloat boxUnits * scaledUnitSize
                            ]
                            []
                        , Svg.line
                            [ SvgAttributes.x1 <| SvgTypes.px <| 0
                            , SvgAttributes.y1 <| SvgTypes.px <| toFloat units * scaledUnitSize
                            , SvgAttributes.x2 <| SvgTypes.px <| toFloat boxUnits * scaledUnitSize
                            , SvgAttributes.y2 <| SvgTypes.px <| toFloat units * scaledUnitSize
                            ]
                            []
                        ]
                )
                (List.range 0 boxUnits)
        ]


gridOutline : { x : Float, y : Float, strokeWidth : Float, size : Float } -> Svg Msg
gridOutline { x, y, strokeWidth, size } =
    Svg.rect
        [ SvgAttributes.width <| SvgTypes.px <| size
        , SvgAttributes.height <| SvgTypes.px <| size
        , SvgAttributes.x <| SvgTypes.px <| x
        , SvgAttributes.y <| SvgTypes.px <| y
        , SvgAttributes.fill <| SvgTypes.PaintNone
        , SvgAttributes.strokeWidth <| SvgTypes.px strokeWidth
        ]
        []


charPanels : Model -> E.Element Msg
charPanels ({ spacing, device } as model) =
    (case device.orientation of
        E.Portrait ->
            E.column

        E.Landscape ->
            E.row
    )
        [ E.width E.fill
        , E.height E.fill
        , E.spacing spacing.large
        ]
        [ charPanel SimpleCharType model
        , charPanel CompoundCharType model
        ]


charPanel : MyCharType -> Model -> E.Element Msg
charPanel myCharType ({ trs, boxUnits, thumbnailUnitSize, palette, spacing, fontSize } as model) =
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
        , E.width E.fill
        ]
        [ E.row
            [ E.spacing spacing.small
            , Font.size fontSize.title
            , E.width E.fill
            , E.htmlAttribute <| Html.Attributes.style "user-select" "none"
            ]
            [ E.text <|
                (String.Extra.toSentenceCase <| stringFromMyCharType trs myCharType)
            , E.el [ Font.color palette.lightFg ] <|
                iconButton
                    { icon =
                        FeatherIcons.plusCircle
                    , size =
                        fontSize.title
                    , onPress =
                        Just <| AddChar myCharType
                    }
            , E.el [ Font.color palette.danger ] <|
                iconButton
                    { icon =
                        FeatherIcons.trash2
                    , size =
                        fontSize.title
                    , onPress =
                        Just <| RequestClearChars myCharType
                    }
            ]
        , E.wrappedRow
            [ E.width E.fill
            , E.height E.fill
            , E.htmlAttribute <| Html.Attributes.id "charPanel-wrappedRow"
            , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
            , E.htmlAttribute <| Html.Attributes.style "overflow-x" "hidden"
            , E.spacing spacing.medium
            ]
          <|
            cards
        ]


stringFromMyCharType : I18Next.Translations -> MyCharType -> String
stringFromMyCharType trs myCharType =
    case myCharType of
        SimpleCharType ->
            Translations.CharType.simple trs

        CompoundCharType ->
            Translations.CharType.compound trs


charCard : Model -> MyChar -> E.Element Msg
charCard ({ chars, activeComponentIndex, unitSize, thumbnailUnitSize, boxUnits, borderUnits, strokeWidth, simpleCharSvgs, selectedChar, isAspectRatioLocked, isSnapToGrid, palette, spacing, fontSize } as model) myChar =
    let
        char =
            charFromMyChar myChar

        outerBoxSize =
            (toFloat boxUnits + 2 * minBorderUnits) * thumbnailUnitSize
    in
    E.column
        ([ E.width <| E.px <| round outerBoxSize
         , Background.color palette.lightBg
         , Border.rounded spacing.medium
         , Events.onClick <| SelectChar myChar
         , E.pointer
         ]
            ++ (case selectedChar of
                    Just selected ->
                        if selected == char then
                            highlightBorder
                                { color = palette.lightFg
                                , width = 0
                                , spacing = spacing
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
                [ E.alignLeft
                , E.htmlAttribute <| Html.Attributes.style "user-select" "none"
                ]
                (E.text <|
                    String.fromChar <|
                        char
                )
            , if selectedChar == Just char then
                E.row
                    [ E.alignRight
                    , E.spacing spacing.tiny
                    ]
                    [ E.el
                        [ Font.color palette.lightFg
                        ]
                        (iconButton
                            { icon =
                                FeatherIcons.edit
                            , size =
                                fontSize.large
                            , onPress =
                                Just <| UpdateMode EditMode
                            }
                        )
                    , E.el
                        [ Font.color palette.danger
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
                    ]

              else
                E.none
            ]
        , E.html <|
            renderChar
                { model
                    | unitSize = thumbnailUnitSize
                    , strokeWidth = strokeWidth * thumbnailUnitSize / unitSize
                    , activeComponentIndex =
                        Maybe.andThen
                            (\selected ->
                                if selected == char then
                                    activeComponentIndex

                                else
                                    Nothing
                            )
                            selectedChar
                }
                { isThumbnail = True }
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


renderChar : Model -> { isThumbnail : Bool } -> MyChar -> Svg Msg
renderChar ({ unitSize, boxUnits, borderUnits, strokeWidth, chars, simpleCharSvgs, activeComponentIndex, isAspectRatioLocked, isSnapToGrid } as model) { isThumbnail } myChar =
    let
        boxSize =
            toFloat boxUnits * unitSize

        minBorderSize =
            minBorderUnits * unitSize

        outerBoxSize =
            boxSize + 2 * minBorderSize

        borderSize =
            borderUnits * unitSize

        scaledBoxSize =
            outerBoxSize - 2 * borderSize

        scaledUnitSize =
            scaledBoxSize / boxSize * unitSize

        offset =
            borderUnits * unitSize

        charClassName =
            "char-with-size-" ++ (String.fromInt <| round strokeWidth)
    in
    Svg.svg
        ([ SvgAttributes.width <| SvgTypes.px <| outerBoxSize
         , SvgAttributes.height <| SvgTypes.px <| outerBoxSize
         , Html.Attributes.style "pointer-events" "none"
         ]
            ++ (if isThumbnail then
                    [ SvgAttributes.id ("char-" ++ String.fromChar (charFromMyChar myChar)) ]

                else
                    []
               )
        )
        [ Svg.defs []
            [ Svg.style []
                [ TypedSvg.Core.text <|
                    "."
                        ++ charClassName
                        ++ """ * {
                        fill: none;
                        stroke: #000;
                        stroke-linecap: round !important;
                        stroke-miterlimit: 10;
                        stroke-width: """
                        ++ String.fromFloat strokeWidth
                        ++ """ !important;
                        stroke-linejoin: round !important;
                        vector-effect: non-scaling-stroke;
                    }
                    svg {
                        overflow: visible;
                    }
                    """
                ]
            ]
        , Svg.svg
            [ SvgAttributes.width <| SvgTypes.px scaledBoxSize
            , SvgAttributes.height <| SvgTypes.px scaledBoxSize
            , SvgAttributes.x <| SvgTypes.px offset
            , SvgAttributes.y <| SvgTypes.px offset
            , Html.Attributes.style "pointer-events" "none"
            ]
            [ renderCharHelper
                { model
                    | unitSize = scaledUnitSize
                }
                { charClassName = charClassName
                , index = -1
                , isThumbnail = isThumbnail
                , tightDimension =
                    { position = Vector2.vec2 0 0, dimension = Vector2.vec2 100 100 }
                , parentMyCharType =
                    myCharTypeFromMyChar myChar
                }
                0
                myChar
            ]
        ]


renderCharHelper :
    Model
    ->
        { charClassName : String
        , index : Int
        , isThumbnail : Bool
        , tightDimension : { position : Vec2, dimension : Vec2 }
        , parentMyCharType : MyCharType
        }
    -> Int
    -> MyChar
    -> Svg Msg
renderCharHelper ({ unitSize, boxUnits, chars, simpleCharSvgs, activeComponentIndex, isAspectRatioLocked, isSnapToGrid, palette, fontSize } as model) { charClassName, index, isThumbnail, tightDimension, parentMyCharType } level myChar =
    let
        char =
            charFromMyChar myChar

        levelwiseIndex =
            index + (level - 1) * 10

        isDraggable =
            not (isThumbnail || level > 1)

        constraint charType dimension position contents =
            let
                tightPosition =
                    Vector2.sub position tightDimension.position

                xFactor =
                    100 / Vector2.getX tightDimension.dimension

                yFactor =
                    100 / Vector2.getY tightDimension.dimension

                styledContents =
                    case charType of
                        SimpleCharType ->
                            [ Svg.g
                                [ SvgAttributes.class [ charClassName ] ]
                                contents
                            ]

                        CompoundCharType ->
                            contents

                unitPercent =
                    100 / toFloat boxUnits
            in
            Svg.svg
                ([ SvgAttributes.x <| SvgTypes.Percent <| xFactor * Vector2.getX tightPosition
                 , SvgAttributes.y <| SvgTypes.Percent <| yFactor * Vector2.getY tightPosition
                 , SvgAttributes.width <| SvgTypes.Percent <| xFactor * Vector2.getX dimension
                 , SvgAttributes.height <| SvgTypes.Percent <| yFactor * Vector2.getY dimension
                 , Html.Attributes.style "pointer-events" "auto"
                 ]
                    ++ dragTrigger isDraggable
                        { index = levelwiseIndex
                        , scale = NoScale
                        }
                )
            <|
                if Just levelwiseIndex == activeComponentIndex && not isThumbnail then
                    styledContents
                        ++ [ activeComponentButtons parentMyCharType model ]
                        ++ (if Vector2.getX dimension < unitPercent then
                                [ Svg.line
                                    [ SvgAttributes.x1 <| SvgTypes.px <| 0
                                    , SvgAttributes.y1 <| SvgTypes.px <| 0
                                    , SvgAttributes.x2 <| SvgTypes.px <| 0
                                    , SvgAttributes.y2 <| SvgTypes.Percent 100
                                    , SvgAttributes.strokeWidth <| SvgTypes.px 2
                                    , SvgAttributes.stroke <| SvgTypes.Paint <| toColor palette.darkFg
                                    ]
                                    []
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleTop
                                    }
                                    0
                                    0
                                    fontSize.thumb
                                    isDraggable
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleBottom
                                    }
                                    0
                                    100
                                    fontSize.thumb
                                    isDraggable
                                ]

                            else if Vector2.getY dimension < unitPercent then
                                [ Svg.line
                                    [ SvgAttributes.x1 <| SvgTypes.px <| 0
                                    , SvgAttributes.y1 <| SvgTypes.px <| 0
                                    , SvgAttributes.x2 <| SvgTypes.Percent 100
                                    , SvgAttributes.y2 <| SvgTypes.px <| 0
                                    , SvgAttributes.strokeWidth <| SvgTypes.px 2
                                    , SvgAttributes.stroke <| SvgTypes.Paint <| toColor palette.darkFg
                                    ]
                                    []
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleLeft
                                    }
                                    0
                                    0
                                    fontSize.thumb
                                    isDraggable
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleRight
                                    }
                                    100
                                    0
                                    fontSize.thumb
                                    isDraggable
                                ]

                            else
                                [ Svg.rect
                                    [ SvgAttributes.width <| SvgTypes.Percent 100
                                    , SvgAttributes.height <| SvgTypes.Percent 100
                                    , SvgAttributes.fill <| SvgTypes.PaintNone
                                    , SvgAttributes.strokeWidth <| SvgTypes.px 2
                                    , SvgAttributes.stroke <| SvgTypes.Paint <| toColor palette.darkFg
                                    ]
                                    []
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleTopLeft
                                    }
                                    0
                                    0
                                    fontSize.thumb
                                    isDraggable
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleTopRight
                                    }
                                    100
                                    0
                                    fontSize.thumb
                                    isDraggable
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleBottomLeft
                                    }
                                    0
                                    100
                                    fontSize.thumb
                                    isDraggable
                                , scaleHandle
                                    palette
                                    { index = levelwiseIndex
                                    , scale = ScaleBottomRight
                                    }
                                    100
                                    100
                                    fontSize.thumb
                                    isDraggable
                                ]
                           )

                else
                    styledContents
    in
    case myChar of
        SimpleChar { dimension, position } ->
            case Dict.get char simpleCharSvgs of
                Just ( svg, _ ) ->
                    constraint SimpleCharType dimension position [ svg ]

                Nothing ->
                    -- impossible
                    TypedSvg.Core.text <| "Error rendering " ++ String.fromChar char

        CompoundChar ({ dimension, position } as compoundChar) components ->
            constraint CompoundCharType dimension position <|
                List.indexedMap
                    (\componentIndex ->
                        (\component ->
                            renderCharHelper
                                model
                                { charClassName = charClassName
                                , index = componentIndex
                                , isThumbnail = isThumbnail
                                , tightDimension =
                                    if level >= 1 then
                                        calculateMyCharDimension myChar

                                    else
                                        { position = Vector2.vec2 0 0, dimension = Vector2.vec2 100 100 }
                                , parentMyCharType = parentMyCharType
                                }
                                (level + 1)
                                component
                        )
                            << myCharFromMyCharRef chars
                    )
                    components


activeComponentButtons : MyCharType -> Model -> Svg Msg
activeComponentButtons charType ({ palette } as model) =
    Svg.svg
        [ SvgAttributes.x <| SvgTypes.px -35
        , SvgAttributes.y <| SvgTypes.percent 50
        , SvgAttributes.strokeWidth <| SvgTypes.px 2
        , SvgAttributes.color <| toColor palette.white
        ]
    <|
        case charType of
            SimpleCharType ->
                [ aspectRatioLockButton model
                ]

            CompoundCharType ->
                [ aspectRatioLockButton model
                , copyActiveComponentButton model
                , deleteActiveComponentButton model
                ]


aspectRatioLockButton : Model -> Svg Msg
aspectRatioLockButton { isSnapToGrid, isAspectRatioLocked, palette, spacing, fontSize } =
    if isSnapToGrid then
        Svg.g [] []

    else
        activeComponentButton fontSize
            (-1.5 * toFloat fontSize.title - toFloat spacing.small)
            (if isAspectRatioLocked then
                FeatherIcons.lock

             else
                FeatherIcons.unlock
            )
            palette.darkFg
            ToggleIsAspectRatioLocked


copyActiveComponentButton : Model -> Svg Msg
copyActiveComponentButton { palette, spacing, fontSize } =
    activeComponentButton fontSize (-0.5 * toFloat fontSize.title) FeatherIcons.copy palette.lightFg CopyActiveComponent


deleteActiveComponentButton : Model -> Svg Msg
deleteActiveComponentButton { palette, spacing, fontSize } =
    activeComponentButton fontSize (0.5 * toFloat fontSize.title + toFloat spacing.small) FeatherIcons.trash2 palette.danger DeleteActiveComponent


activeComponentButton : FontSize -> Float -> FeatherIcons.Icon -> E.Color -> Msg -> Svg Msg
activeComponentButton fontSize y icon backgroundColor onClick =
    Svg.svg
        [ SvgAttributes.x <| SvgTypes.px 0
        , SvgAttributes.y <| SvgTypes.px <| y
        , SvgAttributes.width <| SvgTypes.px <| toFloat fontSize.title
        , SvgAttributes.height <| SvgTypes.px <| toFloat fontSize.title
        ]
        [ Svg.rect
            [ SvgAttributes.width <| SvgTypes.percent 100
            , SvgAttributes.height <| SvgTypes.percent 100
            , SvgAttributes.rx <| SvgTypes.percent 20
            , SvgAttributes.ry <| SvgTypes.percent 20
            , SvgAttributes.fill <| SvgTypes.Paint <| toColor backgroundColor
            , TypedSvg.Events.onClick onClick
            , Html.Events.Extra.Touch.onStart (\_ -> onClick)
            ]
            []
        , Svg.svg
            [ SvgAttributes.x <| SvgTypes.Percent 7.5
            , SvgAttributes.y <| SvgTypes.Percent 7.5
            , SvgAttributes.pointerEvents "none"
            ]
            [ icon
                |> FeatherIcons.withSize 85
                |> FeatherIcons.withSizeUnit "%"
                |> FeatherIcons.toHtml []
            ]
        ]


scaleHandle : Palette -> DragData -> Float -> Float -> Int -> Bool -> Svg Msg
scaleHandle palette data x y size isDraggable =
    Svg.circle
        ([ SvgAttributes.cx (SvgTypes.percent x)
         , SvgAttributes.cy (SvgTypes.percent y)
         , SvgAttributes.r (SvgTypes.px <| toFloat size / 2)
         , SvgAttributes.fill <| SvgTypes.Paint <| toColor palette.darkFg
         ]
            ++ dragTrigger isDraggable data
        )
        []


dragTrigger : Bool -> DragData -> List (Html.Attribute Msg)
dragTrigger isDraggable data =
    if isDraggable then
        Draggable.mouseTrigger data DragMsg
            :: Draggable.touchTriggers data DragMsg

    else
        []


myCharFromChar : Dict Char MyChar -> Char -> MyChar
myCharFromChar chars char =
    -- impossible
    Maybe.withDefault emptyMyChar <|
        Dict.get char chars


myCharFromMyCharRef : Dict Char MyChar -> MyCharRef -> MyChar
myCharFromMyCharRef chars ref =
    let
        myChar =
            myCharFromChar chars ref.char

        updateAttributes r =
            { r
                | dimension = ref.dimension
                , position = ref.position
            }
    in
    case myChar of
        SimpleChar r ->
            SimpleChar (updateAttributes r)

        CompoundChar r components ->
            CompoundChar (updateAttributes r) components


iconButton : { icon : FeatherIcons.Icon, size : Int, onPress : Maybe Msg } -> E.Element Msg
iconButton { icon, size, onPress } =
    Input.button
        []
        { label =
            E.html
                (icon
                    |> FeatherIcons.withSize (toFloat size)
                    |> FeatherIcons.toHtml []
                )
        , onPress =
            onPress
        }


textButton : Model -> String -> Maybe Msg -> E.Element Msg
textButton { palette, spacing, fontSize } text onPress =
    Input.button
        [ Border.width 3
        , Border.color palette.black
        , Border.rounded fontSize.small
        , E.padding spacing.small
        ]
        { label =
            E.text text
        , onPress =
            onPress
        }


highlightBorder :
    { color : E.Color
    , width : Int
    , spacing : Spacing
    , glowWidth : Int
    , style : E.Attribute Msg
    }
    -> List (E.Attribute Msg)
highlightBorder { color, width, spacing, glowWidth, style } =
    [ Border.rounded spacing.medium
    , Border.color color
    , Border.width width
    , style
    , Border.glow color (toFloat glowWidth)
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
        , gotSavedSimpleCharsPort GotSavedSimpleChars
        , gotNewSimpleCharsPort GotNewSimpleChars
        , Time.every 1000 (\_ -> SaveModel ())
        , Browser.Events.onResize UpdateDevice
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


main : Program Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
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
