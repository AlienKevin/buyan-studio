port module Main exposing (..)

import Array exposing (Array)
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
import String.Graphemes
import SvgParser
import Task
import Time
import Translations
import Translations.CharType
import Translations.PeriodName
import Translations.ScriptName
import Translations.StageName
import Translations.OrientationType
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types as SvgTypes
import Maybe


port addSimpleCharsPort : () -> Cmd msg


port gotSavedSimpleCharsPort : (Encode.Value -> msg) -> Sub msg


port gotNewSimpleCharsPort : (Encode.Value -> msg) -> Sub msg


port uploadSimpleCharPort : String -> Cmd msg


port loadedSimpleCharPort : (Encode.Value -> msg) -> Sub msg


port downloadCharsPort : List String -> Cmd msg


port saveModelPort : Value -> Cmd msg


port getModelPort : (Value -> msg) -> Sub msg


port deleteSimpleCharPort : String -> Cmd msg


port clearSimpleCharsPort : () -> Cmd msg


port backupAsLocalFilePort : Value -> Cmd msg


port updateBackupLocationPort : Value -> Cmd msg


port succeededInBackupPort : (() -> msg) -> Sub msg


port uploadBackupPort : () -> Cmd msg



---- MODEL ----


type alias Model =
    { mode : Mode
    , chars : Dict Grapheme MyChar
    , charExplainations : Dict Grapheme Explaination
    , selectedChar : Maybe Grapheme
    , selectedCharHistory : History
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
    , previewParagraph : String
    , previewOrientation : TextOrientation
    , previewFontSize : Int
    , trs : I18Next.Translations
    , language : Language
    , device : E.Device
    , palette :
        Palette
    , spacing :
        Spacing
    , fontSize :
        FontSize
    , isBackingUp : Bool
    }


type alias History =
    { snapshots : Array MyChar
    , currentIndex : Int
    }


emptyHistory : History
emptyHistory =
    { snapshots = Array.empty
    , currentIndex = 0
    }


type TextOrientation
    = Horizontal
    | Vertical


type alias Grapheme =
    String


type alias Explaination =
    { note : String
    , referenceImage : Maybe ReferenceImage
    }


type alias ReferenceImage =
    { image : DataUrl
    , origin : String
    , time :
        Maybe
            { period : Period
            , stage : Maybe Stage
            }
    , script : Script
    , url : String
    }


emptyReferenceImage : ReferenceImage
emptyReferenceImage =
    { image = ""
    , origin = ""
    , time = Nothing
    , script = Seal
    , url = ""
    }


type alias DataUrl =
    String


type Period
    = Shang
    | WesternZhou
    | SpringAndAutumn
    | WarringStates


type Stage
    = Early
    | Middle
    | Late


type Script
    = Oracle
    | Bronze
    | Seal


emptyExplaination : Explaination
emptyExplaination =
    { note = ""
    , referenceImage = Nothing
    }


type Mode
    = BrowseMode
    | EditMode


type InputError
    = InvalidInputLength Int
    | CharacterNotFound
    | CharacterAlreadyExists
    | ContainsSelfReference


type alias Palette =
    { lightBg : E.Color
    , lightFg : E.Color
    , darkFg : E.Color
    , danger : E.Color
    , disabled : E.Color
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
    { chars : Dict Grapheme MyChar
    , charExplainations : Dict Grapheme Explaination
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
    { char : Grapheme
    , dimension : Vec2
    , position : Vec2
    , mirror : Mirror
    }


type alias Mirror =
    { x : Bool, y : Bool }


type MirrorDirection
    = MirrorDirectionX
    | MirrorDirectionY


emptyMirror : Mirror
emptyMirror =
    { x = False, y = False }


emptyMyChar : MyChar
emptyMyChar =
    SimpleChar
        { char = "?"
        , dimension = Vector2.vec2 0 0
        , position = Vector2.vec2 0 0
        , mirror = emptyMirror
        }


type MyCharType
    = SimpleCharType
    | CompoundCharType


type alias SimpleCharSvgs =
    Dict Grapheme SimpleCharSvg


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


minPreviewFontSize : Float
minPreviewFontSize =
    30


maxPreviewFontSize : Float
maxPreviewFontSize =
    200


minBorderUnits : Float
minBorderUnits =
    2


maxBorderUnits : Float
maxBorderUnits =
    3.5


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        fontSize =
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
        
        model =
            { mode = BrowseMode
            , chars = Dict.empty
            , charExplainations = Dict.empty
            , selectedChar = Nothing
            , selectedCharHistory = emptyHistory
            , simpleCharSvgs = Dict.empty
            , boxUnits = 36
            , borderUnits = 3
            , unitSize = 16
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
            , previewParagraph = ""
            , previewOrientation = Vertical
            , previewFontSize = fontSize.title * 2
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
                , disabled =
                    toElmUiColor Color.grey
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
                fontSize
            , isBackingUp =
                False
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

        Err _ ->
            ( model, Cmd.none )



---- UPDATE ----


type Msg
    = AddChar MyCharType
    | GotNewSimpleChars Value
    | GotSavedSimpleChars Value
    | UploadSimpleChar
    | LoadedSimpleChar Value
    | SelectChar Grapheme
    | UndoSelectedCharHistory
    | RedoSelectedCharHistory
    | EditChar Grapheme
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
    | EndDragging
    | DragMsg (Draggable.Msg DragData)
    | SetActiveComponent (Maybe Int)
    | MirrorActiveComponent MirrorDirection
    | CopyActiveComponent
    | DeleteActiveComponent
    | GotModel Value
    | SaveModel ()
    | UpdateStrokeWidth Float
    | ToggleIsAspectRatioLocked
    | PreviewInParagraph
    | UpdatePreviewParagraph String
    | UpdatePreviewOrientation TextOrientation
    | UpdatePreviewFontSize Int
    | ToggleIsSnapToGrid
    | ToggleIsReferenceCharShown
    | DownloadSelectedChar
    | DownloadCharsForPreview
    | UpdateLanguage Language
    | GotTranslations (Result Http.Error I18Next.Translations)
    | UpdateDevice Int Int
    | UpdateMode Mode
    | ShowAppPreferences
    | UpdateExplainationNote String
    | UploadReferenceImage
    | SelectedReferenceImage File
    | LoadedReferenceImage DataUrl
    | UpdateReferenceImageOrigin String
    | UpdateReferenceImagePeriod Period
    | UpdateReferenceImageStage Stage
    | UpdateReferenceImageScript Script
    | UpdateReferenceImageUrl String
    | UpdateBackupLocation
    | SucceededInBackup
    | UploadBackup


type alias DragData =
    { index : Int
    , scale : Scale
    }


dragConfig : Draggable.Config DragData Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vector2.vec2 dx dy |> OnDragBy)
        , Draggable.Events.onDragStart StartDragging
        , Draggable.Events.onDragEnd EndDragging
        , Draggable.Events.onClick (\{ index } -> SetActiveComponent (Just index))
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddChar myCharType ->
            addChar myCharType model

        GotNewSimpleChars svgsJson ->
            gotNewSimpleChars svgsJson model

        GotSavedSimpleChars svgsJson ->
            gotSavedSimpleChars svgsJson model

        UploadSimpleChar ->
            uploadSimpleChar model

        LoadedSimpleChar svgJson ->
            loadedSimpleChar svgJson model

        SelectChar char ->
            selectChar char model
        
        UndoSelectedCharHistory ->
            undoSelectedCharHistory model
        
        RedoSelectedCharHistory ->
            redoSelectedCharHistory model
        
        EditChar char ->
            editChar char model

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

        EndDragging ->
            endDragging model

        SetActiveComponent index ->
            setActiveComponent index model

        MirrorActiveComponent mirrorDirection ->
            mirrorActiveComponent mirrorDirection model

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

        UpdatePreviewParagraph paragraph ->
            updatePreviewParagraph paragraph model

        UpdatePreviewOrientation newOrientation ->
            updatePreviewOrientation newOrientation model
        
        UpdatePreviewFontSize newSize ->
            updatePreviewFontSize newSize model

        ToggleIsSnapToGrid ->
            toggleIsSnapToGrid model

        ToggleIsReferenceCharShown ->
            toggleIsReferenceCharShown model

        DownloadSelectedChar ->
            downloadSelectedChar model
        
        DownloadCharsForPreview ->
            downloadCharsForPreview model

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

        UpdateReferenceImageOrigin origin ->
            updateReferenceImageOrigin origin model

        UpdateReferenceImagePeriod period ->
            updateReferenceImagePeriod period model

        UpdateReferenceImageStage stage ->
            updateReferenceImageStage stage model

        UpdateReferenceImageScript script ->
            updateReferenceImageScript script model

        UpdateReferenceImageUrl url ->
            updateReferenceImageUrl url model

        UpdateBackupLocation ->
            updateBackupLocation model

        SucceededInBackup ->
            succeededInBackup model

        UploadBackup ->
            uploadBackup model


undoSelectedCharHistory : Model -> (Model, Cmd Msg)
undoSelectedCharHistory model =
    updateSelectedCharHistory
        (\index ->
            if index == 0 then
                index
            else
                index - 1
        )
        model


redoSelectedCharHistory : Model -> (Model, Cmd Msg)
redoSelectedCharHistory model =
    let
        lastIndex =
            Array.length model.selectedCharHistory.snapshots - 1
    in
    updateSelectedCharHistory
        (\index ->
            if index == lastIndex then
                index
            else
                index + 1
        )
        model


updateSelectedCharHistory : (Int -> Int) -> Model -> (Model, Cmd Msg)
updateSelectedCharHistory updateCurrentIndex model =
    let
        history =
            model.selectedCharHistory
        
        newCurrentIndex =
            updateCurrentIndex history.currentIndex
        
        selectedChar =
            unboxChar model.selectedChar
        
        currentSnapshot =
            unboxMyChar <| Array.get newCurrentIndex history.snapshots
    in
    ( { model
        | selectedCharHistory =
            { history
                | currentIndex =
                    newCurrentIndex
            }
        , chars =
            Dict.insert
                selectedChar
                currentSnapshot
                model.chars
    }
    , Cmd.none
    )


editChar : Grapheme -> Model -> (Model, Cmd Msg)
editChar char model =
    selectChar char model
    |> Tuple.first
    |> updateMode EditMode
    |> Tuple.first
    |> closePopUp


updatePreviewFontSize : Int -> Model -> ( Model, Cmd Msg )
updatePreviewFontSize newSize model =
    let
        _ = Debug.log "newSize" newSize
    in
    ( { model
        | previewFontSize =
            newSize
    }
    , Cmd.none
    )

updatePreviewOrientation : TextOrientation -> Model -> ( Model, Cmd Msg )
updatePreviewOrientation newOrientation model =
    ( { model
        | previewOrientation =
            newOrientation
    }
    , Cmd.none
    )


uploadBackup : Model -> ( Model, Cmd Msg )
uploadBackup model =
    ( model
    , uploadBackupPort ()
    )


succeededInBackup : Model -> ( Model, Cmd Msg )
succeededInBackup model =
    ( { model
        | isBackingUp =
            True
      }
    , Cmd.none
    )


updateBackupLocation : Model -> ( Model, Cmd Msg )
updateBackupLocation model =
    ( model
    , updateBackupLocationPort <| encodeModel model
    )


loadedSimpleChar : Value -> Model -> ( Model, Cmd Msg )
loadedSimpleChar svgJson model =
    ( case Decode.decodeValue decodeSimpleCharSvg svgJson of
        Ok svg ->
            { model
                | simpleCharSvgs =
                    case model.selectedChar of
                        Just c ->
                            Dict.insert
                                c
                                svg
                                model.simpleCharSvgs

                        Nothing ->
                            model.simpleCharSvgs
            }

        Err _ ->
            model
    , Cmd.none
    )


uploadSimpleChar : Model -> ( Model, Cmd Msg )
uploadSimpleChar model =
    ( model
    , uploadSimpleCharPort (unboxChar model.selectedChar)
    )


updateReferenceImageUrl : String -> Model -> ( Model, Cmd Msg )
updateReferenceImageUrl url model =
    updateReferenceImage
        (Maybe.map
            (\referenceImage ->
                { referenceImage
                    | url =
                        url
                }
            )
        )
        model


updateReferenceImageScript : Script -> Model -> ( Model, Cmd Msg )
updateReferenceImageScript script model =
    updateReferenceImage
        (Maybe.map
            (\referenceImage ->
                { referenceImage
                    | script =
                        script
                }
            )
        )
        model


updateReferenceImageStage : Stage -> Model -> ( Model, Cmd Msg )
updateReferenceImageStage stage model =
    updateReferenceImage
        (Maybe.map
            (\referenceImage ->
                { referenceImage
                    | time =
                        case referenceImage.time of
                            Just time ->
                                if time.stage == Just stage then
                                    Just
                                        { time
                                            | stage =
                                                Nothing
                                        }

                                else
                                    Just
                                        { time
                                            | stage =
                                                Just stage
                                        }

                            Nothing ->
                                Just
                                    { stage =
                                        Just stage
                                    , period =
                                        Shang
                                    }
                }
            )
        )
        model


updateReferenceImagePeriod : Period -> Model -> ( Model, Cmd Msg )
updateReferenceImagePeriod period model =
    updateReferenceImage
        (Maybe.map
            (\referenceImage ->
                { referenceImage
                    | time =
                        case referenceImage.time of
                            Just time ->
                                if time.period == period then
                                    Nothing

                                else
                                    Just
                                        { time
                                            | period =
                                                period
                                        }

                            Nothing ->
                                Just
                                    { period =
                                        period
                                    , stage =
                                        Nothing
                                    }
                }
            )
        )
        model


updateReferenceImageOrigin : String -> Model -> ( Model, Cmd Msg )
updateReferenceImageOrigin origin model =
    updateReferenceImage
        (Maybe.map
            (\referenceImage ->
                { referenceImage
                    | origin =
                        origin
                }
            )
        )
        model


updateReferenceImage : (Maybe ReferenceImage -> Maybe ReferenceImage) -> Model -> ( Model, Cmd Msg )
updateReferenceImage func model =
    updateExplaination
        (\e ->
            { e
                | referenceImage =
                    func e.referenceImage
            }
        )
        model


loadedReferenceImage : DataUrl -> Model -> ( Model, Cmd Msg )
loadedReferenceImage dataUrl model =
    updateExplaination
        (\e ->
            { e
                | referenceImage =
                    case e.referenceImage of
                        Just referenceImage ->
                            Just
                                { referenceImage
                                    | image =
                                        dataUrl
                                }

                        Nothing ->
                            Just
                                { emptyReferenceImage
                                    | image =
                                        dataUrl
                                }
            }
        )
        model


selectedReferenceImage : File -> Model -> ( Model, Cmd Msg )
selectedReferenceImage file model =
    ( model
    , Task.perform LoadedReferenceImage (File.toUrl file)
    )


uploadReferenceImage : Model -> ( Model, Cmd Msg )
uploadReferenceImage model =
    ( model
    , File.Select.file [ "image/jpeg", "image/png", "image/webp" ] SelectedReferenceImage
    )


updateExplainationNote : String -> Model -> ( Model, Cmd Msg )
updateExplainationNote note model =
    updateExplaination
        (\e ->
            { e
                | note =
                    note
            }
        )
        model


updateExplaination : (Explaination -> Explaination) -> Model -> ( Model, Cmd Msg )
updateExplaination func model =
    ( { model
        | charExplainations =
            case model.selectedChar of
                Just char ->
                    Dict.update
                        char
                        (Just << func << Maybe.withDefault emptyExplaination)
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
    ( addSnapshot <| { model
        | chars =
            Maybe.map
                (\selectedChar ->
                    Dict.update
                        selectedChar
                        (Maybe.map <|
                            addComponentToMyChar model.chars model.newComponentChar
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
updateMode mode ({ isBackingUp } as model) =
    ( { model
        | mode =
            mode
      }
    , case mode of
        BrowseMode ->
            if isBackingUp then
                backupAsLocalFilePort <| encodeModel model

            else
                Cmd.none

        _ ->
            Cmd.none
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


mirrorActiveComponent : MirrorDirection -> Model -> ( Model, Cmd Msg )
mirrorActiveComponent direction ({ activeComponentIndex } as model) =
    ( { model
        | chars =
            Maybe.map
                (\selectedChar ->
                    Dict.update
                        selectedChar
                        (Maybe.map <|
                            updateMyCharComponents
                                (List.Extra.updateAt
                                    (Maybe.withDefault -1 activeComponentIndex)
                                    (\component ->
                                        let
                                            oldMirror =
                                                component.mirror
                                        in
                                        { component
                                            | mirror =
                                                case direction of
                                                    MirrorDirectionX ->
                                                        { oldMirror
                                                            | x =
                                                                not oldMirror.x
                                                        }

                                                    MirrorDirectionY ->
                                                        { oldMirror
                                                            | y =
                                                                not oldMirror.y
                                                        }
                                        }
                                    )
                                )
                        )
                        model.chars
                )
                model.selectedChar
                |> Maybe.withDefault model.chars
      }
    , Cmd.none
    )


deleteActiveComponent : Model -> ( Model, Cmd Msg )
deleteActiveComponent ({ activeComponentIndex } as model) =
    ( addSnapshot <| { model
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
copyActiveComponent ({ activeComponentIndex, isSnapToGrid, boxUnits } as model) =
    ( addSnapshot <| { model
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
                                                        , mirror = component.mirror
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
                        (\_ myChar ->
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
                        (\_ myChar ->
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
    , downloadCharsPort [ unboxChar model.selectedChar ]
    )


downloadCharsForPreview : Model -> ( Model, Cmd Msg )
downloadCharsForPreview model =
    ( model
    , downloadCharsPort <| String.Graphemes.toList <| model.previewParagraph
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


updatePreviewParagraph : String -> Model -> ( Model, Cmd Msg )
updatePreviewParagraph paragraph model =
    ( { model
        | previewParagraph =
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
            deleteSimpleCharPort char

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
encodeModel { chars, charExplainations, strokeWidth, language } =
    Encode.object
        [ ( "chars", Encode.dict identity encodeMyChar chars )
        , ( "charExplainations", Encode.dict identity encodeExplaination charExplainations )
        , ( "strokeWidth", Encode.float strokeWidth )
        , ( "language", encodeLanguage language )
        ]


encodeExplaination : Explaination -> Value
encodeExplaination { note, referenceImage } =
    Encode.object <|
        ( "note", Encode.string note )
            :: (case referenceImage of
                    Just image ->
                        [ ( "referenceImage", encodeReferenceImage image ) ]

                    Nothing ->
                        []
               )


encodeReferenceImage : ReferenceImage -> Value
encodeReferenceImage { image, origin, time, script, url } =
    Encode.object <|
        [ ( "image", Encode.string image )
        , ( "origin", Encode.string origin )
        ]
            ++ (case time of
                    Just { period, stage } ->
                        [ ( "time"
                          , Encode.object
                                (( "period", encodePeriod period )
                                    :: (case stage of
                                            Just s ->
                                                [ ( "stage", encodeStage s ) ]

                                            Nothing ->
                                                []
                                       )
                                )
                          )
                        ]

                    Nothing ->
                        []
               )
            ++ [ ( "script", encodeScript script )
               , ( "url", Encode.string url )
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


encodeChar : Grapheme -> Value
encodeChar =
    Encode.string


encodePeriod : Period -> Value
encodePeriod period =
    Encode.string <|
        case period of
            Shang ->
                "Shang"

            WesternZhou ->
                "WesternZhou"

            SpringAndAutumn ->
                "SpringAndAutumn"

            WarringStates ->
                "WarringStates"


encodeStage : Stage -> Value
encodeStage stage =
    Encode.string <|
        case stage of
            Early ->
                "Early"

            Middle ->
                "Middle"

            Late ->
                "Late"


encodeScript : Script -> Value
encodeScript script =
    Encode.string <|
        case script of
            Oracle ->
                "Oracle"

            Bronze ->
                "Bronze"

            Seal ->
                "Seal"


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
encodeMyCharRef { char, dimension, position, mirror } =
    Encode.object <|
        [ ( "char", encodeChar char )
        , ( "dimension", encodeVec2 dimension )
        , ( "position", encodeVec2 position )
        ]
            ++ (if mirror /= emptyMirror then
                    [ ( "mirror", encodeMirror mirror ) ]

                else
                    []
               )


encodeMirror : Mirror -> Value
encodeMirror mirror =
    Encode.object
        [ ( "x", Encode.bool mirror.x )
        , ( "y", Encode.bool mirror.y )
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
        Ok { chars, charExplainations, strokeWidth, language } ->
            let
                newModel =
                    { model
                        | chars =
                            chars
                        , charExplainations =
                            charExplainations
                        , strokeWidth =
                            strokeWidth
                        , language =
                            language
                    }
            in
            updateLanguage language newModel

        Err _ ->
            ( model, Cmd.none )


decodeSavedModel : Decoder SavedModel
decodeSavedModel =
    Decode.map4 SavedModel
        (Decode.field "chars" <| Decode.dict decodeMyChar)
        (Decode.field "charExplainations" <| Decode.dict decodeExplaination)
        (Decode.field "strokeWidth" Decode.float)
        (Decode.field "language" decodeLanguage)


decodeExplaination : Decoder Explaination
decodeExplaination =
    Decode.map2 Explaination
        (Decode.field "note" Decode.string)
        (Decode.maybe <| Decode.field "referenceImage" decodeReferenceImage)


decodeReferenceImage : Decoder ReferenceImage
decodeReferenceImage =
    Decode.map5 ReferenceImage
        (Decode.field "image" Decode.string)
        (Decode.field "origin" Decode.string)
        (Decode.maybe <|
            Decode.field "time" <|
                Decode.map2
                    (\period stage ->
                        { period = period
                        , stage = stage
                        }
                    )
                    (Decode.field "period" decodePeriod)
                    (Decode.maybe <| Decode.field "stage" decodeStage)
        )
        (Decode.field "script" decodeScript)
        (Decode.field "url" Decode.string)


decodePeriod : Decoder Period
decodePeriod =
    Decode.string
        |> Decode.andThen
            (\period ->
                case period of
                    "Shang" ->
                        Decode.succeed Shang

                    "WesternZhou" ->
                        Decode.succeed WesternZhou

                    "SpringAndAutumn" ->
                        Decode.succeed SpringAndAutumn

                    "WarringStates" ->
                        Decode.succeed WarringStates

                    _ ->
                        Decode.fail <|
                            "Trying to decode Period, but "
                                ++ period
                                ++ " is not supported."
            )


decodeStage : Decoder Stage
decodeStage =
    Decode.string
        |> Decode.andThen
            (\stage ->
                case stage of
                    "Early" ->
                        Decode.succeed Early

                    "Middle" ->
                        Decode.succeed Middle

                    "Late" ->
                        Decode.succeed Late

                    _ ->
                        Decode.fail <|
                            "Trying to decode Stage, but "
                                ++ stage
                                ++ " is not supported."
            )


decodeScript : Decoder Script
decodeScript =
    Decode.string
        |> Decode.andThen
            (\script ->
                case script of
                    "Oracle" ->
                        Decode.succeed Oracle

                    "Bronze" ->
                        Decode.succeed Bronze

                    "Seal" ->
                        Decode.succeed Seal

                    _ ->
                        Decode.fail <|
                            "Trying to decode Script, but "
                                ++ script
                                ++ " is not supported."
            )


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
        (Decode.field "char" Decode.string)
        (Decode.field "dimension" decodeVec2)
        (Decode.field "position" decodeVec2)
        (Decode.map (Maybe.withDefault emptyMirror) <| Decode.maybe <| Decode.field "mirror" decodeMirror)


decodeMirror : Decoder Mirror
decodeMirror =
    Decode.map2 Mirror
        (Decode.field "x" Decode.bool)
        (Decode.field "y" Decode.bool)


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


endDragging : Model -> ( Model, Cmd Msg )
endDragging model =
    ( addSnapshot <| { model
        | activeComponentIndex =
            Nothing
        , dragDelta =
            Vector2.vec2 0 0
      }
    , Cmd.none
    )


addSnapshot : Model -> Model
addSnapshot model =
    let
        history =
            model.selectedCharHistory
        selectedMyChar =
            unboxMyChar <| Dict.get (unboxChar model.selectedChar) model.chars
        lastIndex =
            Array.length history.snapshots - 1
    in
    { model
        | selectedCharHistory =
            if history.currentIndex >= lastIndex then
                { history
                    | snapshots =
                        Array.push selectedMyChar history.snapshots
                    , currentIndex =
                        history.currentIndex + 1
                }
            else
                { history
                    | snapshots =
                        Array.push selectedMyChar <|
                            Array.slice 0 history.currentIndex history.snapshots
                }
    }


startDragging : DragData -> Model -> ( Model, Cmd Msg )
startDragging { index, scale } ({ isSnapToGrid, boxUnits } as model) =
    ( updateActiveComponent
        (if isSnapToGrid then
            updateMyCharRefDimension (snapToGrid boxUnits)
                << updateMyCharRefPosition (snapToGrid boxUnits)

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
onDragBy delta ({ dragDelta, isSnapToGrid, boxUnits, unitSize } as model) =
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
updateOnDrag factor delta ({ activeScale, isAspectRatioLocked } as model) =
    updateActiveComponent
        (\char ->
            let
                offsetDimension xDir yDir =
                    offsetDrag DimensionOffset isAspectRatioLocked char.dimension factor xDir yDir delta

                offsetPosition xDir yDir =
                    offsetDrag PositionOffset isAspectRatioLocked char.dimension factor xDir yDir delta
            in
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

                ScaleLeft ->
                    updateMyCharRefDimension (Vector2.add (offsetDimension -1 0))
                        << updateMyCharRefPosition (Vector2.add (offsetPosition 1 1))

                ScaleRight ->
                    updateMyCharRefDimension (Vector2.add (offsetDimension 1 0))
                        << updateMyCharRefPosition (Vector2.add (offsetPosition 0 0))

                ScaleTop ->
                    updateMyCharRefDimension (Vector2.add (offsetDimension 0 -1))
                        << updateMyCharRefPosition (Vector2.add (offsetPosition 0 1))

                ScaleBottom ->
                    updateMyCharRefDimension (Vector2.add (offsetDimension 0 1))
                        << updateMyCharRefPosition (Vector2.add (offsetPosition 0 0))
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
            if pos < unitPercent then
                pos

            else
                (*) unitPercent <| toFloat <| round <| pos / unitPercent
    in
    Vector2.vec2
        (roundToGrid <| Vector2.getX position)
        (roundToGrid <| Vector2.getY position)


addComponentToMyChar : Dict Grapheme MyChar -> Grapheme -> MyChar -> MyChar
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
                        , mirror = emptyMirror
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
                        (\{ position, dimension } ->
                            let
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
            model.newCompoundChar

        newCompoundChar =
            CompoundChar
                { char = newChar
                , dimension = Vector2.vec2 100 100
                , position = Vector2.vec2 0 0
                , mirror = emptyMirror
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


selectChar : Grapheme -> Model -> ( Model, Cmd Msg )
selectChar char model =
    let
        selectedMyChar =
            unboxMyChar <| Dict.get char model.chars
    in
    ( { model
        | selectedChar =
            Just char
        , selectedCharHistory =
            { snapshots =
                Array.fromList [ selectedMyChar ]
            , currentIndex =
                0
            }
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
                        (\key a _ -> Dict.insert key a)
                        (\key b -> Dict.insert key b)
                        svgs
                        model.simpleCharSvgs
                        Dict.empty
            }

        Err _ ->
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
                                    , mirror = emptyMirror
                                    }
                                )
                        )
                        model.chars
                        svgs
                , simpleCharSvgs =
                    Dict.merge
                        (\key a -> Dict.insert key a)
                        (\key a _ -> Dict.insert key a)
                        (\key b -> Dict.insert key b)
                        svgs
                        model.simpleCharSvgs
                        Dict.empty
            }

        Err _ ->
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


updateMyCharRefPosition : (Vec2 -> Vec2) -> MyCharRef -> MyCharRef
updateMyCharRefPosition func myCharRef =
    { myCharRef
        | position =
            func myCharRef.position
    }


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


myCharTypeFromChar : Dict Grapheme MyChar -> Grapheme -> MyCharType
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
appHeader { spacing, fontSize } =
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
            case String.Graphemes.uncons newComponentChar of
                Just ( char, _ ) ->
                    let
                        inputLength =
                            String.Graphemes.length newComponentChar
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
                                    
                                    CharacterAlreadyExists ->
                                        ""
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
appPreferencesPopUp ({ palette, spacing, fontSize, isBackingUp } as model) =
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
        , if isBackingUp then
            E.row
                [ E.centerX
                ]
                [ E.el
                    [ E.paddingXY spacing.tiny 0 ]
                    (E.text "Backuped")
                , iconButton
                    { icon =
                        FeatherIcons.refreshCw
                    , size =
                        fontSize.thumb
                    , onPress =
                        Just UpdateBackupLocation
                    }
                ]

          else
            E.row
                [ E.spacing spacing.tiny ]
                [ E.text "Create backup"
                , iconButton
                    { icon =
                        FeatherIcons.plusCircle
                    , size =
                        fontSize.thumb
                    , onPress =
                        Just UpdateBackupLocation
                    }
                ]
        , E.row
            [ E.spacing spacing.tiny
            , E.centerX
            ]
            [ E.text "Upload backup"
            , iconButton
                { icon =
                    FeatherIcons.upload
                , size =
                    fontSize.thumb
                , onPress =
                    Just UploadBackup
                }
            ]
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
confirmDeletePopUpTemplate ({ trs, palette, spacing, fontSize } as model) targetName onConfirm =
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
previewInParagraphPopUp ({ palette, spacing, fontSize, previewOrientation, previewFontSize, trs } as model) =
    let
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
                            UpdatePreviewParagraph
                        , text =
                            model.previewParagraph
                        , placeholder =
                            Just <| Input.placeholder [ E.alignLeft ] (E.text <| Translations.writeTextHereToPreview model.trs)
                        , label =
                            Input.labelHidden <| Translations.writeTextHereToPreview model.trs
                        , spellcheck =
                            False
                        }
                    )
                ]
            , E.column
                [ E.width <| E.fillPortion 2
                , E.height E.fill
                , E.paddingXY 0 spacing.small
                , E.spacing spacing.small
                ]
                [ strokeWidthPreference model
                , previewFontSizePreference model
                , Input.radio
                    [ E.spacing spacing.small
                    ]
                    { onChange = UpdatePreviewOrientation
                    , selected = Just previewOrientation
                    , label =
                        Input.labelAbove [ E.alignLeft, E.paddingEach { top = 0, bottom = spacing.small, left = 0, right = 0 } ]
                            (E.text <| Translations.orientation trs)
                    , options =
                        [ Input.optionWith Horizontal
                            (radioOption palette.darkFg fontSize (E.text <| Translations.OrientationType.horizontal trs))
                        , Input.optionWith Vertical
                            (radioOption palette.darkFg fontSize (E.text <| Translations.OrientationType.vertical trs))
                        ]
                    }
                , iconButton
                    { icon =
                        FeatherIcons.download
                    , size =
                        fontSize.large
                    , onPress =
                        Just <| DownloadCharsForPreview
                    }
                ]
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
renderPreviewInParagraph displayFontSize ({ previewParagraph, previewOrientation, chars, unitSize, boxUnits, strokeWidth } as model) =
    let
        lines =
            String.Graphemes.lines previewParagraph
    in
    ( case previewOrientation of
        Horizontal ->
            E.column
                [ E.centerX
                , E.centerY
                ]
        
        Vertical ->
            E.row
                [ E.centerX
                , E.centerY
                , E.htmlAttribute <| Html.Attributes.style "flex-direction" "row-reverse"
                , E.height <| E.fill
                ]
    )
    <|
        List.map
            (E.wrappedRow
                ( case previewOrientation of
                    Horizontal ->
                        [ E.width E.fill ]
                    
                    Vertical ->
                        [ E.width E.fill
                        , E.htmlAttribute <| Html.Attributes.style "flex-flow" "column wrap-reverse"
                        , E.height <| E.fill
                        ]
                )
                << List.map
                    (\char ->
                        case Dict.get char chars of
                            Just myChar ->
                                E.el
                                    [ E.htmlAttribute <|
                                        Html.Attributes.style "margin" <|
                                            String.fromInt (displayFontSize // 4)
                                                ++ "px 0"
                                    ]
                                <|
                                    E.html <|
                                        renderChar
                                            { model
                                                | unitSize = toFloat displayFontSize / toFloat boxUnits
                                                , strokeWidth = strokeWidth * toFloat displayFontSize / (toFloat boxUnits * unitSize)
                                            }
                                            RenderModeDisplay
                                            myChar

                            Nothing ->
                                E.text <| char
                    )
                << (\charsInLine ->
                        if List.isEmpty charsInLine then
                            [ " " ]

                        else
                            charsInLine
                   )
                << String.Graphemes.toList
            )
            lines


confirmDeleteSelectedCharPopUp : Model -> E.Element Msg
confirmDeleteSelectedCharPopUp ({ selectedChar } as model) =
    confirmDeletePopUpTemplate
        model
        (unboxChar selectedChar)
        DeleteSelectedChar


addCompoundCharPopUp : Model -> E.Element Msg
addCompoundCharPopUp ({ trs, newCompoundChar, inputError, palette, spacing, fontSize, boxUnits, thumbnailUnitSize, chars } as model) =
    let
        inputLength =
            String.Graphemes.length newCompoundChar

        newInputError =
            if inputLength /= 1 then
                Just <| InvalidInputLength inputLength

            else if Dict.member newCompoundChar chars then
                Just <| CharacterAlreadyExists
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
                    Just e ->
                        E.column
                        [ E.centerX
                        , Font.size fontSize.small
                        , E.width <| E.px <| round width
                        , E.padding spacing.small
                        , E.spacing spacing.small
                        ]
                        [ E.paragraph
                            [ E.centerX
                            ]
                            [ E.text <|
                                case e of
                                    CharacterAlreadyExists ->
                                        Translations.characterAlreadyExists trs
                                    _ ->
                                        Translations.acceptOnlyOneCharacter trs
                            ]
                        , case e of
                            CharacterAlreadyExists ->
                                E.el [ E.centerX ] <|
                                    textButton model (Translations.editChar trs newCompoundChar)
                                        (Just <| EditChar newCompoundChar)
                            _ ->
                                E.none
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
editorSidePanel ({ spacing } as model) =
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
charExplaination { palette, fontSize, spacing, selectedChar, charExplainations, trs } =
    let
        explaination =
            Maybe.withDefault emptyExplaination <|
                Dict.get (unboxChar selectedChar) charExplainations
    in
    E.column
        [ E.spacing spacing.medium
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
                    (E.text <| Translations.explaination trs)
            , spellcheck =
                False
            }
        , E.row
            [ E.spacing spacing.small
            , E.width E.fill
            ]
            [ E.text <| Translations.referenceImage trs
            , iconButton
                { icon =
                    case explaination.referenceImage of
                        Just _ ->
                            FeatherIcons.refreshCw

                        Nothing ->
                            FeatherIcons.upload
                , size =
                    fontSize.thumb
                , onPress =
                    Just UploadReferenceImage
                }
            ]
        , case explaination.referenceImage of
            Just referenceImage ->
                E.row
                    [ E.spacing spacing.medium ]
                    [ E.column
                        [ E.spacing <| spacing.medium
                        , E.width <| E.px <| fontSize.medium * 15
                        ]
                        [ E.image
                            [ E.width E.fill ]
                            { src =
                                referenceImage.image
                            , description =
                                Translations.referenceImage trs
                            }
                        , Input.text
                            [ E.width E.fill
                            , Font.alignLeft
                            ]
                            { onChange =
                                UpdateReferenceImageOrigin
                            , text =
                                referenceImage.origin
                            , placeholder =
                                Just <| Input.placeholder [] <| E.text <| Translations.origin trs
                            , label =
                                Input.labelHidden <| Translations.origin trs
                            }
                        , Input.text
                            [ E.width E.fill
                            , Font.alignLeft
                            , E.below <|
                                if String.isEmpty referenceImage.url then
                                    E.none

                                else
                                    E.newTabLink
                                        [ E.centerY
                                        , E.alignRight
                                        , E.paddingXY 0 spacing.small
                                        ]
                                        { url =
                                            referenceImage.url
                                        , label =
                                            iconButton
                                                { icon =
                                                    FeatherIcons.externalLink
                                                , size =
                                                    fontSize.thumb
                                                , onPress =
                                                    Nothing
                                                }
                                        }
                            ]
                            { onChange =
                                UpdateReferenceImageUrl
                            , text =
                                referenceImage.url
                            , placeholder =
                                Just <| Input.placeholder [] <| E.text <| Translations.url trs
                            , label =
                                Input.labelHidden <| Translations.url trs
                            }
                        ]
                    , E.column
                        [ E.spacing spacing.medium
                        , E.alignTop
                        ]
                        [ Input.radio
                            [ E.spacing spacing.small
                            ]
                            { onChange = UpdateReferenceImageScript
                            , selected = Just referenceImage.script
                            , label =
                                Input.labelAbove [ E.alignLeft, E.paddingEach { top = 0, bottom = spacing.small, left = 0, right = 0 } ]
                                    (E.text <| Translations.script trs)
                            , options =
                                [ Input.optionWith Oracle
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.ScriptName.oracle trs))
                                , Input.optionWith Bronze
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.ScriptName.bronze trs))
                                , Input.optionWith Seal
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.ScriptName.seal trs))
                                ]
                            }
                        , Input.radio
                            [ E.spacing spacing.small
                            ]
                            { onChange = UpdateReferenceImagePeriod
                            , selected = Maybe.map .period referenceImage.time
                            , label =
                                Input.labelAbove [ E.alignLeft, E.paddingEach { top = 0, bottom = spacing.small, left = 0, right = 0 } ]
                                    (E.text <| Translations.period trs)
                            , options =
                                [ Input.optionWith Shang
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.PeriodName.shang trs))
                                , Input.optionWith WesternZhou
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.PeriodName.westernZhou trs))
                                , Input.optionWith SpringAndAutumn
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.PeriodName.springAndAutumn trs))
                                , Input.optionWith WarringStates
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.PeriodName.warringStates trs))
                                ]
                            }
                        , Input.radio
                            [ E.spacing spacing.small
                            ]
                            { onChange = UpdateReferenceImageStage
                            , selected = Maybe.andThen .stage referenceImage.time
                            , label =
                                Input.labelAbove [ E.alignLeft, E.paddingEach { top = 0, bottom = spacing.small, left = 0, right = 0 } ]
                                    (E.text <| Translations.stage trs)
                            , options =
                                [ Input.optionWith Early
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.StageName.early trs))
                                , Input.optionWith Middle
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.StageName.middle trs))
                                , Input.optionWith Late
                                    (radioOption palette.lightFg fontSize (E.text <| Translations.StageName.late trs))
                                ]
                            }
                        ]
                    ]

            Nothing ->
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


previewFontSizePreference : Model -> E.Element Msg
previewFontSizePreference { palette, spacing, fontSize, previewFontSize, trs } =
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
        { onChange = UpdatePreviewFontSize << round
        , label =
            Input.labelLeft []
                (E.row
                    [ E.spacing spacing.small ]
                    [ E.text <| Translations.fontSize trs
                    , E.text <| String.fromInt previewFontSize
                    ]
                )
        , min = minPreviewFontSize
        , max = maxPreviewFontSize
        , step = Just 1
        , value = toFloat previewFontSize
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
editor ({ selectedChar, selectedCharHistory, chars, spacing, palette, fontSize } as model) =
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
                (E.text <| unboxChar selectedChar)
            , E.el
                [ E.alignRight
                , E.spacing spacing.small
                , E.onLeft <|
                    if Array.isEmpty selectedCharHistory.snapshots then
                        E.none
                    else
                        E.row
                        [ E.spacing spacing.small
                        , E.paddingXY spacing.small 0
                        ]
                        [ E.el
                            [ Font.color <|
                                if selectedCharHistory.currentIndex == 0 then
                                    palette.disabled
                                else
                                    palette.black
                            ] <|
                            iconButton
                            { icon =
                                FeatherIcons.cornerUpLeft
                            , size =
                                fontSize.title
                            , onPress =
                                Just <| UndoSelectedCharHistory
                            }
                        , E.el
                            [ Font.color <|
                                if selectedCharHistory.currentIndex == Array.length selectedCharHistory.snapshots - 1 then
                                    palette.disabled
                                else
                                    palette.black
                            ] <|
                            iconButton
                            { icon =
                                FeatherIcons.cornerUpRight
                            , size =
                                fontSize.title
                            , onPress =
                                Just <| RedoSelectedCharHistory
                            }
                        ]
                ] <|
                if isMyCharType SimpleCharType (myCharFromChar chars (unboxChar selectedChar)) then
                    iconButton
                        { icon =
                            FeatherIcons.refreshCw
                        , size =
                            fontSize.title
                        , onPress =
                            Just <| UploadSimpleChar
                        }

                else
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
                                RenderModeEditor
                                (myCharFromChar chars char)

                    Nothing ->
                        E.none
            ]
            (E.html <| gridBackground model)
        ]


unboxChar : Maybe Grapheme -> Grapheme
unboxChar =
    Maybe.withDefault "?"


unboxMyChar : Maybe MyChar -> MyChar
unboxMyChar =
    Maybe.withDefault emptyMyChar


isCharPartOfMyChar : Dict Grapheme MyChar -> Grapheme -> MyChar -> Bool
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
                [ TypedSvg.Core.text <| unboxChar selectedChar
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
charPanel myCharType ({ trs, palette, spacing, fontSize } as model) =
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
charCard ({ activeComponentIndex, unitSize, thumbnailUnitSize, boxUnits, strokeWidth, selectedChar, palette, spacing, fontSize } as model) myChar =
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
         , Events.onClick <| SelectChar char
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
                (E.text <| char)
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
                RenderModeThumbnail
                myChar
        ]


charFromMyChar : MyChar -> Grapheme
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


type RenderMode
    = RenderModeEditor
    | RenderModeThumbnail
    | RenderModeDisplay


renderChar : Model -> RenderMode -> MyChar -> Svg Msg
renderChar ({ unitSize, boxUnits, borderUnits, strokeWidth } as model) renderMode myChar =
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

        dimension =
            (calculateMyCharDimension myChar).dimension

        width =
            Vector2.getX dimension

        margin =
            15

        renderedMargin =
            margin - (100 - width) * (0.5 - 1 / 100 * margin)
    in
    Svg.svg
        ([ SvgAttributes.width <| SvgTypes.px outerBoxSize
         , SvgAttributes.height <| SvgTypes.px outerBoxSize
         , Html.Attributes.style "pointer-events" "none"
         ]
            ++ (case renderMode of
                    RenderModeThumbnail ->
                        [ SvgAttributes.id ("char-" ++ charFromMyChar myChar) ]

                    RenderModeDisplay ->
                        [ Html.Attributes.style "margin" <| "0 " ++ String.fromFloat renderedMargin ]

                    _ ->
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
            (case renderMode of
                RenderModeDisplay ->
                    [ Html.Attributes.style "pointer-events" "none"
                    ]

                _ ->
                    [ SvgAttributes.width <| SvgTypes.px scaledBoxSize
                    , SvgAttributes.height <| SvgTypes.px scaledBoxSize
                    , SvgAttributes.x <| SvgTypes.px offset
                    , SvgAttributes.y <| SvgTypes.px offset
                    ]
            )
            [ renderCharHelper
                { model
                    | unitSize = scaledUnitSize
                }
                { charClassName = charClassName
                , index = -1
                , isStatic =
                    case renderMode of
                        RenderModeEditor ->
                            False

                        _ ->
                            True
                , tightDimension =
                    { position = Vector2.vec2 0 0, dimension = Vector2.vec2 100 100 }
                , mirror = emptyMirror
                , parentMyCharType =
                    myCharTypeFromMyChar myChar
                }
                0
                myChar
            ]
        ]


myCharRefFromMyChar : MyChar -> MyCharRef
myCharRefFromMyChar myChar =
    case myChar of
        SimpleChar ref ->
            ref

        CompoundChar ref _ ->
            ref


renderCharHelper :
    Model
    ->
        { charClassName : String
        , index : Int
        , isStatic : Bool
        , tightDimension : { position : Vec2, dimension : Vec2 }
        , mirror : Mirror
        , parentMyCharType : MyCharType
        }
    -> Int
    -> MyChar
    -> Svg Msg
renderCharHelper ({ boxUnits, chars, simpleCharSvgs, activeComponentIndex, palette, fontSize } as model) { charClassName, index, isStatic, tightDimension, parentMyCharType, mirror } level myChar =
    let
        char =
            charFromMyChar myChar

        levelwiseIndex =
            index + (level - 1) * 10

        isDraggable =
            not (isStatic || level > 1)

        constraint charType dimension position contents =
            let
                tightPosition =
                    Vector2.sub position tightDimension.position

                xFactor =
                    100 / Vector2.getX tightDimension.dimension

                yFactor =
                    100 / Vector2.getY tightDimension.dimension

                styledContents =
                    [ Svg.g
                        ((case charType of
                            SimpleCharType ->
                                [ SvgAttributes.class [ charClassName ] ]

                            CompoundCharType ->
                                []
                         )
                            ++ (if mirror /= emptyMirror then
                                    [ SvgAttributes.transform
                                        [ SvgTypes.Scale
                                            (if mirror.x then
                                                -1

                                             else
                                                1
                                            )
                                            (if mirror.y then
                                                -1

                                             else
                                                1
                                            )
                                        ]
                                    , Html.Attributes.style "transform-origin" "center"
                                    ]

                                else
                                    []
                               )
                        )
                        contents
                    ]

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
                if Just levelwiseIndex == activeComponentIndex && not isStatic then
                    styledContents
                        ++ (activeComponentButtons parentMyCharType model
                                :: (if Vector2.getX dimension < unitPercent then
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
                    TypedSvg.Core.text <| "Error rendering " ++ char

        CompoundChar { dimension, position } components ->
            constraint CompoundCharType dimension position <|
                List.indexedMap
                    (\componentIndex componentRef ->
                        (\component ->
                            renderCharHelper
                                model
                                { charClassName = charClassName
                                , index = componentIndex
                                , isStatic = isStatic
                                , tightDimension =
                                    if level >= 1 then
                                        calculateMyCharDimension myChar

                                    else
                                        { position = Vector2.vec2 0 0, dimension = Vector2.vec2 100 100 }
                                , mirror = componentRef.mirror
                                , parentMyCharType = parentMyCharType
                                }
                                (level + 1)
                                component
                        )
                        <|
                            myCharFromMyCharRef chars componentRef
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
                , mirrorXButton model
                , mirrorYButton model
                , copyActiveComponentButton model
                , deleteActiveComponentButton model
                ]


mirrorXButton : Model -> Svg Msg
mirrorXButton { palette, spacing, fontSize } =
    activeComponentButton
        fontSize
        (-1.5 * toFloat fontSize.title - toFloat spacing.small)
        mirrorXIcon
        palette.darkFg
        (MirrorActiveComponent MirrorDirectionY)


mirrorYButton : Model -> Svg Msg
mirrorYButton { palette, spacing, fontSize } =
    activeComponentButton
        fontSize
        (-2.5 * toFloat fontSize.title - 2 * toFloat spacing.small)
        mirrorYIcon
        palette.darkFg
        (MirrorActiveComponent MirrorDirectionX)


mirrorXIcon : FeatherIcons.Icon
mirrorXIcon =
    [ Svg.polygon
        [ SvgAttributes.points [ ( 50, 50 ), ( 450, 50 ), ( 250, 180 ) ] ]
        []
    , Svg.line
        [ SvgAttributes.x1 <| SvgTypes.px 50
        , SvgAttributes.y1 <| SvgTypes.px 250
        , SvgAttributes.x2 <| SvgTypes.px 450
        , SvgAttributes.y2 <| SvgTypes.px 250
        , SvgAttributes.strokeDasharray "60, 100"
        ]
        []
    , Svg.polygon
        [ SvgAttributes.points [ ( 50, 450 ), ( 450, 450 ), ( 250, 320 ) ] ]
        []
    ]
        |> FeatherIcons.customIcon
        |> FeatherIcons.withStrokeWidth 40
        |> FeatherIcons.withSize 26
        |> FeatherIcons.withViewBox "0 0 500 500"


mirrorYIcon : FeatherIcons.Icon
mirrorYIcon =
    [ Svg.polygon
        [ SvgAttributes.points [ ( 50, 100 ), ( 50, 400 ), ( 180, 250 ) ] ]
        []
    , Svg.line
        [ SvgAttributes.x1 <| SvgTypes.px 250
        , SvgAttributes.y1 <| SvgTypes.px 50
        , SvgAttributes.x2 <| SvgTypes.px 250
        , SvgAttributes.y2 <| SvgTypes.px 450
        , SvgAttributes.strokeDasharray "60, 100"
        ]
        []
    , Svg.polygon
        [ SvgAttributes.points [ ( 450, 100 ), ( 450, 400 ), ( 320, 250 ) ] ]
        []
    ]
        |> FeatherIcons.customIcon
        |> FeatherIcons.withStrokeWidth 40
        |> FeatherIcons.withSize 26
        |> FeatherIcons.withViewBox "0 0 500 500"


aspectRatioLockButton : Model -> Svg Msg
aspectRatioLockButton { isSnapToGrid, isAspectRatioLocked, palette, fontSize } =
    if isSnapToGrid then
        Svg.g [] []

    else
        activeComponentButton fontSize
            (-0.5 * toFloat fontSize.title)
            (if isAspectRatioLocked then
                FeatherIcons.lock

             else
                FeatherIcons.unlock
            )
            palette.darkFg
            ToggleIsAspectRatioLocked


copyActiveComponentButton : Model -> Svg Msg
copyActiveComponentButton { palette, spacing, fontSize } =
    activeComponentButton fontSize (0.5 * toFloat fontSize.title + toFloat spacing.small) FeatherIcons.copy palette.lightFg CopyActiveComponent


deleteActiveComponentButton : Model -> Svg Msg
deleteActiveComponentButton { palette, spacing, fontSize } =
    activeComponentButton fontSize (1.5 * toFloat fontSize.title + 2 * toFloat spacing.small) FeatherIcons.trash2 palette.danger DeleteActiveComponent


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


myCharFromChar : Dict Grapheme MyChar -> Grapheme -> MyChar
myCharFromChar chars char =
    -- impossible
    Maybe.withDefault emptyMyChar <|
        Dict.get char chars


myCharFromMyCharRef : Dict Grapheme MyChar -> MyCharRef -> MyChar
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



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Sub.batch
        [ Draggable.subscriptions DragMsg drag
        , getModelPort GotModel
        , gotSavedSimpleCharsPort GotSavedSimpleChars
        , gotNewSimpleCharsPort GotNewSimpleChars
        , loadedSimpleCharPort LoadedSimpleChar
        , Time.every 1000 (\_ -> SaveModel ())
        , Browser.Events.onResize UpdateDevice
        , succeededInBackupPort (\_ -> SucceededInBackup)
        ]


decodeSimpleCharSvgs : Decode.Decoder SimpleCharSvgs
decodeSimpleCharSvgs =
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
