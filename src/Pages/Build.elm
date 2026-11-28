module Pages.Build exposing
    ( BuilderTab(..)
    , Model
    , Msg(..)
    , Outcome(..)
    , document
    , history
    , init
    , previewRequests
    , resize
    , setHistory
    , setLanguage
    , setPartCacheAndIndex
    , subscriptions
    , update
    , view
    )

{-| The car editor (`BuildPage`): a Brick Play builder for one car.

Includes the 3D builder `Studio.Core`, the parts dock, starter bases & preset cars,
URL/file/QR model import, and live mechanical inspection & chassis specs.

The build is saved as it changes into the build history (`Game.BuildHistory`,
kept in localStorage by `main.js` through `Ports.saveBuildHistory`): a short
while after each edit, when the page is hidden (a phone switching apps or
discarding the tab), when the bases drawer opens, before another base, preset
or build replaces it, and when the player finishes. The bases drawer lists the
saved builds under the starter bases, so a replaced build is one tap away.
Base cards and saved builds show pictures from the shared thumbnail pipeline
(`Game.ModelPreview`, `Ports.requestModelThumbnails`).

-}

import Browser.Events
import DesignTokens.Colors as Brand
import Dict exposing (Dict)
import FeatherIcons
import Game.BuildHistory as BuildHistory exposing (History)
import Game.ModelPreview as ModelPreview
import Game.Ports as GamePorts
import Html exposing (Html, button, div, h3, input, span, text, textarea)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onMouseLeave)
import I18n exposing (Language(..))
import Json.Decode as Decode
import LDraw.Catalog as Catalog exposing (PartIndex)
import LDraw.ColorData as ColorData
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache)
import Ports
import Process
import Racer.Analyze as Analyze
import Racer.Editor as Editor exposing (Base, PartEntry)
import Racer.GarageFloor as GarageFloor
import Racer.I18n as RI18n
import Racer.Ingest as Ingest
import Racer.Types exposing (Fix(..), PresetBuild, ValidationCode(..), ValidationIssue, ValidationSeverity(..))
import Set
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.Health as Health
import Studio.Shortcuts exposing (TurnDirection(..))
import Task
import Time
import UI.PartCard as PartCard
import Viewport


{-| Tabs available within the builder experience.
-}
type BuilderTab
    = BuildBricksTab
    | AllBricksTab
    | BasesTab
    | ImportBuildTab
    | ReviewTab


{-| Editor state: studio, base, active tab, thumbnails, import inputs, and part cache/index.

The build history: the saved builds (`history`), the clock (ms, refreshed
by `ClockTick` and each autosave), the editing session saves go to
(`session`, `Nothing` until the next save starts a new one; `lastSession` is
the newest session id used), the edit counter the autosave debounce checks
(`revision`), the build link of the build as last saved (`savedCode`), and
the pictures of bases (`"base:" ++ id`) and saved builds (by build link) in
`previews`.

-}
type alias Model =
    { studio : Core.Model
    , base : Maybe Base
    , baseLocked : Bool
    , colour : Int
    , language : Language
    , width : Int
    , height : Int
    , thumbnails : Dict String String
    , notice : Maybe String
    , activeTab : BuilderTab
    , pasteText : String
    , pasteError : Maybe String
    , catalogFilter : String
    , partCache : PartCache
    , partIndex : PartIndex
    , activePart : String
    , recentParts : List String
    , history : History
    , clock : Int
    , session : Maybe Int
    , lastSession : Int
    , revision : Int
    , savedCode : String
    , previews : Dict String String
    }


{-| Editor messages.
-}
type Msg
    = StudioMsg Core.Msg
    | PickColour Int
    | AddPart String
    | AddPartList (List String)
    | SelectPlacements (List Int)
    | RemovePlacements (List Int)
    | SnapPlacements (List Int)
    | Turn TurnDirection
    | Raise
    | Lower
    | DeleteSelected
    | Undo
    | Redo
    | FitView
    | Finish
    | Leave
    | ThumbnailLoaded { file : String, color : Int, url : String }
    | SelectTab BuilderTab
    | SelectPreset PresetBuild
    | SelectBase Base
    | ToggleBaseLock
    | PasteTextChanged String
    | CopySelected
    | PasteSelected
    | PaletteLeft
    | CanvasTapped
    | CatalogFilterChanged String
    | SubmitPaste
    | CopyBuildCode
    | RequestFileUpload
    | OpenQrScanner
    | LoadExternalDocument Document (Maybe Base)
    | ClockTick Time.Posix
    | AutoSave Int Time.Posix
    | PageVisibility Browser.Events.Visibility
    | RestoreBuild BuildHistory.Entry
    | ModelPreviewLoaded { key : String, url : String }


{-| What the editor asks `Main` to do after an update.
-}
type Outcome
    = Stay
    | Quit
    | Finished Document
    | TriggerFileUpload
    | TriggerQrScanner


{-| Open the editor on a build that optionally stands on `base`.
-}
init :
    { ldrawBase : String, ldrawFallbackBase : String, supportsPointerEvents : Bool, language : Language, width : Int, height : Int }
    -> Maybe Base
    -> Document
    -> ( Model, Cmd Msg )
init config maybeBase doc =
    let
        ( studio, studioCmd ) =
            Core.init
                { ldrawBase = config.ldrawBase
                , ldrawFallbackBase = config.ldrawFallbackBase
                , defaultModelUrl = ""
                , initialHash = ""
                , supportsPointerEvents = config.supportsPointerEvents
                , document = doc
                , gridStep = Just 20
                , rotateStep = Just 90
                , snapDistance = Just 24.0
                , collisionMode = Nothing
                }
                |> Tuple.mapFirst
                    (Core.setGizmoStyle Core.NoGizmo
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                        >> Core.setTwoFingerOrbits True
                        >> Core.setGlueEditable False
                        >> Core.setNudgeSteps { horizontal = 20, vertical = 8 }
                    )

        ( fittedStudio, fitCmd ) =
            Core.fitView studio

        model =
            { studio = fittedStudio
            , base = maybeBase
            , baseLocked = maybeBase /= Nothing
            , colour = Editor.defaultColour
            , language = config.language
            , width = config.width
            , height = config.height
            , thumbnails = Dict.empty
            , notice = Nothing
            , activeTab = BuildBricksTab
            , pasteText = ""
            , pasteError = Nothing
            , catalogFilter = ""
            , partCache = Dict.empty
            , partIndex = Catalog.emptyPartIndex
            , activePart = "3001.dat"
            , recentParts = [ "3001.dat", "3003.dat", "3020.dat", "3022.dat", "3039.dat", "4600.dat" ]
            , history = BuildHistory.empty
            , clock = 0
            , session = Nothing
            , lastSession = 0
            , revision = 0
            , savedCode = ""
            , previews = Dict.empty
            }

        ( sized, sizeCmd ) =
            resize config.width config.height model
    in
    ( sized
    , Cmd.batch
        [ Cmd.map StudioMsg studioCmd
        , Cmd.map StudioMsg fitCmd
        , sizeCmd
        , requestThumbnails sized
        , Task.perform ClockTick Time.now
        ]
    )


{-| The saved builds, newest first.
-}
history : Model -> History
history model =
    model.history


{-| Hand the editor the saved builds (opening it, or once `main.js` has read
them from localStorage).
-}
setHistory : History -> Model -> Model
setHistory saved model =
    { model | history = saved }


{-| The build as it stands.
-}
document : Model -> Document
document model =
    Core.getDocument model.studio


{-| Inject part cache and index for live chassis validation.
-}
setPartCacheAndIndex : PartCache -> PartIndex -> Model -> Model
setPartCacheAndIndex cache index model =
    { model | partCache = cache, partIndex = index }


{-| Follow a change of the window size.
-}
resize : Int -> Int -> Model -> ( Model, Cmd Msg )
resize width height model =
    let
        sized =
            { model | width = width, height = height }

        ( next, cmd ) =
            runCore (Core.update (Core.ViewportMsg (Viewport.WindowResize width (canvasHeight sized)))) sized
    in
    ( next, cmd )


{-| Follow a change of the language.
-}
setLanguage : Language -> Model -> Model
setLanguage language model =
    { model | language = language }


{-| Move the active part to the front of the palette. The palette keeps its
order while the user taps the same card, and this runs once the user has moved on.
-}
commitRecent : Model -> Model
commitRecent model =
    { model
        | recentParts =
            (model.activePart :: List.filter (\f -> f /= model.activePart) model.recentParts)
                |> List.take 7
    }


{-| Messages that mean the user has moved on from the palette card they tapped.
-}
endsPaletteTap : Msg -> Bool
endsPaletteTap msg =
    case msg of
        SelectTab _ ->
            True

        PaletteLeft ->
            True

        CanvasTapped ->
            True

        PickColour _ ->
            True

        Turn _ ->
            True

        Raise ->
            True

        Lower ->
            True

        DeleteSelected ->
            True

        Undo ->
            True

        Redo ->
            True

        FitView ->
            True

        ToggleBaseLock ->
            True

        SelectBase _ ->
            True

        SelectPreset _ ->
            True

        _ ->
            False


{-| Handle an editor message.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Outcome )
update msg model =
    let
        shouldCommit =
            case msg of
                AddPart file ->
                    file /= model.activePart

                _ ->
                    endsPaletteTap msg

        ( next, cmd, outcome ) =
            updateModel msg
                (if shouldCommit then
                    commitRecent model

                 else
                    model
                )

        ( saved, saveCmd ) =
            scheduleAutoSave model next
    in
    if saved.activeTab /= model.activeTab then
        -- Any change of drawer changes the canvas height, so resize it here.
        let
            ( resized, resizeCmd ) =
                runCore (Core.update (Core.ViewportMsg (Viewport.WindowResize saved.width (canvasHeight saved)))) saved
        in
        ( resized, Cmd.batch [ cmd, saveCmd, resizeCmd, requestPreviews resized ], outcome )

    else
        ( saved, Cmd.batch [ cmd, saveCmd ], outcome )


{-| How long the build must stay unchanged before it is saved.
-}
autoSaveDelay : Float
autoSaveDelay =
    1500


{-| After an edit, save the build once it has stayed unchanged for
`autoSaveDelay` (each edit bumps `revision`, so only the last one saves).
-}
scheduleAutoSave : Model -> Model -> ( Model, Cmd Msg )
scheduleAutoSave before after =
    if Core.getDocument after.studio /= Core.getDocument before.studio then
        let
            revision =
                after.revision + 1
        in
        ( { after | revision = revision }
        , Process.sleep autoSaveDelay
            |> Task.andThen (\_ -> Time.now)
            |> Task.perform (AutoSave revision)
        )

    else
        ( after, Cmd.none )


{-| Whether the build has anything on it beyond a bare starter base.
-}
hasOwnParts : Model -> Bool
hasOwnParts model =
    let
        doc =
            Core.getDocument model.studio

        count =
            List.length (Document.flattenedPlacements doc)
    in
    case model.base of
        Just base ->
            not (Editor.baseIntact base doc && count <= base.partCount)

        Nothing ->
            count > 0


{-| Save the build as it stands into the history, in the current session (a
new one when there is none), and store the history. A bare starter base is
not saved.
-}
saveBuild : Model -> ( Model, Cmd Msg )
saveBuild model =
    if not (hasOwnParts model) then
        ( model, Cmd.none )

    else
        let
            doc =
                Core.getDocument model.studio

            sessionId =
                case model.session of
                    Just id ->
                        id

                    Nothing ->
                        max model.clock (model.lastSession + 1)

            code =
                Ingest.encodeBuildLink doc

            saved =
                BuildHistory.record
                    { code = code
                    , base = model.base |> Maybe.map .id |> Maybe.withDefault ""
                    , parts = List.length (Document.flattenedPlacements doc)
                    , savedAt = model.clock
                    , session = sessionId
                    }
                    model.history

            next =
                { model | session = Just sessionId, lastSession = max sessionId model.lastSession, savedCode = code }
        in
        if saved == model.history then
            ( next, Cmd.none )

        else
            ( { next | history = saved }, Ports.saveBuildHistory (BuildHistory.toJson saved) )


{-| Save the build and start a new session, so later edits are saved as a
new entry (and the saved one stays in the history).
-}
saveSnapshot : Model -> ( Model, Cmd Msg )
saveSnapshot model =
    saveBuild model |> Tuple.mapFirst (\m -> { m | session = Nothing })


{-| The picture requests the bases drawer still needs: every starter base and
every saved build without a picture yet. Asked for whenever the drawer opens.
-}
previewRequests : Model -> List ModelPreview.Request
previewRequests model =
    let
        bases =
            Editor.bases
                |> List.filter (\b -> not (Dict.member (basePreviewKey b) model.previews))
                |> List.map (\b -> ModelPreview.request (basePreviewKey b) b.document)

        builds =
            BuildHistory.entries model.history
                |> List.filter (\e -> not (Dict.member e.code model.previews))
                |> List.filterMap
                    (\e ->
                        Ingest.decodeRaceInput Nothing e.code
                            |> Result.toMaybe
                            |> Maybe.map (\decoded -> ModelPreview.request e.code decoded.document)
                    )
    in
    bases ++ builds


requestPreviews : Model -> Cmd Msg
requestPreviews model =
    if model.activeTab == BasesTab then
        case previewRequests model of
            [] ->
                Cmd.none

            requests ->
                Ports.requestModelThumbnails requests

    else
        Cmd.none


basePreviewKey : Base -> String
basePreviewKey base =
    "base:" ++ base.id


{-| Replace the build with `doc` (optionally on `base`): the studio is reset
(its undo history too), the view fitted and the drawer closed. The base is
locked when the build holds it.
-}
loadBuild : Maybe Base -> Document -> Model -> ( Model, Cmd Msg )
loadBuild maybeBase doc model =
    let
        ( nextStudio, cmd, events ) =
            Core.resetDocument doc model.studio

        ( fittedStudio, fitCmd ) =
            Core.fitView nextStudio

        updated =
            { model
                | studio = fittedStudio
                , base = maybeBase
                , baseLocked = maybeBase /= Nothing
                , activeTab = BuildBricksTab
                , notice = Nothing
            }
    in
    ( updated
    , Cmd.batch
        [ Cmd.map StudioMsg cmd
        , Cmd.map StudioMsg fitCmd
        , Cmd.batch (List.map eventCmd events)
        , requestThumbnails updated
        ]
    )


{-| Save the build, then replace it (see `loadBuild`): the old build stays in
the history.
-}
replaceBuild : Maybe Base -> Document -> Model -> ( Model, Cmd Msg )
replaceBuild maybeBase doc model =
    let
        ( saved, saveCmd ) =
            saveSnapshot model

        ( loaded, loadCmd ) =
            loadBuild maybeBase doc saved
    in
    ( loaded, Cmd.batch [ saveCmd, loadCmd ] )


updateModel : Msg -> Model -> ( Model, Cmd Msg, Outcome )
updateModel msg model =
    let
        stay ( m, c ) =
            ( m, c, Stay )
    in
    case msg of
        StudioMsg studioMsg ->
            stay (runCore (Core.update studioMsg) model)

        PickColour code ->
            let
                picked =
                    { model | colour = code, notice = Nothing }
            in
            if Core.hasSelection model.studio && not (baseSelected model) then
                let
                    ( next, cmd ) =
                        runCore (Core.recolorSelection code) picked
                in
                ( next, Cmd.batch [ cmd, requestThumbnails next ], Stay )

            else if Core.hasSelection model.studio && baseSelected model then
                ( { picked | notice = Just (keepBaseColours model.language) }, Cmd.none, Stay )

            else
                ( picked, requestThumbnails picked, Stay )

        AddPart file ->
            let
                matrix =
                    Editor.dropMatrix (Core.getDocument model.studio) model.studio.selection file

                partName =
                    Editor.parts
                        |> List.filter (\p -> p.file == file)
                        |> List.head
                        |> Maybe.map
                            (\p ->
                                case model.language of
                                    Fi ->
                                        p.nameFi

                                    En ->
                                        p.nameEn
                            )
                        |> Maybe.withDefault file

                noticeText =
                    case model.language of
                        Fi ->
                            "➕ Lisätty: " ++ partName

                        En ->
                            "➕ Added: " ++ partName

                updatedModel =
                    { model
                        | activePart = file
                        , activeTab = BuildBricksTab
                        , notice = Just noticeText
                    }
            in
            stay (runCore (Core.addPartAt file model.colour matrix) updatedModel)

        AddPartList files ->
            List.foldl
                (\file ( acc, accCmd, _ ) ->
                    let
                        ( added, addCmd, _ ) =
                            updateModel (AddPart file) acc
                    in
                    ( added, Cmd.batch [ accCmd, addCmd ], Stay )
                )
                ( model, Cmd.none, Stay )
                files

        SelectPlacements ids ->
            stay (selectPlacements ids { model | activeTab = BuildBricksTab, notice = Nothing })

        RemovePlacements ids ->
            let
                doc =
                    Core.getDocument model.studio

                -- Only parts placed straight in the main model are deleted; a
                -- part of the base (or of another group) is selected instead.
                removable =
                    Document.mainPlacements doc
                        |> List.filter
                            (\p ->
                                List.member p.id ids
                                    && not (Document.isSubmodelReference p doc)
                                    && not (isLockedBase model p)
                            )
                        |> List.map .id

                kept =
                    List.filter (\id -> not (List.member id removable)) ids

                closed =
                    { model | activeTab = BuildBricksTab, notice = Nothing }
            in
            if List.isEmpty removable then
                stay (selectPlacements ids closed)

            else
                let
                    ( selected, selectCmd ) =
                        runCore (Core.update (Core.Select (Set.fromList removable))) closed

                    ( removed, removeCmd ) =
                        runCore Core.deleteSelection selected

                    ( shown, showCmd ) =
                        if List.isEmpty kept then
                            ( removed, Cmd.none )

                        else
                            selectPlacements kept removed
                in
                ( shown, Cmd.batch [ selectCmd, removeCmd, showCmd ], Stay )

        SnapPlacements ids ->
            let
                doc =
                    Core.getDocument model.studio

                -- Each part placed straight in the main model moves onto the
                -- grid in one undo step (`Core.applyFixes`); runCore still
                -- drops the step if it would split the base.
                fixes =
                    Document.mainPlacements doc
                        |> List.filter
                            (\p ->
                                List.member p.id ids
                                    && not (Document.isSubmodelReference p doc)
                                    && not (isLockedBase model p)
                            )
                        |> List.map
                            (\p ->
                                { placement = p.id
                                , from = p.matrix
                                , to = Analyze.snapMatrix p.matrix
                                , reason = Health.SnapToStud
                                }
                            )

                closed =
                    { model | activeTab = BuildBricksTab, notice = Nothing }
            in
            if List.isEmpty fixes then
                stay (selectPlacements ids closed)

            else
                stay (runCore (Core.applyFixes fixes) closed)

        Turn direction ->
            stay (runCore (Core.turnSelected direction) model)

        Raise ->
            stay (runCore Core.raiseSelected model)

        Lower ->
            stay (runCore Core.lowerSelected model)

        CopySelected ->
            stay (runCore (\studio -> ( Core.copySelection studio, Cmd.none, [] )) model)

        PasteSelected ->
            stay (runCore Core.pasteClipboard model)

        DeleteSelected ->
            if baseSelected model then
                ( { model | notice = Just (keepBaseText model.language) }, Cmd.none, Stay )

            else
                stay (runCore Core.deleteSelection model)

        Undo ->
            stay (runCore Core.undo model)

        Redo ->
            stay (runCore Core.redo model)

        FitView ->
            let
                ( studio, cmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = studio }, Cmd.map StudioMsg cmd, Stay )

        Finish ->
            let
                ( saved, saveCmd ) =
                    saveSnapshot model
            in
            ( saved, saveCmd, Finished (Core.getDocument model.studio) )

        Leave ->
            ( model, Cmd.none, Quit )

        ThumbnailLoaded thumb ->
            ( { model | thumbnails = Dict.insert (PartCard.thumbnailKey thumb.file thumb.color) thumb.url model.thumbnails }
            , Cmd.none
            , Stay
            )

        SelectTab tab ->
            let
                -- Pressing the button of the open drawer closes it.
                nextTab =
                    if tab == model.activeTab then
                        BuildBricksTab

                    else
                        tab

                -- Opening the bases drawer saves the build first, so it is
                -- listed there, and later edits are saved as a new entry.
                ( snapshot, saveCmd ) =
                    if nextTab == BasesTab && model.activeTab /= BasesTab then
                        saveSnapshot model

                    else
                        ( model, Cmd.none )

                updated =
                    { snapshot | activeTab = nextTab, notice = Nothing }

                ( next, cmd ) =
                    runCore (Core.update (Core.ViewportMsg (Viewport.WindowResize model.width (canvasHeight updated)))) updated
            in
            ( next, Cmd.batch [ saveCmd, cmd ], Stay )

        SelectPreset preset ->
            let
                doc =
                    Document.parse preset.ldrText
            in
            stay (replaceBuild (Editor.baseOf doc) doc model)

        SelectBase base ->
            if Maybe.map .id model.base == Just base.id then
                -- The base the build stands on: keep the build.
                ( { model | activeTab = BuildBricksTab, notice = Nothing }, Cmd.none, Stay )

            else
                -- The build is saved first; it stays in the history below the bases.
                stay (replaceBuild (Just base) base.document model)

        RestoreBuild entry ->
            if entry.code == Ingest.encodeBuildLink (Core.getDocument model.studio) then
                -- Already the build on the stand.
                ( { model | activeTab = BuildBricksTab, notice = Nothing }, Cmd.none, Stay )

            else
                case Ingest.decodeRaceInput Nothing entry.code of
                    Ok decoded ->
                        let
                            base =
                                case List.filter (\b -> b.id == entry.base) Editor.bases |> List.head of
                                    Just found ->
                                        if Editor.baseIntact found decoded.document then
                                            Just found

                                        else
                                            Editor.baseOf decoded.document

                                    Nothing ->
                                        Editor.baseOf decoded.document

                            ( restored, restoreCmd ) =
                                replaceBuild base decoded.document model
                        in
                        ( { restored | notice = Just (RI18n.buildHistoryRestored model.language), savedCode = entry.code }, restoreCmd, Stay )

                    Err _ ->
                        -- Cannot be read any more: drop it from the list.
                        let
                            pruned =
                                BuildHistory.remove entry.code model.history
                        in
                        ( { model | history = pruned }, Ports.saveBuildHistory (BuildHistory.toJson pruned), Stay )

        ClockTick now ->
            ( { model | clock = Time.posixToMillis now }, Cmd.none, Stay )

        AutoSave revision now ->
            let
                ticked =
                    { model | clock = Time.posixToMillis now }
            in
            if revision == model.revision then
                stay (saveBuild ticked)

            else
                ( ticked, Cmd.none, Stay )

        PageVisibility Browser.Events.Hidden ->
            -- A phone may discard a hidden tab: save what is there now.
            stay (saveBuild model)

        PageVisibility Browser.Events.Visible ->
            ( model, Cmd.none, Stay )

        ModelPreviewLoaded picture ->
            ( { model | previews = Dict.insert picture.key picture.url model.previews }, Cmd.none, Stay )

        ToggleBaseLock ->
            if model.baseLocked then
                case model.base of
                    Just base ->
                        let
                            doc =
                                Core.getDocument model.studio

                            maybeBasePlacement =
                                Document.mainPlacements doc
                                    |> List.filter (Editor.isBasePlacement base)
                                    |> List.head
                        in
                        case maybeBasePlacement of
                            Just basePlacement ->
                                let
                                    unlockedModel =
                                        { model | baseLocked = False, notice = Nothing }

                                    ( selected, selCmd ) =
                                        runCore (Core.update (Core.Select (Set.singleton basePlacement.id))) unlockedModel

                                    ( exploded, expCmd ) =
                                        runCore (Core.update Core.ExplodeSelection) selected
                                in
                                ( { exploded | baseLocked = False, notice = Nothing }
                                , Cmd.batch [ selCmd, expCmd ]
                                , Stay
                                )

                            Nothing ->
                                ( { model | baseLocked = False, notice = Nothing }, Cmd.none, Stay )

                    Nothing ->
                        ( { model | baseLocked = False, notice = Nothing }, Cmd.none, Stay )

            else
                ( { model | baseLocked = True, notice = Nothing }, Cmd.none, Stay )

        PasteTextChanged textVal ->
            ( { model | pasteText = textVal, pasteError = Nothing }, Cmd.none, Stay )

        CopyBuildCode ->
            ( model
            , GamePorts.copyShareUrl (Ingest.encodeBuildLink (Core.getDocument model.studio))
            , Stay
            )

        PaletteLeft ->
            stay ( model, Cmd.none )

        CanvasTapped ->
            stay ( model, Cmd.none )

        CatalogFilterChanged query ->
            ( { model | catalogFilter = query }, Cmd.none, Stay )

        SubmitPaste ->
            case Ingest.decodeRaceInput Nothing model.pasteText of
                Ok decoded ->
                    stay (replaceBuild (Editor.baseOf decoded.document) decoded.document { model | pasteText = "", pasteError = Nothing })

                Err _ ->
                    ( { model | pasteError = Just (RI18n.pasteErrorFallback model.language) }, Cmd.none, Stay )

        RequestFileUpload ->
            ( model, Cmd.none, TriggerFileUpload )

        OpenQrScanner ->
            ( model, Cmd.none, TriggerQrScanner )

        LoadExternalDocument doc maybeBase ->
            let
                newBase =
                    case maybeBase of
                        Just b ->
                            Just b

                        Nothing ->
                            Editor.baseOf doc
            in
            stay (replaceBuild newBase doc { model | pasteText = "", pasteError = Nothing })


{-| Run a studio step, unless it would split the base.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore step model =
    let
        ( studio, cmd, events ) =
            step model.studio

        isAllowed =
            if not model.baseLocked then
                True

            else
                case model.base of
                    Just base ->
                        Editor.baseIntact base (Core.getDocument studio)

                    Nothing ->
                        True
    in
    if isAllowed then
        ( { model | studio = studio }
        , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
        )

    else
        ( { model | notice = Just (keepBaseText model.language) }, Cmd.none )


eventCmd : Core.Event -> Cmd Msg
eventCmd event =
    case event of
        Core.FlattenRequested payload ->
            GamePorts.requestGeometryFlatten payload

        _ ->
            Cmd.none


{-| Select the main-model placements that hold the flattened placements `ids`
(`Racer.Analyze` numbers the parts inside a group by their own ids, so a part
of the base selects the whole base).
-}
selectPlacements : List Int -> Model -> ( Model, Cmd Msg )
selectPlacements ids model =
    let
        doc =
            Core.getDocument model.studio

        holds p =
            List.member p.id ids
                || List.any (\inner -> List.member inner.id ids) (Document.constituentPlacements p.id doc)

        selection =
            Document.mainPlacements doc
                |> List.filter holds
                |> List.map .id
                |> Set.fromList
    in
    runCore (Core.update (Core.Select selection)) model


{-| Whether `placement` is the locked starter base.
-}
isLockedBase : Model -> Document.Placement -> Bool
isLockedBase model placement =
    model.baseLocked
        && (case model.base of
                Just base ->
                    Editor.isBasePlacement base placement

                Nothing ->
                    False
           )


baseSelected : Model -> Bool
baseSelected model =
    if not model.baseLocked then
        False

    else
        case model.base of
            Just base ->
                Core.getPlacements model.studio
                    |> List.any (\p -> Set.member p.id model.studio.selection && Editor.isBasePlacement base p)

            Nothing ->
                False


requestThumbnails : Model -> Cmd Msg
requestThumbnails model =
    let
        wanted =
            List.map (\p -> ( p.file, model.colour )) Editor.parts
                ++ List.map (\file -> ( file, model.colour )) missingPartFiles

        missing =
            dedupe wanted
                |> List.filter (\( file, color ) -> not (Dict.member (PartCard.thumbnailKey file color) model.thumbnails))
    in
    if List.isEmpty missing then
        Cmd.none

    else
        Ports.requestThumbnails
            (List.map
                (\( file, color ) ->
                    { file = file
                    , color = color
                    , rgb = Colors.resolveColor color 16 |> (\c -> [ c.r, c.g, c.b ])
                    }
                )
                missing
            )


{-| Parts the lint dialog offers to add when they are missing.
-}
missingPartFiles : List String
missingPartFiles =
    Analyze.wheelPairFiles ++ Analyze.turntablePairFiles ++ [ "3829c01.dat", "4079.dat" ]


dedupe : List a -> List a
dedupe items =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        items


{-| The studio's subscriptions, geometry worker, and thumbnails.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , GamePorts.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , GamePorts.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , Ports.thumbnailLoaded ThumbnailLoaded
        , Ports.modelThumbnailLoaded ModelPreviewLoaded
        , Browser.Events.onVisibilityChange PageVisibility
        , Time.every 30000 ClockTick
        ]



-- ── View ─────────────────────────────────────────────────────────────────────


{-| Height of bottom panel depending on active tab.
-}
tabDrawerHeight : Model -> BuilderTab -> Int
tabDrawerHeight model tab =
    case tab of
        BuildBricksTab ->
            dockHeight

        AllBricksTab ->
            dockHeight + drawerHeight model catalogDrawerHeight

        _ ->
            drawerHeight model 280


{-| Height of a drawer. On desktop-width screens it grows to nearly the full
window, leaving a strip of the 3D view above it; phones keep the compact height.
-}
drawerHeight : Model -> Int -> Int
drawerHeight model compact =
    if model.width >= 768 then
        max compact (model.height - 120)

    else
        -- Phones get twice the compact height, but leave room for the 3D view.
        max compact (min (2 * compact) (model.height - 200))


canvasHeight : Model -> Int
canvasHeight model =
    max 160 (model.height - tabDrawerHeight model model.activeTab)


{-| The builder view.
-}
view : Model -> Html Msg
view model =
    let
        analyzed =
            Analyze.analyzeDocument "Car" (Core.getDocument model.studio) model.partCache model.partIndex

        floorY =
            analyzed.spec.wheelContactYLdu
    in
    div
        [ Attr.class "brick-editor"
        , Attr.attribute "data-testid" "builder-page"
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "radial-gradient(ellipse 85% 65% at 50% 18%, #1f2e3d 0%, #0d1722 55%, #050b10 100%)"
        , Attr.style "color" "#f8fafc"
        , Attr.style "font-family" "var(--font-sans, system-ui, sans-serif)"
        , Attr.style "user-select" "none"
        , Attr.style "-webkit-user-select" "none"
        ]
        [ div
            [ Attr.attribute "data-testid" "builder-canvas"
            , Attr.style "position" "absolute"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" (px model.width)
            , Attr.style "height" (px (canvasHeight model))
            , Attr.style "touch-action" "none"
            , Html.Events.on "pointerdown" (Decode.succeed CanvasTapped)
            ]
            [ Html.map StudioMsg
                (Core.viewWithExtra
                    { width = model.width
                    , height = canvasHeight model
                    , extraEntities =
                        \camera aspect ->
                            GarageFloor.entities floorY camera aspect
                    }
                    model.studio
                )
            ]
        , topBar model
        , noticeView model
        , bottomPanel model
        ]


topBar : Model -> Html Msg
topBar model =
    let
        lang =
            model.language

        tabBtn tab label icon =
            let
                isActive =
                    model.activeTab == tab
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectTab tab)
                , Attr.attribute "aria-label" label
                , Attr.title label
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "width" "40px"
                , Attr.style "height" "40px"
                , Attr.style "flex-shrink" "0"
                , Attr.style "border-radius" "12px"
                , Attr.style "border"
                    (if isActive then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.18)"
                    )
                , Attr.style "background"
                    (if isActive then
                        Brand.legoYellow

                     else
                        "rgba(15, 23, 42, 0.82)"
                    )
                , Attr.style "color"
                    (if isActive then
                        Brand.legoBlack

                     else
                        "#e2e8f0"
                    )
                , Attr.style "cursor" "pointer"
                , Attr.style "pointer-events" "auto"
                , Attr.style "transition" "all 0.15s ease"
                ]
                [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]
    in
    div
        [ Attr.attribute "data-testid" "builder-topbar"
        , Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "max(8px, env(safe-area-inset-top)) max(8px, env(safe-area-inset-right)) 8px max(8px, env(safe-area-inset-left))"
        , Attr.style "pointer-events" "none"
        ]
        [ button
            (buttonStyle True
                ++ [ onClick Finish
                   , Attr.style "background" Brand.legoYellow
                   , Attr.style "color" Brand.legoBlack
                   , Attr.style "border" "none"
                   , Attr.attribute "aria-label" (tr lang "To the garage" "Talliin")
                   , Attr.title (tr lang "To the garage" "Talliin")
                   ]
            )
            [ FeatherIcons.check |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]
        , div
            [ Attr.attribute "data-testid" "topbar-tabs"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "gap" "4px"
            , Attr.style "flex" "1"
            , Attr.style "min-width" "0"
            , Attr.style "overflow-x" "auto"
            ]
            [ tabBtn BasesTab (RI18n.basesTab lang) FeatherIcons.truck
            , tabBtn ImportBuildTab (RI18n.openBuildTab lang) FeatherIcons.download
            , tabBtn ReviewTab (RI18n.reviewTab lang) FeatherIcons.award
            ]
        , div [ Attr.style "margin-left" "auto", Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "4px" ]
            [ iconButton (tr lang "Show the whole car" "Näytä koko auto") FeatherIcons.maximize (Just FitView)
            ]
        ]


noticeView : Model -> Html Msg
noticeView model =
    case model.notice of
        Just message ->
            div
                [ Attr.attribute "data-testid" "builder-notice"
                , Attr.attribute "role" "status"
                , Attr.style "position" "absolute"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translateX(-50%)"
                , Attr.style "top" "64px"
                , Attr.style "max-width" "calc(100% - 32px)"
                , Attr.style "padding" "6px 12px"
                , Attr.style "border-radius" "10px"
                , Attr.style "background" "rgba(15, 23, 42, 0.86)"
                , Attr.style "font-size" "13px"
                , Attr.style "text-align" "center"
                ]
                [ text message ]

        Nothing ->
            text ""


{-| Base lock toggle, shown at the far right of the palette row.
-}
lockButton : Model -> Html Msg
lockButton model =
    let
        lang =
            model.language

        locked =
            model.baseLocked

        label =
            if locked then
                tr lang "Base locked (click to unlock and edit chassis)" "Alusta lukittu (klikkaa avataksesi alustan muokkaus)"

            else
                tr lang "Base unlocked (click to lock chassis)" "Alusta avattu (klikkaa lukitaksesi alusta)"
    in
    button
        [ Attr.type_ "button"
        , onClick ToggleBaseLock
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "40px"
        , Attr.style "height" "40px"
        , Attr.style "flex-shrink" "0"
        , Attr.style "border-radius" "12px"
        , Attr.style "border"
            (if locked then
                "1px solid rgba(255, 255, 255, 0.2)"

             else
                "1px solid #f59e0b"
            )
        , Attr.style "background"
            (if locked then
                "rgba(15, 23, 42, 0.82)"

             else
                "rgba(245, 158, 11, 0.25)"
            )
        , Attr.style "color"
            (if locked then
                "#cbd5e1"

             else
                "#fbbf24"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , Attr.title label
        , Attr.attribute "aria-label" label
        ]
        [ (if locked then
            FeatherIcons.lock

           else
            FeatherIcons.unlock
          )
            |> FeatherIcons.withSize 18
            |> FeatherIcons.toHtml []
        ]


{-| Render bottom panel according to active tab.
-}
bottomPanel : Model -> Html Msg
bottomPanel model =
    case model.activeTab of
        BuildBricksTab ->
            dock model

        AllBricksTab ->
            dock model

        BasesTab ->
            viewBasesDrawer model

        ImportBuildTab ->
            viewImportDrawer model

        ReviewTab ->
            viewReviewDrawer model


dockHeight : Int
dockHeight =
    126


{-| Height of the full catalog panel, shown above the palette row while open.
-}
catalogDrawerHeight : Int
catalogDrawerHeight =
    240


dock : Model -> Html Msg
dock model =
    let
        catalogOpen =
            model.activeTab == AllBricksTab

        height =
            if catalogOpen then
                tabDrawerHeight model AllBricksTab

            else
                dockHeight
    in
    div
        [ Attr.attribute "data-testid" "builder-dock"
        , Attr.style "position" "absolute"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "height" ("calc(" ++ px height ++ " + env(safe-area-inset-bottom, 0px))")
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "6px max(8px, env(safe-area-inset-right)) max(6px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        , Attr.style "background" "rgba(15, 23, 42, 0.94)"
        , Attr.style "border-top" "1px solid rgba(148, 163, 184, 0.25)"
        ]
        ((if catalogOpen then
            [ viewAllBricksDrawer model ]

          else
            []
         )
            ++ [ modelingControls model
               , paletteRow model
               ]
        )


paletteRow : Model -> Html Msg
paletteRow model =
    let
        shownParts =
            (if List.member model.activePart model.recentParts then
                model.recentParts

             else
                model.activePart :: model.recentParts
            )
                |> List.take 7

        renderedBricks =
            List.map (\f -> partCard model (f == model.activePart) f) shownParts

        catalogOpen =
            model.activeTab == AllBricksTab

        allBricksBtn =
            button
                [ Attr.type_ "button"
                , onClick (SelectTab AllBricksTab)
                , Attr.style "flex-shrink" "0"
                , Attr.style "width" "56px"
                , Attr.style "height" "56px"
                , Attr.style "padding" "0"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "border-radius" "10px"
                , Attr.style "border" "2px dashed rgba(250, 200, 10, 0.6)"
                , Attr.style "background"
                    (if catalogOpen then
                        "rgba(250, 200, 10, 0.35)"

                     else
                        "rgba(250, 200, 10, 0.15)"
                    )
                , Attr.style "color" Brand.legoYellow
                , Attr.style "cursor" "pointer"
                , Attr.attribute "aria-label" (tr model.language "All bricks" "Kaikki palikat")
                , Attr.attribute "aria-pressed"
                    (if catalogOpen then
                        "true"

                     else
                        "false"
                    )
                , Attr.title (tr model.language "Browse all categorized bricks" "Selaa kaikkia osia kategorioittain")
                ]
                [ FeatherIcons.box |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [] ]
    in
    div
        [ Attr.attribute "data-testid" "palette-row"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "width" "100%"
        , Attr.style "box-sizing" "border-box"
        , onMouseLeave PaletteLeft
        ]
        [ colourRow model
        , div
            [ Attr.style "width" "1px"
            , Attr.style "height" "48px"
            , Attr.style "background" "rgba(255, 255, 255, 0.18)"
            , Attr.style "flex-shrink" "0"
            ]
            []
        , div
            [ Attr.class "palette-scroll-x"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "6px"
            , Attr.style "overflow-x" "auto"
            , Attr.style "overflow-y" "hidden"
            , Attr.style "flex" "1"
            , Attr.style "padding-bottom" "2px"
            , Attr.attribute "aria-label" (tr model.language "Bricks" "Palikat")
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "flex-shrink" "0"
                , Attr.style "margin" "0 auto"
                ]
                (allBricksBtn :: renderedBricks)
            ]
        , div
            [ Attr.style "width" "1px"
            , Attr.style "height" "48px"
            , Attr.style "background" "rgba(255, 255, 255, 0.18)"
            , Attr.style "flex-shrink" "0"
            ]
            []
        , lockButton model
        ]


modelingControls : Model -> Html Msg
modelingControls model =
    let
        lang =
            model.language

        hasSel =
            Core.hasSelection model.studio

        onBase =
            baseSelected model

        modelToolBtn label icon maybeMsg =
            let
                enabled =
                    maybeMsg /= Nothing

                clickAttrs =
                    case maybeMsg of
                        Just msg ->
                            [ onClick msg ]

                        Nothing ->
                            []
            in
            button
                ([ Attr.type_ "button"
                 , Attr.attribute "aria-label" label
                 , Attr.title label
                 , Attr.disabled (not enabled)
                 , Attr.style "display" "inline-flex"
                 , Attr.style "align-items" "center"
                 , Attr.style "justify-content" "center"
                 , Attr.style "width" "44px"
                 , Attr.style "height" "44px"
                 , Attr.style "flex-shrink" "0"
                 , Attr.style "border-radius" "10px"
                 , Attr.style "border"
                    (if enabled then
                        "1px solid rgba(255, 255, 255, 0.25)"

                     else
                        "1px solid rgba(255, 255, 255, 0.08)"
                    )
                 , Attr.style "background"
                    (if enabled then
                        "rgba(30, 41, 59, 0.85)"

                     else
                        "rgba(15, 23, 42, 0.4)"
                    )
                 , Attr.style "color"
                    (if enabled then
                        "#f8fafc"

                     else
                        "#475569"
                    )
                 , Attr.style "cursor"
                    (if enabled then
                        "pointer"

                     else
                        "default"
                    )
                 , Attr.style "opacity"
                    (if enabled then
                        "1"

                     else
                        "0.35"
                    )
                 , Attr.style "touch-action" "manipulation"
                 , Attr.style "transition" "all 0.15s ease"
                 ]
                    ++ clickAttrs
                )
                [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
    in
    div
        [ Attr.attribute "data-testid" "modelling-toolbar"
        , Attr.attribute "role" "toolbar"
        , Attr.attribute "aria-label" (tr lang "Part modeling tools" "Osan muokkaustyökalut")
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "6px"
        , Attr.style "flex-shrink" "0"
        ]
        [ modelToolBtn (tr lang "Undo" "Kumoa") FeatherIcons.cornerUpLeft (enabledIf (Core.canUndo model.studio) Undo)
        , modelToolBtn (tr lang "Redo" "Tee uudelleen") FeatherIcons.cornerUpRight (enabledIf (Core.canRedo model.studio) Redo)
        , modelToolBtn (tr lang "Turn left" "Käännä vasemmalle") FeatherIcons.rotateCcw (enabledIf hasSel (Turn TurnLeft))
        , modelToolBtn (tr lang "Turn right" "Käännä oikealle") FeatherIcons.rotateCw (enabledIf hasSel (Turn TurnRight))
        , modelToolBtn (tr lang "Raise" "Nosta") FeatherIcons.arrowUp (enabledIf hasSel Raise)
        , modelToolBtn (tr lang "Lower" "Laske") FeatherIcons.arrowDown (enabledIf hasSel Lower)
        , if model.width >= 640 then
            modelToolBtn (tr lang "Copy" "Kopioi") FeatherIcons.copy (enabledIf hasSel CopySelected)

          else
            text ""
        , if model.width >= 640 then
            modelToolBtn (tr lang "Paste" "Liitä") FeatherIcons.clipboard (Just PasteSelected)

          else
            text ""
        , modelToolBtn
            (if onBase then
                tr lang "The base cannot be deleted" "Alustaa ei voi poistaa"

             else
                tr lang "Delete" "Poista"
            )
            FeatherIcons.trash2
            (enabledIf (hasSel && not onBase) DeleteSelected)
        ]


colourRow : Model -> Html Msg
colourRow model =
    div
        [ Attr.attribute "role" "listbox"
        , Attr.attribute "aria-label" (tr model.language "Colour" "Väri")
        , Attr.attribute "data-testid" "color-palette"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(4, 26px)"
        , Attr.style "grid-template-rows" "repeat(2, 26px)"
        , Attr.style "gap" "4px"
        , Attr.style "flex-shrink" "0"
        ]
        (List.map (swatchView model) Editor.palette)


swatchView : Model -> Editor.Swatch -> Html Msg
swatchView model swatch =
    let
        selected =
            swatch.code == model.colour

        name =
            tr model.language swatch.nameEn swatch.nameFi

        cssColour =
            Dict.get swatch.code ColorData.colorTable
                |> Maybe.map Colours.css
                |> Maybe.withDefault "#888"
    in
    button
        [ Attr.type_ "button"
        , onClick (PickColour swatch.code)
        , Attr.attribute "role" "option"
        , Attr.attribute "aria-selected"
            (if selected then
                "true"

             else
                "false"
            )
        , Attr.attribute "aria-label" name
        , Attr.title name
        , Attr.style "width" "26px"
        , Attr.style "height" "26px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background" cssColour
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "border"
            (if selected then
                "2.5px solid " ++ Brand.legoYellow

             else
                "1.5px solid rgba(255, 255, 255, 0.4)"
            )
        , Attr.style "box-shadow"
            (if selected then
                "0 0 0 1.5px " ++ Brand.legoBlack ++ " inset"

             else
                "none"
            )
        ]
        []


partCard : Model -> Bool -> String -> Html Msg
partCard model isActive file =
    let
        maybeEntry =
            Editor.parts
                |> List.filter (\p -> p.file == file)
                |> List.head

        name =
            case maybeEntry of
                Just entry ->
                    tr model.language entry.nameEn entry.nameFi

                Nothing ->
                    file

        number =
            String.replace ".dat" "" file

        cardAttrs =
            if isActive then
                [ Attr.style "border" ("2px solid " ++ Brand.legoYellow)
                , Attr.style "box-shadow" "0 0 8px rgba(250, 200, 10, 0.4)"
                ]

            else
                []
    in
    PartCard.compactView
        { imageUrl = Dict.get (PartCard.thumbnailKey file model.colour) model.thumbnails
        , tooltip =
            name
                ++ " ("
                ++ number
                ++ ")"
                ++ (if isActive then
                        " [" ++ tr model.language "Active" "Valittu" ++ "]"

                    else
                        ""
                   )
        , ariaLabel =
            (if isActive then
                "[★] "

             else
                ""
            )
                ++ tr model.language "Add " "Lisää "
                ++ name
        , badge =
            if isActive then
                Just { text = "★", muted = False }

            else
                Nothing
        , onClick = Just (AddPart file)
        , attributes = cardAttrs
        }


{-| Drawer of the starter bases, each with its picture, and below them the
saved builds (`historyCards`).
-}
viewBasesDrawer : Model -> Html Msg
viewBasesDrawer model =
    let
        lang =
            model.language

        renderBaseCard base =
            let
                isCurrent =
                    case model.base of
                        Just b ->
                            base.id == b.id

                        Nothing ->
                            False

                name =
                    tr lang base.nameEn base.nameFi
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectBase base)
                , Attr.attribute "data-base" base.id
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "8px"
                , Attr.style "padding" "10px 12px"
                , Attr.style "border-radius" "12px"
                , Attr.style "border"
                    (if isCurrent then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.15)"
                    )
                , Attr.style "background"
                    (if isCurrent then
                        "rgba(250, 200, 10, 0.16)"

                     else
                        "rgba(15, 23, 42, 0.7)"
                    )
                , Attr.style "color" Brand.legoWhite
                , Attr.style "cursor" "pointer"
                , Attr.style "text-align" "left"
                , Attr.style "touch-action" "manipulation"
                ]
                [ cardPicture name
                    (Dict.get (basePreviewKey base) model.previews)
                    (Just ("base-previews/" ++ base.id ++ ".png"))
                , div []
                    [ div [ Attr.style "font-weight" "800", Attr.style "font-size" "13px" ] [ text ("🔨 " ++ name) ]
                    , div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "margin-top" "2px" ]
                        [ text (String.fromInt base.partCount ++ tr lang " parts • chassis with wheels" " osaa • alusta pyörineen") ]
                    ]
                ]
    in
    div (drawerStyle (drawerHeight model 280) ++ [ Attr.attribute "data-testid" "bases-drawer" ])
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px", Attr.style "height" "100%", Attr.style "overflow-y" "auto" ]
            [ drawerHeading (tr lang "Choose Starter Chassis" "Valitse aloituspohja")
            , div cardGridStyle
                (List.map renderBaseCard Editor.bases)
            , drawerHeading (RI18n.buildHistoryHeading lang)
            , historyCards model
            ]
        ]


drawerHeading : String -> Html msg
drawerHeading label =
    h3 [ Attr.style "font-size" "13px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase", Attr.style "margin" "0" ]
        [ text label ]


{-| The card grid of the bases drawer: as many columns as fit, so a phone in
landscape shows a row of small cards rather than two huge ones.
-}
cardGridStyle : List (Html.Attribute msg)
cardGridStyle =
    [ Attr.style "display" "grid"
    , Attr.style "grid-template-columns" "repeat(auto-fill, minmax(132px, 1fr))"
    , Attr.style "gap" "10px"
    ]


{-| The square picture of a base or saved build card: the rendered picture
when it has arrived, else the bundled one (`fallback`, drawn as a background
so a missing file shows the empty frame rather than a broken image), else a
hammer.
-}
cardPicture : String -> Maybe String -> Maybe String -> Html msg
cardPicture name rendered fallback =
    div
        [ Attr.attribute "data-testid" "card-picture"
        , Attr.style "width" "100%"
        , Attr.style "aspect-ratio" "1 / 1"
        , Attr.style "border-radius" "8px"
        , Attr.style "overflow" "hidden"
        , Attr.style "background-color" "#070c14"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.08)"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ case ( rendered, fallback ) of
            ( Just url, _ ) ->
                Html.img
                    [ Attr.src url
                    , Attr.alt name
                    , Attr.attribute "data-preview" "rendered"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "object-fit" "contain"
                    ]
                    []

            ( Nothing, Just url ) ->
                div
                    [ Attr.attribute "role" "img"
                    , Attr.attribute "aria-label" name
                    , Attr.attribute "data-preview" "bundled"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "background-image" ("url(\"" ++ url ++ "\")")
                    , Attr.style "background-size" "contain"
                    , Attr.style "background-position" "center"
                    , Attr.style "background-repeat" "no-repeat"
                    ]
                    []

            ( Nothing, Nothing ) ->
                span [ Attr.style "font-size" "28px", Attr.style "opacity" "0.6" ] [ text "🔨" ]
        ]


{-| The saved builds (`Game.BuildHistory`), newest first, as cards like the
base cards: a picture of the build, its base, part count and age. A tap opens
the build again; the build open now is marked.
-}
historyCards : Model -> Html Msg
historyCards model =
    let
        lang =
            model.language

        card entry =
            let
                baseLabel =
                    Editor.bases
                        |> List.filter (\b -> b.id == entry.base)
                        |> List.head
                        |> Maybe.map (\b -> tr lang b.nameEn b.nameFi)
                        |> Maybe.withDefault (tr lang "Car" "Auto")

                isCurrent =
                    entry.code == model.savedCode
            in
            button
                [ Attr.type_ "button"
                , onClick (RestoreBuild entry)
                , Attr.attribute "data-history-entry" (String.fromInt entry.savedAt)
                , Attr.attribute "aria-label" (RI18n.restoreBuildLabel lang baseLabel)
                , Attr.title (RI18n.restoreBuildLabel lang baseLabel)
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "8px"
                , Attr.style "padding" "10px 12px"
                , Attr.style "border-radius" "12px"
                , Attr.style "border"
                    (if isCurrent then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.15)"
                    )
                , Attr.style "background" "rgba(15, 23, 42, 0.7)"
                , Attr.style "color" Brand.legoWhite
                , Attr.style "cursor" "pointer"
                , Attr.style "text-align" "left"
                , Attr.style "touch-action" "manipulation"
                ]
                [ cardPicture baseLabel (Dict.get entry.code model.previews) Nothing
                , div []
                    [ div [ Attr.style "font-weight" "800", Attr.style "font-size" "13px" ] [ text ("↺ " ++ baseLabel) ]
                    , div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "margin-top" "2px" ]
                        [ text
                            (RI18n.buildHistoryParts lang entry.parts
                                ++ " • "
                                ++ (if isCurrent then
                                        RI18n.buildHistoryCurrent lang

                                    else
                                        RI18n.buildHistoryAge lang (max 0 (model.clock - entry.savedAt))
                                   )
                            )
                        ]
                    ]
                ]
    in
    case BuildHistory.entries model.history of
        [] ->
            div [ Attr.attribute "data-testid" "build-history-empty", Attr.style "font-size" "12px", Attr.style "color" "#94a3b8" ]
                [ text (RI18n.buildHistoryEmpty lang) ]

        saved ->
            div (Attr.attribute "data-testid" "build-history" :: cardGridStyle)
                (List.map card saved)


{-| Category definitions for the categorized All-Bricks drawer.
-}
partCategories : List { titleEn : String, titleFi : String, partFiles : List String }
partCategories =
    [ { titleEn = "Bricks"
      , titleFi = "Peruspalikat"
      , partFiles = [ "3005.dat", "3004.dat", "3622.dat", "3010.dat", "3009.dat", "3003.dat", "3002.dat", "3001.dat" ]
      }
    , { titleEn = "Plates"
      , titleFi = "Levyt"
      , partFiles = [ "3024.dat", "3023b.dat", "3623.dat", "3710.dat", "3666.dat", "3022.dat", "3021.dat", "3020.dat", "3795.dat", "3034.dat", "3031.dat" ]
      }
    , { titleEn = "Slopes & Curves"
      , titleFi = "Viisteet ja kaaret"
      , partFiles = [ "3040b.dat", "3039.dat", "3298.dat", "54200.dat", "85984.dat", "11477.dat", "15068.dat", "50950.dat", "3665b.dat" ]
      }
    , { titleEn = "Tiles"
      , titleFi = "Laatat"
      , partFiles = [ "3070b.dat", "3069b.dat", "2431.dat", "3068b.dat" ]
      }
    , { titleEn = "Car Parts"
      , titleFi = "Auton erikoisosat"
      , partFiles = [ "3829c01.dat", "4079.dat", "3823.dat", "50745.dat", "4070.dat", "2412b.dat", "3700.dat" ]
      }
    , { titleEn = "Wheels & Axles"
      , titleFi = "Pyörät ja akselit"
      , partFiles = [ "3679.dat", "3680.dat", "4600.dat", "4488.dat", "4624.dat", "3641.dat", "6014b.dat", "56890.dat", "55981.dat", "56891.dat" ]
      }
    ]


{-| Drawer displaying all available bricks grouped into categories.
-}
viewAllBricksDrawer : Model -> Html Msg
viewAllBricksDrawer model =
    let
        lang =
            model.language

        filteredParts cat =
            Editor.parts
                |> List.filter (\p -> List.member p.file cat.partFiles)
                |> List.filter (matchesFilter model.catalogFilter)

        categorySection cat =
            let
                title =
                    tr lang cat.titleEn cat.titleFi

                catParts =
                    filteredParts cat

                renderCard entry =
                    let
                        name =
                            tr lang entry.nameEn entry.nameFi

                        number =
                            String.replace ".dat" "" entry.file

                        thumb =
                            Dict.get (PartCard.thumbnailKey entry.file model.colour) model.thumbnails
                    in
                    button
                        [ Attr.type_ "button"
                        , onClick (AddPart entry.file)
                        , Attr.style "display" "flex"
                        , Attr.style "flex-direction" "column"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        , Attr.style "gap" "4px"
                        , Attr.style "padding" "8px 6px"
                        , Attr.style "border-radius" "10px"
                        , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.12)"
                        , Attr.style "color" "#f8fafc"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "transition" "all 0.15s ease"
                        , Attr.title (name ++ " (" ++ number ++ ")")
                        ]
                        [ div
                            [ Attr.style "width" "64px"
                            , Attr.style "height" "64px"
                            , Attr.style "box-sizing" "border-box"
                            , Attr.style "padding" "4px"
                            , Attr.style "display" "flex"
                            , Attr.style "align-items" "center"
                            , Attr.style "justify-content" "center"
                            , Attr.style "border-radius" "8px"
                            , Attr.style "background"
                                "radial-gradient(circle at 50% 45%, var(--color-bg-subtle) 0%, color-mix(in srgb, var(--color-border-default) 80%, var(--color-text-subtle)) 75%)"
                            , Attr.style "box-shadow" "inset 0 0 0 1px rgba(255, 255, 255, 0.35)"
                            ]
                            [ case thumb of
                                Just url ->
                                    Html.img
                                        [ Attr.src url
                                        , Attr.alt name
                                        , Attr.style "max-width" "100%"
                                        , Attr.style "max-height" "100%"
                                        , Attr.style "object-fit" "contain"
                                        ]
                                        []

                                Nothing ->
                                    span [ Attr.style "font-size" "20px" ] [ text "🧊" ]
                            ]
                        , div [ Attr.style "font-size" "11px", Attr.style "font-weight" "700", Attr.style "text-align" "center", Attr.style "line-height" "1.2" ]
                            [ text name ]
                        , div [ Attr.style "font-size" "9px", Attr.style "color" "#94a3b8" ]
                            [ text ("#" ++ number) ]
                        ]
            in
            div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "8px" ]
                [ div [ Attr.style "font-size" "12px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase", Attr.style "letter-spacing" "0.5px" ]
                    [ text title ]
                , div [ Attr.style "display" "grid", Attr.style "grid-template-columns" "repeat(auto-fill, minmax(110px, 1fr))", Attr.style "gap" "8px" ]
                    (List.map renderCard catParts)
                ]
    in
    div
        [ Attr.attribute "data-testid" "all-bricks-drawer"
        , Attr.style "flex-shrink" "0"
        , Attr.style "height" (px (drawerHeight model catalogDrawerHeight))
        , Attr.style "box-sizing" "border-box"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "10px 12px"
        , Attr.style "border-radius" "12px"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.12)"
        , Attr.style "background" "rgba(5, 19, 29, 0.96)"
        ]
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "12px" ]
            [ div
                [ Attr.style "position" "sticky"
                , Attr.style "top" "0"
                , Attr.style "z-index" "1"
                , Attr.style "display" "flex"
                , Attr.style "justify-content" "center"
                , Attr.style "padding-bottom" "2px"
                , Attr.style "background" "rgba(5, 19, 29, 0.96)"
                ]
                [ input
                    [ Attr.type_ "search"
                    , Attr.value model.catalogFilter
                    , onInput CatalogFilterChanged
                    , Attr.placeholder (tr lang "Filter by name or number" "Suodata nimellä tai numerolla")
                    , Attr.attribute "aria-label" (tr lang "Filter bricks" "Suodata palikoita")
                    , Attr.attribute "autocomplete" "off"
                    , Attr.attribute "autocapitalize" "none"
                    , Attr.attribute "spellcheck" "false"
                    , Attr.style "width" "100%"
                    , Attr.style "max-width" "280px"
                    , Attr.style "box-sizing" "border-box"
                    , Attr.style "padding" "8px 12px"
                    , Attr.style "border-radius" "10px"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.25)"
                    , Attr.style "background" "rgba(15, 23, 42, 0.85)"
                    , Attr.style "color" "#f8fafc"
                    , Attr.style "font-size" "14px"
                    ]
                    []
                ]
            , div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "16px" ]
                (partCategories
                    |> List.filter (\cat -> not (List.isEmpty (filteredParts cat)))
                    |> List.map categorySection
                )
            ]
        ]


{-| Whether a catalog part matches the filter text, by English or Finnish name or by number.
-}
matchesFilter : String -> PartEntry -> Bool
matchesFilter query entry =
    let
        q =
            String.toLower (String.trim query)
    in
    q
        == ""
        || String.contains q (String.toLower entry.nameEn)
        || String.contains q (String.toLower entry.nameFi)
        || String.contains q (String.replace ".dat" "" entry.file)


{-| Drawer for pasting or loading build files / QR scans.
-}
viewImportDrawer : Model -> Html Msg
viewImportDrawer model =
    let
        lang =
            model.language
    in
    div (drawerStyle (drawerHeight model 280) ++ [ Attr.attribute "data-testid" "import-drawer" ])
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px", Attr.style "max-width" "680px", Attr.style "margin" "0 auto" ]
            [ h3 [ Attr.style "font-size" "13px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase" ]
                [ text (RI18n.openBuildTab lang) ]
            , div [ Attr.style "display" "flex", Attr.style "gap" "8px" ]
                [ textarea
                    [ Attr.placeholder (RI18n.pastePlaceholder lang)
                    , Attr.value model.pasteText
                    , onInput PasteTextChanged
                    , Attr.style "flex" "1"
                    , Attr.style "height" "70px"
                    , Attr.style "padding" "8px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "font-family" "monospace"
                    , Attr.style "font-size" "12px"
                    , Attr.style "resize" "none"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick SubmitPaste
                    , Attr.style "padding" "0 16px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    , Attr.style "font-weight" "800"
                    , Attr.style "font-size" "13px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "border" "none"
                    ]
                    [ text (RI18n.pasteButtonLabel lang) ]
                ]
            , button
                [ Attr.type_ "button"
                , onClick CopyBuildCode
                , Attr.attribute "data-testid" "copy-build-code"
                , Attr.style "align-self" "flex-start"
                , Attr.style "padding" "6px 12px"
                , Attr.style "border-radius" "8px"
                , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                , Attr.style "color" Brand.legoWhite
                , Attr.style "font-weight" "700"
                , Attr.style "font-size" "12px"
                , Attr.style "cursor" "pointer"
                , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                ]
                [ text (RI18n.copyBuildCodeButton lang) ]
            , case model.pasteError of
                Just err ->
                    div [ Attr.style "color" "#ef4444", Attr.style "font-size" "12px" ] [ text err ]

                Nothing ->
                    text ""
            , div [ Attr.style "display" "flex", Attr.style "gap" "10px", Attr.style "align-items" "center", Attr.style "padding-top" "6px" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick RequestFileUpload
                    , Attr.style "padding" "8px 14px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" "rgba(255, 255, 255, 0.1)"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "font-size" "12px"
                    , Attr.style "font-weight" "600"
                    , Attr.style "cursor" "pointer"
                    ]
                    [ text ("📂 " ++ RI18n.dropZoneButton lang) ]
                , button
                    [ Attr.type_ "button"
                    , onClick OpenQrScanner
                    , Attr.style "padding" "8px 14px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" "rgba(255, 255, 255, 0.1)"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "font-size" "12px"
                    , Attr.style "font-weight" "600"
                    , Attr.style "cursor" "pointer"
                    ]
                    [ text "📷 Skannaa QR-koodi" ]
                ]
            ]
        ]


{-| Drawer showing car evaluation, telemetry specs, helper actions, and race CTA.
-}
viewReviewDrawer : Model -> Html Msg
viewReviewDrawer model =
    let
        lang =
            model.language

        title =
            case model.base of
                Just b ->
                    baseName lang b

                Nothing ->
                    tr lang "Custom Soapbox" "Oma mäkiauto"

        doc =
            Core.getDocument model.studio

        allParts =
            Document.flattenedPlacements doc |> List.map .part

        hasSteeringWheel =
            List.member "3829c01.dat" allParts

        hasSeat =
            List.member "4079.dat" allParts

        analyzed =
            Analyze.analyzeDocument title doc model.partCache model.partIndex

        spec =
            analyzed.spec

        hasBlocking =
            List.any (\i -> i.severity == Blocker) spec.issues

        ready =
            spec.isDrivable && not hasBlocking

        starCount =
            Analyze.starRating spec.issues spec.isDrivable

        statusChip =
            div
                [ Attr.style "padding" "4px 10px"
                , Attr.style "border-radius" "8px"
                , Attr.style "font-size" "11px"
                , Attr.style "font-weight" "800"
                , Attr.style "white-space" "nowrap"
                , Attr.style "background"
                    (if ready then
                        "rgba(34, 197, 94, 0.22)"

                     else if hasBlocking then
                        "rgba(239, 68, 68, 0.22)"

                     else
                        "rgba(245, 158, 11, 0.22)"
                    )
                , Attr.style "color"
                    (if ready then
                        "#22c55e"

                     else if hasBlocking then
                        "#ef4444"

                     else
                        "#f59e0b"
                    )
                ]
                [ text
                    (if ready then
                        RI18n.readyToRaceBanner lang

                     else if hasBlocking then
                        RI18n.needsFixBanner lang

                     else
                        RI18n.needsPolishBanner lang
                    )
                ]

        stars =
            div
                [ Attr.attribute "role" "img"
                , Attr.attribute "aria-label" (tr lang "Quality" "Laatu" ++ ": " ++ String.fromInt starCount ++ " / 5")
                , Attr.style "display" "flex"
                , Attr.style "gap" "4px"
                , Attr.style "font-size" "28px"
                , Attr.style "line-height" "1"
                ]
                (List.map
                    (\n ->
                        span
                            [ Attr.style "color"
                                (if n <= starCount then
                                    "#facc15"

                                 else
                                    "#334155"
                                )
                            ]
                            [ text "★" ]
                    )
                    (List.range 1 5)
                )

        fact lbl val =
            div
                [ Attr.style "flex" "1"
                , Attr.style "min-width" "0"
                , Attr.style "padding" "8px 10px"
                , Attr.style "border-radius" "10px"
                , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.08)"
                ]
                [ div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8" ] [ text lbl ]
                , div [ Attr.style "font-size" "15px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoWhite, Attr.style "overflow-wrap" "anywhere" ] [ text val ]
                ]

        -- Icon button that adds missing parts, drawn with the first part's real
        -- LDraw thumbnail.
        helperBtn label partFiles =
            let
                partFile =
                    List.head partFiles |> Maybe.withDefault ""
            in
            button
                [ Attr.type_ "button"
                , onClick (AddPartList partFiles)
                , Attr.title label
                , Attr.attribute "aria-label" label
                , Attr.style "position" "relative"
                , Attr.style "width" "56px"
                , Attr.style "height" "56px"
                , Attr.style "padding" "4px"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "border-radius" "10px"
                , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                , Attr.style "border" "1px solid rgba(250, 200, 10, 0.5)"
                , Attr.style "cursor" "pointer"
                ]
                [ case Dict.get (PartCard.thumbnailKey partFile model.colour) model.thumbnails of
                    Just url ->
                        Html.img
                            [ Attr.src url
                            , Attr.alt label
                            , Attr.style "max-width" "100%"
                            , Attr.style "max-height" "100%"
                            , Attr.style "object-fit" "contain"
                            ]
                            []

                    Nothing ->
                        span [ Attr.style "font-size" "18px" ] [ text "🧱" ]
                , span
                    [ Attr.style "position" "absolute"
                    , Attr.style "top" "-6px"
                    , Attr.style "right" "-6px"
                    , Attr.style "width" "18px"
                    , Attr.style "height" "18px"
                    , Attr.style "border-radius" "50%"
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    , Attr.style "font-size" "13px"
                    , Attr.style "font-weight" "900"
                    ]
                    [ text "+" ]
                ]

        fixLabel : ValidationIssue -> String
        fixLabel issue =
            case issue.code of
                TooFewWheels _ ->
                    tr lang "Add wheels" "Lisää pyörät"

                FixedAxleTip ->
                    tr lang "Add turntable" "Lisää kääntöpöytä"

                other ->
                    RI18n.validationTitle lang other

        fixButtons =
            List.filterMap
                (\issue ->
                    case issue.fix of
                        Just (AddParts files) ->
                            Just (helperBtn (fixLabel issue) files)

                        _ ->
                            Nothing
                )
                spec.issues

        missingButtons =
            fixButtons
                ++ List.filterMap identity
                    [ if not hasSteeringWheel then
                        Just (helperBtn (tr lang "Add steering wheel" "Lisää ratti") [ "3829c01.dat" ])

                      else
                        Nothing
                    , if not hasSeat then
                        Just (helperBtn (tr lang "Add seat" "Lisää istuin") [ "4079.dat" ])

                      else
                        Nothing
                    ]

        missingSection =
            if List.isEmpty missingButtons then
                text ""

            else
                div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "6px" ]
                    [ sectionLabel (tr lang "Missing parts" "Puuttuvat osat")
                    , div [ Attr.style "display" "flex", Attr.style "flex-wrap" "wrap", Attr.style "gap" "8px" ] missingButtons
                    ]

        fixButton : Fix -> Html Msg
        fixButton fix =
            let
                ( label, msg ) =
                    case fix of
                        AddParts files ->
                            ( tr lang "Add" "Lisää", AddPartList files )

                        Select ids ->
                            ( tr lang "Show" "Näytä", SelectPlacements ids )

                        Remove ids ->
                            ( tr lang "Remove" "Poista", RemovePlacements ids )

                        Snap ids ->
                            ( tr lang "Snap" "Kohdista", SnapPlacements ids )
            in
            button
                [ Attr.type_ "button"
                , onClick msg
                , Attr.style "flex-shrink" "0"
                , Attr.style "padding" "4px 10px"
                , Attr.style "border-radius" "8px"
                , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                , Attr.style "border" "1px solid rgba(250, 200, 10, 0.5)"
                , Attr.style "color" Brand.legoYellow
                , Attr.style "font-size" "12px"
                , Attr.style "font-weight" "800"
                , Attr.style "cursor" "pointer"
                ]
                [ text label ]

        severityColour issue =
            if issue.severity == Blocker then
                "#ef4444"

            else if issue.severity == Bonus then
                "#22c55e"

            else
                "#f59e0b"

        issuesSection =
            if List.isEmpty spec.issues then
                div [ Attr.style "font-size" "13px", Attr.style "color" "#22c55e" ]
                    [ text (tr lang "No issues found" "Ei huomautuksia") ]

            else
                div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "6px" ]
                    (List.map
                        (\issue ->
                            div
                                [ Attr.style "display" "flex"
                                , Attr.style "align-items" "center"
                                , Attr.style "gap" "8px"
                                , Attr.style "font-size" "13px"
                                , Attr.style "color" "#e2e8f0"
                                ]
                                [ span
                                    [ Attr.style "width" "8px"
                                    , Attr.style "height" "8px"
                                    , Attr.style "border-radius" "50%"
                                    , Attr.style "flex-shrink" "0"
                                    , Attr.style "background" (severityColour issue)
                                    ]
                                    []
                                , div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "2px", Attr.style "flex" "1", Attr.style "min-width" "0" ]
                                    [ text (RI18n.validationTitle lang issue.code)
                                    , case RI18n.validationDetail lang issue.code of
                                        Just detail ->
                                            span [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8" ] [ text detail ]

                                        Nothing ->
                                            text ""
                                    ]
                                , case issue.fix of
                                    Just fix ->
                                        fixButton fix

                                    Nothing ->
                                        text ""
                                ]
                        )
                        spec.issues
                    )
    in
    div (drawerStyle (drawerHeight model 280) ++ [ Attr.attribute "data-testid" "review-drawer" ])
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "12px", Attr.style "height" "100%", Attr.style "overflow-y" "auto" ]
            [ div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "justify-content" "space-between", Attr.style "gap" "8px" ]
                [ h3 [ Attr.style "font-size" "14px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "margin" "0" ]
                    [ text title ]
                , statusChip
                ]
            , stars
            , div [ Attr.style "display" "flex", Attr.style "gap" "8px" ]
                [ fact (RI18n.garageWheelsLabel lang) (String.fromInt (List.length spec.wheels) ++ " kpl")
                , fact (RI18n.garageSteeringLabel lang) (RI18n.steeringMechName lang spec.steering)
                , fact (RI18n.garageMassLabel lang) (String.fromInt (round (spec.totalMassKg * 1000.0)) ++ " g")
                ]
            , missingSection
            , issuesSection
            , if ready then
                button
                    [ Attr.type_ "button"
                    , onClick Finish
                    , Attr.style "width" "100%"
                    , Attr.style "padding" "10px 16px"
                    , Attr.style "border-radius" "12px"
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    , Attr.style "font-size" "14px"
                    , Attr.style "font-weight" "800"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "border" "none"
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "gap" "8px"
                    , Attr.style "box-shadow" "0 4px 14px rgba(0, 0, 0, 0.3)"
                    ]
                    [ FeatherIcons.play |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                    , span [] [ text (RI18n.startRaceButton lang) ]
                    ]

              else
                text ""
            ]
        ]


sectionLabel : String -> Html msg
sectionLabel label =
    div [ Attr.style "font-size" "11px", Attr.style "font-weight" "800", Attr.style "color" "#94a3b8", Attr.style "text-transform" "uppercase", Attr.style "letter-spacing" "0.5px" ]
        [ text label ]


drawerStyle : Int -> List (Html.Attribute Msg)
drawerStyle heightPx =
    [ Attr.style "position" "absolute"
    , Attr.style "left" "0"
    , Attr.style "right" "0"
    , Attr.style "bottom" "0"
    , Attr.style "height" (px heightPx)
    , Attr.style "box-sizing" "border-box"
    , Attr.style "padding" "12px max(16px, env(safe-area-inset-right)) max(12px, env(safe-area-inset-bottom)) max(16px, env(safe-area-inset-left))"
    , Attr.style "background" "rgba(5, 19, 29, 0.96)"
    , Attr.style "border-top" "1px solid rgba(255, 255, 255, 0.15)"
    , Attr.style "box-shadow" "0 -4px 16px rgba(0, 0, 0, 0.4)"
    ]


iconButton : String -> FeatherIcons.Icon -> Maybe Msg -> Html Msg
iconButton label icon maybeMsg =
    button
        (buttonStyle (maybeMsg /= Nothing)
            ++ [ Attr.attribute "aria-label" label
               , Attr.title label
               , Attr.disabled (maybeMsg == Nothing)
               ]
            ++ (case maybeMsg of
                    Just msg ->
                        [ onClick msg ]

                    Nothing ->
                        []
               )
        )
        [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]


buttonStyle : Bool -> List (Html.Attribute Msg)
buttonStyle enabled =
    [ Attr.type_ "button"
    , Attr.style "pointer-events" "auto"
    , Attr.style "display" "inline-flex"
    , Attr.style "align-items" "center"
    , Attr.style "justify-content" "center"
    , Attr.style "min-width" "40px"
    , Attr.style "height" "40px"
    , Attr.style "flex-shrink" "0"
    , Attr.style "border-radius" "12px"
    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.35)"
    , Attr.style "background" "rgba(15, 23, 42, 0.82)"
    , Attr.style "color" "#f8fafc"
    , Attr.style "cursor"
        (if enabled then
            "pointer"

         else
            "default"
        )
    , Attr.style "opacity"
        (if enabled then
            "1"

         else
            "0.4"
        )
    , Attr.style "touch-action" "manipulation"
    ]


enabledIf : Bool -> Msg -> Maybe Msg
enabledIf condition msg =
    if condition then
        Just msg

    else
        Nothing


px : Int -> String
px n =
    String.fromInt n ++ "px"


baseName : Language -> Base -> String
baseName lang base =
    tr lang base.nameEn base.nameFi


keepBaseText : Language -> String
keepBaseText lang =
    tr lang "The base with the wheels stays in one piece." "Alusta pyörineen pysyy yhtenä kappaleena."


keepBaseColours : Language -> String
keepBaseColours lang =
    tr lang "The base keeps its colours. Pick a part you added to colour it." "Alusta pitää värinsä. Valitse lisäämäsi osa värittääksesi sen."


tr : Language -> String -> String -> String
tr lang en fi =
    case lang of
        Fi ->
            fi

        En ->
            en
