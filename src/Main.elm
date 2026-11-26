module Main exposing (Flags, Model, Msg, main)

{-| Top-level Elm Architecture entry point for Brick Racer (`brick-racer`).

Wires URL/file model ingestion (`Racer.Ingest`), mechanical inspection & mesh
splitting (`Racer.Analyze`), procedural countryside courses (`Racer.Track`,
`Racer.TerrainMesh`), 60 Hz downhill soapbox physics (`Racer.Physics`),
WebGL rendering (`Racer.Render`), Web Audio telemetry (`Racer.Audio`),
local leaderboards (`Racer.Leaderboard`), and responsive menus/HUD (`Pages.*`,
`Racer.HUD`).

-}

import Browser
import Browser.Dom as Dom
import Browser.Events
import DesignTokens.Colors as Brand
import Dict exposing (Dict)
import Game.MapParams as Map exposing (Biome, MapParams, Pick(..), Season, Selection, WeatherKind)
import Game.Multiplayer as Multiplayer exposing (Role(..), RoomState, RoomStatus(..))
import Game.PartLoading as PartLoading
import Game.Ports as GamePorts
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import I18n exposing (Language(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import LDraw.Catalog as Catalog exposing (PartIndex)
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, ResolverConfig)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Build as Build
import Pages.Garage as Garage
import Pages.Race as Race
import Pages.Results as Results
import Ports
import Racer.Analyze as Analyze exposing (CarMeshBundle)
import Racer.Audio as Audio
import Racer.Camera as RacerCamera
import Racer.Course as Course
import Racer.Editor as Editor
import Racer.HUD as HUD exposing (HudAlert)
import Racer.I18n as RI18n
import Racer.Ingest as Ingest
import Racer.Leaderboard as Leaderboard exposing (Leaderboard)
import Racer.Physics as Physics
import Racer.Render as Render
import Racer.Room as RacerRoom
import Racer.Scenery as Scenery
import Racer.TerrainMesh as TerrainMesh exposing (TrackRenderBundle)
import Racer.TerrainShader as TerrainShader exposing (DebrisPiece, TerrainVertex)
import Racer.TiltPitch as TiltPitch
import Racer.Track as Track
import Racer.Types
    exposing
        ( CameraMode(..)
        , CrashReason(..)
        , GeneratedTrack
        , InputState
        , LeaderboardEntry
        , PhysicsEvent(..)
        , PhysicsState
        , PresetBuild
        , RaceMode(..)
        , TrackObstacle
        , VehicleSpec
        )
import Random
import Render.Camera as Camera exposing (Camera)
import Set exposing (Set)
import Studio.PartIndex as PartIndex
import Task
import Viewport.Shortcuts as Shortcuts
import WebGL


{-| Application flags injected by `main.js` and `initBricksRuntime`.
-}
type alias Flags =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , indicesBase : String
    , initialHash : String
    , initialBytes : Maybe (List Int)
    , predecodedInitialBytes : Maybe (List Int)
    , coarsePointer : Bool
    , language : String
    , savedAudioMuted : Bool
    , savedLeaderboardJson : String
    , baseUrl : String
    , supportsPointerEvents : Bool
    }


{-| Top-level active screen state.
-}
type Page
    = SetupPage GarageState
    | RacePage RaceState
    | FinishLeaderboardPage ResultsState
    | BuildPage Build.Model


{-| Vehicle inspection state for `SetupPage`.
-}
type alias GarageState =
    { document : Document
    , title : String
    , vehicle : VehicleSpec
    , meshes : CarMeshBundle
    , camera : Camera
    , wheelPreviewSpin : Float
    }


{-| Subphase inside `RacePage`.
-}
type ActiveRaceSubphase
    = CountdownPhase Float Int
    | ActiveDriving
    | CrashRecoveryPhase Float CrashReason
    | CelebrationPhase
        { timer : Float
        , winner : String
        , confettiSpawned : Bool
        , results : ResultsState
        }


{-| Live downhill racing state for `RacePage`.
-}
type alias RaceState =
    { document : Document
    , title : String
    , vehicle : VehicleSpec
    , meshes : CarMeshBundle
    , track : GeneratedTrack
    , bundle : TrackRenderBundle
    , physics : PhysicsState
    , accumulator : Float
    , subPhase : ActiveRaceSubphase
    , elapsedTime : Float
    , topSpeedKmh : Float
    , splitTimes : List Float
    , splitDelta : Maybe Float
    , splitFlash : Maybe { time : Float, timer : Float }
    , hudAlert : Maybe HudAlert
    , cameraMode : CameraMode
    , camera : Camera
    , debris : List DebrisPiece
    , debrisSeed : Int
    , shake : Float
    , player2 : Maybe PlayerTwoState
    , multiplayer : Maybe RoomState
    , roomHitCooldown : Float
    , roomEventsOut : List Encode.Value
    }


{-| State tracking Player 2 during local two-player split-screen races.
-}
type alias PlayerTwoState =
    { vehicle : VehicleSpec
    , meshes : CarMeshBundle
    , physics : PhysicsState
    , camera : Camera
    , accumulator : Float
    , subPhase : ActiveRaceSubphase
    , topSpeedKmh : Float
    , splitTimes : List Float
    , splitDelta : Maybe Float
    , hudAlert : Maybe HudAlert
    , debris : List DebrisPiece
    , debrisSeed : Int
    }


{-| Post-race podium and split state for `FinishLeaderboardPage`.
-}
type alias ResultsState =
    { document : Document
    , title : String
    , vehicle : VehicleSpec
    , meshes : CarMeshBundle
    , map : MapParams
    , finishTimeS : Float
    , topSpeedKmh : Float
    , finalIntegrityPct : Int
    , splitTimes : List Float
    , isNewBest : Bool
    }


{-| Cached track geometry and spatial chunk bundle keyed by map code.
-}
type alias CachedTrack =
    { mapCode : String
    , track : GeneratedTrack
    , bundle : TrackRenderBundle
    }


{-| Touch/on-screen held driving button flags.
-}
type alias TouchState =
    { left : Bool
    , right : Bool
    , brake : Bool
    , tuck : Bool
    , drift : Bool
    }


{-| Tilt steering as reported by the shared `tilt-steer.js`: whether the
device can tilt-steer, whether it is switched on (on by default on phones, the
saved choice wins), and the calibrated steering value -1 .. 1.
-}
type alias TiltState =
    { enabled : Bool
    , available : Bool
    , steer : Float
    }


{-| Static shared WebGL utility meshes allocated once at startup.
-}
type alias StaticMeshes =
    { plinth : WebGL.Mesh TerrainVertex
    , shadow : WebGL.Mesh TerrainVertex
    }


{-| Full Brick Racer TEA application model.
-}
type alias Model =
    { resolverConfig : ResolverConfig
    , ldrawBase : String
    , ldrawFallbackBase : String
    , supportsPointerEvents : Bool
    , indicesBase : String
    , partCache : PartCache
    , partIndex : PartIndex
    , partIndexLoaded : Bool
    , props : Scenery.Props
    , staticMeshes : StaticMeshes
    , selection : Selection
    , map : MapParams
    , seedText : String
    , editingSeed : Bool
    , menuPanel : Garage.MenuPanel
    , lastMap : Maybe MapParams
    , cachedTrack : Maybe CachedTrack
    , language : Language
    , viewportWidth : Int
    , viewportHeight : Int
    , coarsePointer : Bool
    , showTouchControls : Bool
    , audioMuted : Bool
    , leaderboard : Leaderboard
    , pasteText : String
    , pasteError : Maybe String
    , pendingBinaryPaste : Maybe String
    , pressedKeys : Set String
    , touch : TouchState
    , tilt : TiltState
    , tiltPitch : TiltPitch.Pitch
    , shareCopiedNotice : Bool
    , raceMode : RaceMode
    , playerModeTab : Garage.PlayerModeTab
    , roomCodeInput : String
    , multiplayer : Maybe RoomState
    , baseUrl : String
    , remoteCars : Dict String (Maybe RemoteCar)
    , customCar : Maybe { title : String, document : Document }
    , selectedPresetId : Maybe String
    , savedGarage : Maybe GarageState
    , activeSlot : Garage.PlayerSlot
    , playerOneCar : Maybe GarageState
    , playerTwoCar : PlayerTwoCar
    , page : Page
    }


{-| Player 2's customized vehicle in local two-player split-screen mode.
-}
type alias PlayerTwoCar =
    { title : String
    , document : Document
    , vehicle : VehicleSpec
    , meshes : CarMeshBundle
    }


{-| Another player's car, built once from the model they shared.
`Nothing` in `remoteCars` marks a model that could not be used; that player
then shows with the default look.
-}
type alias RemoteCar =
    { title : String
    , document : Document
    , vehicle : VehicleSpec
    , meshes : CarMeshBundle
    , settled : Bool
    }


{-| Builds with more parts than this are not drawn for other players.
-}
maxRemoteParts : Int
maxRemoteParts =
    600


{-| Top-level application messages.
-}
type Msg
    = NoOp
    | GotDomViewport Dom.Viewport
    | ViewportResized { width : Int, height : Int }
    | PartIndexLoaded (Result Http.Error PartIndex.PartIndex)
    | PartFetched String (Result Http.Error String)
    | ToggleLanguage
    | ToggleAudioMute
    | ToggleRaceMode
    | SetPlayerModeTab Garage.PlayerModeTab
    | SetRaceMode RaceMode
    | SelectPlayerSlot Garage.PlayerSlot
    | LeaveMultiplayerRoom
    | StartHostMultiplayer Multiplayer.SessionMode
    | JoinMultiplayer String
    | RoomCodeChanged String
    | MultiplayerRoomChanged Decode.Value
    | MultiplayerError String
    | MultiplayerVehicleLoaded { id : String, payload : String }
    | StartMultiplayerCountdown
    | CopyMultiplayerLink
    | PasteChanged String
    | SubmitPaste
    | BinaryHashDecoded (Maybe (List Int))
    | FileContentReceived String
    | FileLoadError String
    | HashChanged String
    | RequestFileUpload
    | SelectPreset PresetBuild
    | SelectCustomCar
    | StartBuild String
    | EditInBuilder
    | OpenBuilder
    | BuildMsg Build.Msg
    | PickBiome (Pick Biome)
    | PickSeason (Pick Season)
    | PickWeather (Pick WeatherKind)
    | ChangeSeed String
    | SeedDraftChanged String
    | StartSeedEdit
    | CommitSeed
    | CancelSeedEdit
    | CopySeed
    | ToggleSteepness
    | SetMenuPanel Garage.MenuPanel
    | RandomSeed
    | UsePreviousMap
    | NewMap
    | BackToMenu
    | CopyShareLink
    | OpenQrScanner
    | QrCodeScanned String
    | QrScannerError String
    | GarageToRace
    | InspectInGarage
    | RestartRace
    | ShowLeaderboardPage ResultsState
    | RespawnCheckpoint
    | TiltStatusChanged { available : Bool, enabled : Bool }
    | TiltSteerChanged Float
    | TiltOrientation { available : Bool, gamma : Float, beta : Float }
    | TouchSteerLeft Bool
    | TouchSteerRight Bool
    | TouchBrake Bool
    | TouchTuck Bool
    | TouchDrift Bool
    | KeyDown Shortcuts.KeyEvent
    | KeyUp String
    | VisibilityChanged Browser.Events.Visibility
    | AnimationFrame Float
    | CamMouseDown Float Float
    | CamMouseMove Float Float
    | CamMouseUp
    | CamWheel Float


{-| Default car on startup when no URL model hash is present, and the car the
menu falls back to: the pinewood starter base (11 parts, fixed wheel holders,
4 wheels), as the editor's `.mpd` with its grouped base submodel, so "Build"
opens it in the car editor (`Editor.baseOf` recognises it).
-}
defaultPreset : PresetBuild
defaultPreset =
    case Editor.starterPreset "pinewood-classic" of
        Just preset ->
            preset

        Nothing ->
            case Ingest.presets of
                p :: _ ->
                    p

                [] ->
                    { id = "pinewood-classic"
                    , nameEn = "Pinewood Plank Racer"
                    , nameFi = "Klassinen mäkiauto"
                    , summaryEn = ""
                    , summaryFi = ""
                    , ldrText = ""
                    }


{-| Initial fallback turntable garage state before asynchronous part caches settle.
-}
initGarageState : PresetBuild -> Language -> GarageState
initGarageState preset lang =
    let
        doc =
            Document.parse preset.ldrText

        title =
            presetTitle lang preset

        analyzed =
            Analyze.analyzeDocument title doc Resolve.initCache Catalog.emptyPartIndex
    in
    { document = doc
    , title = title
    , vehicle = analyzed.spec
    , meshes = analyzed.meshes
    , camera = RacerCamera.initGarageCamera 720 analyzed.spec
    , wheelPreviewSpin = 0.0
    }


{-| Initialize Elm program.
-}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Initialize application state from JS flags, checking for preloaded URL hashes.
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        lang =
            if String.startsWith "fi" (String.toLower flags.language) then
                Fi

            else
                En

        resolverCfg =
            PartLoading.resolverFromFlags flags

        parsedInit =
            Ingest.parseHash flags.initialHash

        initSelection =
            parsedInit.map
                |> Maybe.map exactSelection
                |> Maybe.withDefault Map.anySelection

        initMap =
            Map.resolve initSelection

        initPreset =
            defaultPreset

        initP2Preset =
            initPreset

        initP2Garage =
            initGarageState initP2Preset lang

        p2Car =
            { title = initP2Garage.title
            , document = initP2Garage.document
            , vehicle = initP2Garage.vehicle
            , meshes = initP2Garage.meshes
            }

        ( baseModel, propsCmd ) =
            refreshProps
                { resolverConfig = resolverCfg
                , ldrawBase = flags.ldrawBase
                , ldrawFallbackBase = flags.ldrawFallbackBase
                , supportsPointerEvents = flags.supportsPointerEvents
                , indicesBase = flags.indicesBase
                , partCache = Resolve.initCache
                , partIndex = Catalog.emptyPartIndex
                , partIndexLoaded = False
                , props = Scenery.empty
                , staticMeshes =
                    { plinth = TerrainMesh.buildGaragePlinthMesh
                    , shadow = TerrainShader.buildShadowQuadMesh
                    }
                , selection = initSelection
                , map = initMap
                , seedText = Map.toCode initMap
                , editingSeed = False
                , menuPanel = Garage.WorldPanel
                , lastMap = Nothing
                , cachedTrack = Nothing
                , language = lang
                , viewportWidth = 1280
                , viewportHeight = 720
                , coarsePointer = flags.coarsePointer
                , showTouchControls = flags.coarsePointer
                , audioMuted = flags.savedAudioMuted
                , leaderboard = Leaderboard.decodeLeaderboard flags.savedLeaderboardJson
                , pasteText = ""
                , pasteError = Nothing
                , pendingBinaryPaste = Nothing
                , pressedKeys = Set.empty
                , touch =
                    { left = False
                    , right = False
                    , brake = False
                    , tuck = False
                    , drift = False
                    }
                , tilt =
                    { enabled = False
                    , available = False
                    , steer = 0.0
                    }
                , tiltPitch = TiltPitch.init
                , shareCopiedNotice = False
                , raceMode = SinglePlayer
                , playerModeTab = Garage.HotSeatTab
                , roomCodeInput = ""
                , multiplayer = Nothing
                , baseUrl = flags.baseUrl
                , remoteCars = Dict.empty
                , customCar = Nothing
                , selectedPresetId = Just initPreset.id
                , savedGarage = Nothing
                , activeSlot = Garage.SlotPlayer1
                , playerOneCar = Nothing
                , playerTwoCar = p2Car
                , page = SetupPage (initGarageState initPreset lang)
                }

        startupCmds =
            [ Task.perform GotDomViewport Dom.getViewport
            , propsCmd
            , PartIndex.fetchWithFallback (nonEmptyMaybe flags.indicesBase) flags.ldrawBase PartIndexLoaded
            ]

        bytesCandidate =
            case flags.predecodedInitialBytes of
                Just b ->
                    Just b

                Nothing ->
                    flags.initialBytes

        trimmedHash =
            String.trim flags.initialHash
    in
    if String.contains "room=" (String.toLower trimmedHash) then
        let
            code =
                Multiplayer.roomCodeFrom trimmedHash

            doc =
                Document.parse initPreset.ldrText

            title =
                presetTitle lang initPreset

            vHash =
                Ingest.encodeRaceHash initMap doc

            ( nextModel, enterCmd ) =
                loadDocumentIntoGarage title doc Nothing False { baseModel | selectedPresetId = Just initPreset.id, roomCodeInput = code, playerModeTab = Garage.NetworkTab }
        in
        ( nextModel
        , Cmd.batch
            (enterCmd
                :: GamePorts.joinMultiplayerRoom
                    { roomId = code
                    , name = "Racer"
                    , vehicleHash = vHash
                    }
                :: startupCmds
            )
        )

    else if parsedInit.isGarage then
        let
            decodedResult =
                Ingest.decodeRaceInput bytesCandidate trimmedHash

            initData =
                case decodedResult of
                    Ok decoded ->
                        { doc = decoded.document
                        , title = decoded.suggestedName
                        , map = decoded.map
                        }

                    Err _ ->
                        { doc = Document.parse initPreset.ldrText
                        , title = presetTitle lang initPreset
                        , map = Nothing
                        }

            analyzed =
                Analyze.analyzeDocument initData.title initData.doc baseModel.partCache baseModel.partIndex

            savedGarageState =
                { document = initData.doc
                , title = initData.title
                , vehicle = analyzed.spec
                , meshes = analyzed.meshes
                , camera = RacerCamera.initGarageCamera baseModel.viewportHeight analyzed.spec
                , wheelPreviewSpin = 0.0
                }

            modelWithMap =
                withMap initData.map baseModel

            modelForBuilder =
                { modelWithMap
                    | customCar = Just { title = initData.title, document = initData.doc }
                    , selectedPresetId = Nothing
                    , savedGarage = Just savedGarageState
                }

            ( builderModel, builderCmd ) =
                openBuilder (Editor.baseOf initData.doc) initData.doc modelForBuilder
        in
        ( builderModel, Cmd.batch (builderCmd :: startupCmds) )

    else if bytesCandidate /= Nothing || not (String.isEmpty (String.replace "#" "" trimmedHash)) && (parsedInit.base64Payload /= Nothing || parsedInit.hasModelParam) then
        case Ingest.decodeRaceInput bytesCandidate trimmedHash of
            Ok decoded ->
                let
                    custom =
                        Just { title = decoded.suggestedName, document = decoded.document }

                    ( nextModel, enterCmd ) =
                        loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.map False { baseModel | customCar = custom, selectedPresetId = Nothing }
                in
                ( nextModel, Cmd.batch (enterCmd :: startupCmds) )

            Err _ ->
                let
                    doc =
                        Document.parse initPreset.ldrText

                    title =
                        presetTitle lang initPreset

                    ( nextModel, enterCmd ) =
                        loadDocumentIntoGarage title doc Nothing False { baseModel | selectedPresetId = Just initPreset.id }
                in
                ( nextModel, Cmd.batch (enterCmd :: startupCmds) )

    else
        let
            doc =
                Document.parse initPreset.ldrText

            title =
                presetTitle lang initPreset

            ( nextModel, enterCmd ) =
                loadDocumentIntoGarage title doc Nothing False { baseModel | selectedPresetId = Just initPreset.id }
        in
        ( nextModel, Cmd.batch (enterCmd :: startupCmds) )


{-| Dispatch application messages.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotDomViewport vp ->
            let
                w =
                    max 320 (round vp.viewport.width)

                h =
                    max 240 (round vp.viewport.height)
            in
            ( applyViewportSize w h model, Cmd.none )

        ViewportResized { width, height } ->
            let
                sized =
                    applyViewportSize (max 320 width) (max 240 height) model
            in
            case sized.page of
                BuildPage build ->
                    let
                        ( nextBuild, buildCmd ) =
                            Build.resize sized.viewportWidth sized.viewportHeight build
                    in
                    ( { sized | page = BuildPage nextBuild }, Cmd.map BuildMsg buildCmd )

                _ ->
                    ( sized, Cmd.none )

        PartIndexLoaded result ->
            case result of
                Ok studioIndex ->
                    let
                        catalogIndex =
                            studioIndex.parts
                                |> List.map (\p -> { file = p.file, description = p.description, category = p.category })
                                |> Catalog.fromPartList

                        nextModel =
                            { model
                                | partIndex = catalogIndex
                                , partIndexLoaded = True
                            }
                    in
                    ( reanalyzeActiveVehicle nextModel, Cmd.none )

                Err _ ->
                    ( { model | partIndexLoaded = True }, Cmd.none )

        PartFetched partName result ->
            let
                nextCache =
                    Resolve.updateCache partName result Parser.parseFile model.partCache

                modelWithCache =
                    { model | partCache = nextCache }
            in
            withProps <|
                refreshRemoteCars <|
                    case activeDocument modelWithCache.page of
                        Nothing ->
                            ( modelWithCache, Cmd.none )

                        Just doc ->
                            let
                                stats =
                                    reachablePartStats doc nextCache

                                markedCache =
                                    Resolve.markLoading stats.needed nextCache

                                fetchCmd =
                                    fetchPartsCmd modelWithCache.resolverConfig stats.needed

                                updatedModel =
                                    { modelWithCache | partCache = markedCache }

                                finalModel =
                                    if stats.inFlight == 0 && List.isEmpty stats.needed then
                                        reanalyzeActiveVehicle updatedModel

                                    else if modBy 4 stats.settled == 0 then
                                        reanalyzeActiveVehicle updatedModel

                                    else
                                        updatedModel
                            in
                            ( finalModel, fetchCmd )

        ToggleLanguage ->
            let
                nextLang =
                    case model.language of
                        En ->
                            Fi

                        Fi ->
                            En

                code =
                    case nextLang of
                        En ->
                            "en"

                        Fi ->
                            "fi"
            in
            ( { model | language = nextLang }
            , GamePorts.storeLanguage code
            )

        ToggleAudioMute ->
            let
                nextMuted =
                    not model.audioMuted

                nextModel =
                    { model | audioMuted = nextMuted }

                audioCmd =
                    raceAudioCmd nextModel
            in
            ( nextModel
            , Cmd.batch [ GamePorts.setAudioMuted nextMuted, audioCmd ]
            )

        ToggleRaceMode ->
            let
                nextMode =
                    case model.raceMode of
                        SinglePlayer ->
                            SplitScreenTwoPlayer

                        SplitScreenTwoPlayer ->
                            SinglePlayer
            in
            ( { model | raceMode = nextMode }, Cmd.none )

        SetPlayerModeTab tab ->
            ( { model | playerModeTab = tab }, Cmd.none )

        SetRaceMode mode ->
            ( { model | raceMode = mode }, Cmd.none )

        SelectPlayerSlot slot ->
            if slot == model.activeSlot then
                ( model, Cmd.none )

            else
                case model.page of
                    SetupPage currentGarage ->
                        if slot == Garage.SlotPlayer2 then
                            let
                                p2 =
                                    model.playerTwoCar

                                nextGarage =
                                    { document = p2.document
                                    , title = p2.title
                                    , vehicle = p2.vehicle
                                    , meshes = p2.meshes
                                    , camera = RacerCamera.initGarageCamera model.viewportHeight p2.vehicle
                                    , wheelPreviewSpin = 0.0
                                    }
                            in
                            ( { model
                                | activeSlot = Garage.SlotPlayer2
                                , playerOneCar = Just currentGarage
                                , page = SetupPage nextGarage
                              }
                            , Cmd.none
                            )

                        else
                            let
                                nextP2 =
                                    { title = currentGarage.title
                                    , document = currentGarage.document
                                    , vehicle = currentGarage.vehicle
                                    , meshes = currentGarage.meshes
                                    }

                                p1 =
                                    model.playerOneCar
                                        |> Maybe.withDefault currentGarage

                                nextGarage =
                                    { document = p1.document
                                    , title = p1.title
                                    , vehicle = p1.vehicle
                                    , meshes = p1.meshes
                                    , camera = RacerCamera.initGarageCamera model.viewportHeight p1.vehicle
                                    , wheelPreviewSpin = 0.0
                                    }
                            in
                            ( { model
                                | activeSlot = Garage.SlotPlayer1
                                , playerTwoCar = nextP2
                                , page = SetupPage nextGarage
                              }
                            , Cmd.none
                            )

                    _ ->
                        ( model, Cmd.none )

        LeaveMultiplayerRoom ->
            let
                garageDoc =
                    getGarageDoc model
            in
            ( { model | multiplayer = Nothing }
            , Cmd.batch
                [ GamePorts.leaveMultiplayerRoom ()
                , GamePorts.setUrlHash (Ingest.encodeRaceHash model.map garageDoc)
                ]
            )

        StartHostMultiplayer mode ->
            let
                vHash =
                    case activeDocument model.page of
                        Just doc ->
                            Ingest.encodeRaceHash model.map doc

                        Nothing ->
                            ""
            in
            ( { model | playerModeTab = Garage.NetworkTab }
            , GamePorts.createMultiplayerRoom
                { roomId = ""
                , courseId = Map.toCode model.map
                , name = "Host Commentator"
                , vehicleHash = vHash
                , maxPlayers = Multiplayer.sessionCapacity mode
                }
            )

        RoomCodeChanged code ->
            ( { model | roomCodeInput = Multiplayer.roomCodeFrom code }, Cmd.none )

        JoinMultiplayer code ->
            let
                vHash =
                    case activeDocument model.page of
                        Just doc ->
                            Ingest.encodeRaceHash model.map doc

                        Nothing ->
                            ""
            in
            ( { model | playerModeTab = Garage.NetworkTab }
            , GamePorts.joinMultiplayerRoom
                { roomId = Multiplayer.roomCodeFrom code
                , name = "Racer"
                , vehicleHash = vHash
                }
            )

        StartMultiplayerCountdown ->
            ( model, GamePorts.startMultiplayerCountdown () )

        CopyMultiplayerLink ->
            let
                roomUrl =
                    case model.multiplayer of
                        Just r ->
                            let
                                cleanBase =
                                    if String.endsWith "/" model.baseUrl then
                                        String.dropRight 1 model.baseUrl

                                    else
                                        model.baseUrl
                            in
                            if String.isEmpty cleanBase then
                                "#room=" ++ r.roomId

                            else
                                cleanBase ++ "/#room=" ++ r.roomId

                        Nothing ->
                            ""
            in
            ( { model | shareCopiedNotice = True }, GamePorts.copyShareUrl roomUrl )

        MultiplayerRoomChanged rawJson ->
            case RacerRoom.fromPort rawJson of
                RacerRoom.NoRoom ->
                    -- Left the room (or never in one): the menu and race hold none.
                    case model.page of
                        RacePage race ->
                            ( { model | page = RacePage { race | multiplayer = Nothing }, multiplayer = Nothing }, Cmd.none )

                        _ ->
                            ( { model | multiplayer = Nothing }, Cmd.none )

                RacerRoom.InRoom room ->
                    case model.page of
                        RacePage race ->
                            let
                                ( updatedRace, eventCmds ) =
                                    applyRoomEvents model room { race | multiplayer = Just room }
                            in
                            ( { model | page = RacePage updatedRace, multiplayer = Just room }, Cmd.batch eventCmds )

                        SetupPage garage ->
                            let
                                -- A joiner races the host's map.
                                joinedModel =
                                    case ( room.role, Map.fromCode room.courseId ) of
                                        ( JoinedPlayer, Just hostMap ) ->
                                            if hostMap /= model.map then
                                                withSelection (exactSelection hostMap) model

                                            else
                                                model

                                        _ ->
                                            model

                                shareCmd =
                                    if room.role == JoinedPlayer then
                                        GamePorts.setMultiplayerVehicle (Ingest.encodeRaceHash joinedModel.map garage.document)

                                    else
                                        Cmd.none

                                hashCmd =
                                    GamePorts.setUrlHash ("#room=" ++ room.roomId)
                            in
                            if room.status /= Lobby then
                                let
                                    ( nextModel, enterCmd ) =
                                        startRaceFromDoc garage.title garage.document garage.vehicle garage.meshes { joinedModel | multiplayer = Just room }
                                in
                                ( nextModel
                                , Cmd.batch
                                    [ enterCmd
                                    , shareCmd
                                    , hashCmd
                                    ]
                                )

                            else
                                ( { joinedModel | multiplayer = Just room, playerModeTab = Garage.NetworkTab }
                                , Cmd.batch
                                    [ shareCmd
                                    , hashCmd
                                    ]
                                )

                        _ ->
                            ( { model | multiplayer = Just room }, Cmd.none )

                RacerRoom.Unreadable ->
                    ( model, Cmd.none )

        MultiplayerError err ->
            ( { model | pasteError = Just err }, Cmd.none )

        MultiplayerVehicleLoaded vehicle ->
            addRemoteCar vehicle.id vehicle.payload model

        PasteChanged txt ->
            ( { model | pasteText = txt, pasteError = Nothing }, Cmd.none )

        SubmitPaste ->
            let
                trimmed =
                    String.trim model.pasteText
            in
            if String.isEmpty trimmed then
                ( { model | pasteError = Just (RI18n.pasteErrorFallback model.language) }, Cmd.none )

            else
                case Ingest.decodeRaceInput Nothing trimmed of
                    Ok decoded ->
                        let
                            custom =
                                Just { title = decoded.suggestedName, document = decoded.document }
                        in
                        loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.map True { model | customCar = custom, selectedPresetId = Nothing }

                    Err Ingest.InvalidBinaryPayload ->
                        ( { model | pendingBinaryPaste = Just trimmed }
                        , GamePorts.decodeBinaryHash trimmed
                        )

                    Err _ ->
                        ( { model | pasteError = Just (RI18n.pasteErrorFallback model.language) }
                        , Cmd.none
                        )

        BinaryHashDecoded maybeBytes ->
            case ( maybeBytes, model.pendingBinaryPaste ) of
                ( Just bytes, Just rawInput ) ->
                    case Ingest.decodeRaceInput (Just bytes) rawInput of
                        Ok decoded ->
                            let
                                custom =
                                    Just { title = decoded.suggestedName, document = decoded.document }
                            in
                            loadDocumentIntoGarage decoded.suggestedName
                                decoded.document
                                decoded.map
                                True
                                { model
                                    | pendingBinaryPaste = Nothing
                                    , pasteError = Nothing
                                    , customCar = custom
                                    , selectedPresetId = Nothing
                                }

                        Err _ ->
                            ( { model
                                | pendingBinaryPaste = Nothing
                                , pasteError = Just (RI18n.pasteErrorFallback model.language)
                              }
                            , Cmd.none
                            )

                ( Nothing, Just _ ) ->
                    ( { model
                        | pendingBinaryPaste = Nothing
                        , pasteError = Just (RI18n.pasteErrorFallback model.language)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FileContentReceived rawLdr ->
            case model.page of
                BuildPage build ->
                    case Ingest.decodeRaceInput Nothing rawLdr of
                        Ok decoded ->
                            let
                                ( nextBuild, buildCmd, _ ) =
                                    Build.update (Build.LoadExternalDocument decoded.document Nothing) build
                            in
                            ( { model | page = BuildPage nextBuild }, Cmd.map BuildMsg buildCmd )

                        Err _ ->
                            ( { model | pasteError = Just (RI18n.pasteErrorFallback model.language) }
                            , Cmd.none
                            )

                _ ->
                    case Ingest.decodeRaceInput Nothing rawLdr of
                        Ok decoded ->
                            let
                                custom =
                                    Just { title = decoded.suggestedName, document = decoded.document }
                            in
                            loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.map True { model | customCar = custom, selectedPresetId = Nothing }

                        Err _ ->
                            ( { model | pasteError = Just (RI18n.pasteErrorFallback model.language) }
                            , Cmd.none
                            )

        FileLoadError errText ->
            ( { model | pasteError = Just errText }
            , silenceAudioCmd
            )

        HashChanged rawHash ->
            let
                trimmed =
                    String.trim rawHash
            in
            if String.isEmpty trimmed || trimmed == "#" then
                ( model, Cmd.none )

            else if String.contains "room=" (String.toLower trimmed) then
                let
                    code =
                        Multiplayer.roomCodeFrom trimmed
                in
                if not (String.isEmpty code) && (model.multiplayer |> Maybe.map .roomId) /= Just code then
                    let
                        vHash =
                            case activeDocument model.page of
                                Just doc ->
                                    Ingest.encodeRaceHash model.map doc

                                Nothing ->
                                    ""
                    in
                    ( { model | playerModeTab = Garage.NetworkTab, roomCodeInput = code }
                    , GamePorts.joinMultiplayerRoom
                        { roomId = code
                        , name = "Racer"
                        , vehicleHash = vHash
                        }
                    )

                else
                    ( model, Cmd.none )

            else
                case model.page of
                    BuildPage _ ->
                        ( model, Cmd.none )

                    _ ->
                        case Ingest.decodeRaceInput Nothing trimmed of
                            Ok decoded ->
                                if decoded.isGarage then
                                    openBuilder (Editor.baseOf decoded.document) decoded.document model

                                else
                                    let
                                        custom =
                                            Just { title = decoded.suggestedName, document = decoded.document }
                                    in
                                    loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.map False { model | customCar = custom, selectedPresetId = Nothing }

                            Err _ ->
                                ( model, Cmd.none )

        RequestFileUpload ->
            ( model, GamePorts.requestFileUpload () )

        SelectPreset preset ->
            let
                doc =
                    Document.parse preset.ldrText

                title =
                    presetTitle model.language preset
            in
            loadDocumentIntoGarage title doc Nothing True { model | selectedPresetId = Just preset.id }

        SelectCustomCar ->
            case model.customCar of
                Just custom ->
                    loadDocumentIntoGarage custom.title custom.document Nothing True { model | selectedPresetId = Nothing }

                Nothing ->
                    ( model, Cmd.none )

        StartBuild baseId ->
            case List.filter (\b -> b.id == baseId) Editor.bases |> List.head of
                Just base ->
                    let
                        modelWithSaved =
                            case model.page of
                                SetupPage garage ->
                                    { model | savedGarage = Just garage }

                                _ ->
                                    model
                    in
                    openBuilder (Just base) base.document modelWithSaved

                Nothing ->
                    ( model, Cmd.none )

        EditInBuilder ->
            case model.page of
                SetupPage garage ->
                    openBuilder (Editor.baseOf garage.document) garage.document { model | savedGarage = Just garage }

                _ ->
                    ( model, Cmd.none )

        OpenBuilder ->
            case model.page of
                SetupPage garage ->
                    openBuilder (Editor.baseOf garage.document) garage.document { model | savedGarage = Just garage }

                _ ->
                    ( model, Cmd.none )

        BuildMsg buildMsg ->
            case model.page of
                BuildPage build ->
                    let
                        ( nextBuild, buildCmd, outcome ) =
                            Build.update buildMsg build
                    in
                    case outcome of
                        Build.Stay ->
                            let
                                docBefore =
                                    Build.document build

                                docAfter =
                                    Build.document nextBuild

                                hashCmd =
                                    if docBefore /= docAfter then
                                        GamePorts.setUrlHash (Ingest.encodeGarageHash model.map docAfter)

                                    else
                                        Cmd.none
                            in
                            ( { model | page = BuildPage nextBuild }
                            , Cmd.batch [ Cmd.map BuildMsg buildCmd, hashCmd ]
                            )

                        Build.Quit ->
                            case model.savedGarage of
                                Just garage ->
                                    ( { model | page = SetupPage garage, savedGarage = Nothing }
                                    , GamePorts.setUrlHash (Ingest.encodeRaceHash model.map garage.document)
                                    )

                                Nothing ->
                                    let
                                        doc =
                                            Document.parse defaultPreset.ldrText

                                        title =
                                            presetTitle model.language defaultPreset
                                    in
                                    loadDocumentIntoGarage title doc Nothing True { model | selectedPresetId = Just defaultPreset.id }

                        Build.Finished doc ->
                            let
                                title =
                                    builtCarTitle model.language nextBuild.base

                                isP2 =
                                    model.raceMode == SplitScreenTwoPlayer && model.activeSlot == Garage.SlotPlayer2
                            in
                            if isP2 then
                                let
                                    analyzed =
                                        Analyze.analyzeDocument title doc model.partCache model.partIndex

                                    updatedModel =
                                        { model
                                            | playerTwoCar =
                                                { title = title
                                                , document = doc
                                                , vehicle = analyzed.spec
                                                , meshes = analyzed.meshes
                                                }
                                            , savedGarage = Nothing
                                        }
                                in
                                loadDocumentIntoGarage title doc Nothing True updatedModel

                            else
                                let
                                    custom =
                                        Just { title = title, document = doc }

                                    ( nextModel, enterCmd ) =
                                        loadDocumentIntoGarage title doc Nothing True { model | customCar = custom, selectedPresetId = Nothing, savedGarage = Nothing }

                                    shareCmd =
                                        case model.multiplayer of
                                            Just _ ->
                                                GamePorts.setMultiplayerVehicle (Ingest.encodeRaceHash nextModel.map doc)

                                            Nothing ->
                                                Cmd.none
                                in
                                ( nextModel, Cmd.batch [ enterCmd, shareCmd ] )

                        Build.TriggerFileUpload ->
                            ( { model | page = BuildPage nextBuild }, Cmd.batch [ Cmd.map BuildMsg buildCmd, GamePorts.requestFileUpload () ] )

                        Build.TriggerQrScanner ->
                            ( { model | page = BuildPage nextBuild }, Cmd.batch [ Cmd.map BuildMsg buildCmd, GamePorts.openQrScanner () ] )

                _ ->
                    ( model, Cmd.none )

        PickBiome pick ->
            applySelection (\sel -> { sel | biome = pick }) model

        PickSeason pick ->
            applySelection (\sel -> { sel | season = pick }) model

        PickWeather pick ->
            applySelection (\sel -> { sel | weather = pick }) model

        ChangeSeed rawStr ->
            let
                trimmed =
                    String.trim rawStr

                maybeMap =
                    case Map.fromCode trimmed of
                        Just pasted ->
                            Just pasted

                        Nothing ->
                            (Ingest.parseHash trimmed).map
            in
            case maybeMap of
                Just pasted ->
                    applySelection (\_ -> exactSelection pasted) model

                Nothing ->
                    case String.toInt (String.replace "#" "" trimmed) of
                        Just n ->
                            let
                                ( nextModel, cmd ) =
                                    applySelection (\sel -> { sel | seed = clamp 1 999999 n }) model
                            in
                            ( { nextModel | seedText = rawStr }, cmd )

                        Nothing ->
                            ( { model | seedText = rawStr, shareCopiedNotice = False }, Cmd.none )

        SeedDraftChanged rawStr ->
            ( { model | seedText = rawStr }, Cmd.none )

        StartSeedEdit ->
            ( { model | editingSeed = True, seedText = Map.toCode model.map }
            , Task.attempt (\_ -> NoOp) (Dom.focus Garage.seedInputId)
            )

        CommitSeed ->
            if model.editingSeed then
                let
                    ( nextModel, cmd ) =
                        update (ChangeSeed model.seedText) { model | editingSeed = False }
                in
                ( { nextModel | seedText = Map.toCode nextModel.map }, cmd )

            else
                ( model, Cmd.none )

        CancelSeedEdit ->
            ( { model | editingSeed = False, seedText = Map.toCode model.map }, Cmd.none )

        CopySeed ->
            let
                ( nextModel, commitCmd ) =
                    if model.editingSeed then
                        let
                            ( committed, cmd ) =
                                update (ChangeSeed model.seedText) { model | editingSeed = False }
                        in
                        ( { committed | seedText = Map.toCode committed.map }, cmd )

                    else
                        ( model, Cmd.none )
            in
            ( { nextModel | shareCopiedNotice = True }
            , Cmd.batch
                [ commitCmd
                , GamePorts.copyShareUrl (Map.toCode nextModel.map)
                ]
            )

        ToggleSteepness ->
            case seedWithOtherSteepness model.map of
                Just seed ->
                    let
                        currentMap =
                            model.map
                    in
                    applySelection (\_ -> exactSelection { currentMap | seed = seed }) model

                Nothing ->
                    ( model, Cmd.none )

        SetMenuPanel panel ->
            ( { model | menuPanel = panel }, Cmd.none )

        RandomSeed ->
            let
                chance =
                    Map.anySelection
            in
            applySelection (\sel -> { chance | seed = nextRandomSeed sel.seed }) model

        UsePreviousMap ->
            case model.lastMap of
                Just previous ->
                    applySelection (\_ -> exactSelection previous) model

                Nothing ->
                    ( model, Cmd.none )

        NewMap ->
            let
                sel =
                    model.selection

                nextModel =
                    withSelection { sel | seed = nextRandomSeed sel.seed } model
            in
            update InspectInGarage nextModel

        BackToMenu ->
            let
                cleanModel =
                    { model
                        | pressedKeys = Set.empty
                        , touch = emptyTouch
                        , shareCopiedNotice = False
                        , multiplayer = Nothing
                    }
            in
            case model.page of
                RacePage race ->
                    let
                        ( nextModel, cmd ) =
                            enterGarageFromRaceOrResults race.document race.title race.vehicle race.meshes cleanModel
                    in
                    ( nextModel, Cmd.batch [ cmd, GamePorts.leaveMultiplayerRoom () ] )

                FinishLeaderboardPage res ->
                    let
                        ( nextModel, cmd ) =
                            enterGarageFromRaceOrResults res.document res.title res.vehicle res.meshes cleanModel
                    in
                    ( nextModel, Cmd.batch [ cmd, GamePorts.leaveMultiplayerRoom () ] )

                BuildPage _ ->
                    case model.savedGarage of
                        Just garage ->
                            ( { cleanModel | page = SetupPage garage, savedGarage = Nothing }
                            , Cmd.batch [ silenceAudioCmd, GamePorts.leaveMultiplayerRoom () ]
                            )

                        Nothing ->
                            let
                                doc =
                                    Document.parse defaultPreset.ldrText

                                title =
                                    presetTitle model.language defaultPreset
                            in
                            loadDocumentIntoGarage title doc Nothing False { cleanModel | selectedPresetId = Just defaultPreset.id }

                SetupPage _ ->
                    ( cleanModel, Cmd.batch [ silenceAudioCmd, GamePorts.leaveMultiplayerRoom () ] )

        CopyShareLink ->
            case activeDocument model.page of
                Nothing ->
                    ( model, Cmd.none )

                Just doc ->
                    let
                        hashStr =
                            Ingest.encodeRaceHash model.map doc
                    in
                    ( { model | shareCopiedNotice = True }
                    , Cmd.batch [ GamePorts.setUrlHash hashStr, GamePorts.copyShareUrl hashStr ]
                    )

        OpenQrScanner ->
            ( model, GamePorts.openQrScanner () )

        QrCodeScanned scannedHash ->
            let
                trimmed =
                    String.trim scannedHash
            in
            if String.contains "room=" (String.toLower trimmed) then
                let
                    code =
                        Multiplayer.roomCodeFrom trimmed

                    vHash =
                        case activeDocument model.page of
                            Just doc ->
                                Ingest.encodeRaceHash model.map doc

                            Nothing ->
                                ""
                in
                ( { model | playerModeTab = Garage.NetworkTab, roomCodeInput = code }
                , GamePorts.joinMultiplayerRoom
                    { roomId = code
                    , name = "Racer"
                    , vehicleHash = vHash
                    }
                )

            else
                ( { model | pendingBinaryPaste = Just scannedHash }
                , GamePorts.decodeBinaryHash scannedHash
                )

        QrScannerError _ ->
            ( model, Cmd.none )

        GarageToRace ->
            case model.page of
                SetupPage garage ->
                    if garage.vehicle.isDrivable then
                        if model.raceMode == SplitScreenTwoPlayer && model.activeSlot == Garage.SlotPlayer2 then
                            let
                                p1 =
                                    model.playerOneCar
                                        |> Maybe.withDefault garage

                                updatedModel =
                                    { model
                                        | playerTwoCar =
                                            { title = garage.title
                                            , document = garage.document
                                            , vehicle = garage.vehicle
                                            , meshes = garage.meshes
                                            }
                                    }
                            in
                            startRaceFromDoc p1.title p1.document p1.vehicle p1.meshes updatedModel

                        else
                            let
                                updatedModel =
                                    if model.raceMode == SplitScreenTwoPlayer then
                                        { model | playerOneCar = Just garage }

                                    else
                                        model
                            in
                            startRaceFromDoc garage.title garage.document garage.vehicle garage.meshes updatedModel

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InspectInGarage ->
            let
                cleanModel =
                    { model | multiplayer = Nothing }
            in
            case model.page of
                RacePage race ->
                    let
                        ( nextModel, cmd ) =
                            enterGarageFromRaceOrResults race.document race.title race.vehicle race.meshes cleanModel
                    in
                    ( nextModel, Cmd.batch [ cmd, GamePorts.leaveMultiplayerRoom () ] )

                FinishLeaderboardPage res ->
                    let
                        ( nextModel, cmd ) =
                            enterGarageFromRaceOrResults res.document res.title res.vehicle res.meshes cleanModel
                    in
                    ( nextModel, Cmd.batch [ cmd, GamePorts.leaveMultiplayerRoom () ] )

                _ ->
                    ( model, Cmd.none )

        RestartRace ->
            case model.page of
                RacePage race ->
                    startRaceFromDoc race.title race.document race.vehicle race.meshes model

                FinishLeaderboardPage res ->
                    startRaceFromDoc res.title res.document res.vehicle res.meshes model

                _ ->
                    ( model, Cmd.none )

        ShowLeaderboardPage res ->
            ( { model | page = FinishLeaderboardPage res }, Cmd.none )

        RespawnCheckpoint ->
            case model.page of
                RacePage race ->
                    let
                        respawned =
                            Physics.respawnAtCheckpoint race.physics

                        nextCam =
                            RacerCamera.initRaceCamera model.viewportHeight respawned race.vehicle

                        nextRace =
                            { race
                                | physics = respawned
                                , accumulator = 0.0
                                , subPhase = ActiveDriving
                                , camera = nextCam
                            }

                        nextModel =
                            { model | page = RacePage nextRace }
                    in
                    ( nextModel, raceAudioCmd nextModel )

                _ ->
                    ( model, Cmd.none )

        TiltStatusChanged status ->
            ( { model | tilt = { enabled = status.enabled, available = status.available, steer = 0.0 } }
            , Cmd.none
            )

        TiltSteerChanged value ->
            let
                oldTilt =
                    model.tilt
            in
            ( { model | tilt = { oldTilt | steer = clamp -1.0 1.0 value } }, Cmd.none )

        TiltOrientation orientation ->
            if orientation.available then
                ( { model | tiltPitch = TiltPitch.receive orientation.beta model.tiltPitch }, Cmd.none )

            else
                ( model, Cmd.none )

        TouchSteerLeft held ->
            let
                t =
                    model.touch
            in
            ( { model | touch = { t | left = held } }, Cmd.none )

        TouchSteerRight held ->
            let
                t =
                    model.touch
            in
            ( { model | touch = { t | right = held } }, Cmd.none )

        TouchBrake held ->
            let
                t =
                    model.touch
            in
            ( { model | touch = { t | brake = held } }, Cmd.none )

        TouchTuck held ->
            let
                t =
                    model.touch
            in
            ( { model | touch = { t | tuck = held } }, Cmd.none )

        TouchDrift held ->
            let
                t =
                    model.touch
            in
            ( { model | touch = { t | drift = held } }, Cmd.none )

        KeyDown keyEvent ->
            handleKeyDown keyEvent model

        KeyUp rawKey ->
            let
                norm =
                    Shortcuts.normalizeKey rawKey
            in
            ( { model | pressedKeys = Set.remove norm model.pressedKeys }, Cmd.none )

        VisibilityChanged vis ->
            case ( vis, model.page ) of
                ( Browser.Events.Hidden, RacePage _ ) ->
                    ( { model | pressedKeys = Set.empty, touch = emptyTouch }, silenceAudioCmd )

                _ ->
                    ( model, Cmd.none )

        AnimationFrame deltaMs ->
            stepFrame deltaMs model

        CamMouseDown x y ->
            updateActiveCamera (Camera.onMouseDown x y) model

        CamMouseMove x y ->
            updateActiveCamera (Camera.onMouseMove x y) model

        CamMouseUp ->
            updateActiveCamera Camera.onMouseUp model

        CamWheel dy ->
            updateActiveCamera (Camera.onWheel dy) model


{-| Metres from the car to the checkpoint it has to pass next (the finish is
the last one); 0 once there is none left.
-}
nextCheckpointMeters : List { a | index : Int, s : Float } -> PhysicsState -> Float
nextCheckpointMeters checkpoints physics =
    case List.filter (\cp -> cp.index == physics.checkpointIndex) checkpoints of
        cp :: _ ->
            max 0 (cp.s - physics.progressS)

        [] ->
            0


handleKeyDown : Shortcuts.KeyEvent -> Model -> ( Model, Cmd Msg )
handleKeyDown keyEvent model =
    if Shortcuts.isTextEntryTarget keyEvent || keyEvent.ctrl || keyEvent.meta || keyEvent.alt then
        ( model, Cmd.none )

    else
        let
            norm =
                Shortcuts.normalizeKey keyEvent.key
        in
        case model.page of
            RacePage _ ->
                if not keyEvent.repeat && norm == "r" then
                    update RespawnCheckpoint model

                else
                    ( { model | pressedKeys = Set.insert norm model.pressedKeys }, Cmd.none )

            BuildPage _ ->
                -- The editor's studio handles its own keys.
                ( model, Cmd.none )

            _ ->
                if not keyEvent.repeat && norm == "2" then
                    update ToggleRaceMode model

                else
                    ( model, Cmd.none )


stepFrame : Float -> Model -> ( Model, Cmd Msg )
stepFrame deltaMs model =
    let
        dt =
            clamp 0.001 0.05 (deltaMs / 1000.0)
    in
    case model.page of
        SetupPage garage ->
            let
                nextCam =
                    RacerCamera.updateGarageCamera dt garage.camera

                nextSpin =
                    garage.wheelPreviewSpin + dt * 2.8
            in
            ( { model
                | page =
                    SetupPage
                        { garage
                            | camera = nextCam
                            , wheelPreviewSpin = nextSpin
                        }
              }
            , Cmd.none
            )

        RacePage race ->
            let
                ( nextModel, raceCmd ) =
                    stepRaceFrame dt race model

                telemetryCmd =
                    case race.multiplayer of
                        Just room ->
                            let
                                p =
                                    race.physics

                                prog =
                                    clamp 0.0 1.0 (p.progressS / max 1.0 race.track.totalLengthM)

                                spd =
                                    Vec3.length p.velocity

                                isFin =
                                    case race.subPhase of
                                        ActiveDriving ->
                                            False

                                        _ ->
                                            p.progressS >= (race.track.totalLengthM - 5.0)
                            in
                            GamePorts.sendMultiplayerTelemetry
                                { pos = [ Vec3.getX p.position, Vec3.getY p.position, Vec3.getZ p.position ]
                                , rot = [ p.pitch, p.yaw, p.roll ]
                                , speed = spd
                                , progress = prog
                                , finished = isFin
                                }

                        Nothing ->
                            Cmd.none
            in
            applyRoomContacts dt ( nextModel, Cmd.batch [ raceCmd, telemetryCmd ] )
                |> sendRoomEvents

        _ ->
            ( model, Cmd.none )


{-| Whether this race is the local hot-seat split screen: two players on one
device, each with half of it. An online room never is: there every player is on
their own device, and their own view fills it.
-}
localSplitScreen : Model -> Bool
localSplitScreen model =
    model.raceMode == SplitScreenTwoPlayer && model.multiplayer == Nothing


{-| Sideways offset (m) of this player's start spot in an online room: the two
players of a two-player room start where the hot-seat players do, the players
of a bigger room side by side 1.3 m apart (more than two cars' contact
distance) in slot order.
-}
roomStartOffset : RoomState -> Float
roomStartOffset room =
    let
        slotIndex =
            clamp 0 7 (room.mySlot - 1)
    in
    if room.maxPlayers <= 2 then
        if slotIndex == 0 then
            -1.8

        else
            1.8

    else
        let
            side =
                if modBy 2 slotIndex == 0 then
                    -1.0

                else
                    1.0
        in
        side * (0.65 + 1.3 * toFloat (slotIndex // 2))


{-| What the other racers of an online room do in this page's race, worked
out from their shared position, heading, speed and progress.

Collisions are also told as room events (`applyRoomEvents`, `sendRoomEvents`):
the racer whose car smashes a piece or bumps another car tells the room, and
every page replays it. The inference here stays as the fallback for racers
whose game does not send events, and for events still on their way: each page
smashes the scenery another car is seen ploughing through
(`Physics.remoteSmashes`), and bumps its own car off another car it runs into
(`resolveCarsCollision`, keeping only this car's half). Neither path applies a
collision twice: a piece that is already gone does not shatter again, and a
bump starts the cooldown that both paths respect.

-}
applyRoomContacts : Float -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyRoomContacts dt ( model, cmd ) =
    case model.page of
        RacePage race ->
            case race.multiplayer of
                Just room ->
                    let
                        ( contacted, cmds ) =
                            roomContacts dt room model race
                    in
                    ( { model | page = RacePage contacted }, Cmd.batch (cmd :: cmds) )

                Nothing ->
                    ( model, cmd )

        _ ->
            ( model, cmd )


roomContacts : Float -> RoomState -> Model -> RaceState -> ( RaceState, List (Cmd Msg) )
roomContacts dt room model race =
    let
        others =
            List.filter (\p -> p.slot /= room.mySlot) room.players

        lookOf =
            remoteCarLook model

        telemetryCar p =
            { position = p.position
            , yaw = Vec3.getY p.rotation
            , speed = p.speed
            , progressS = p.progress * race.track.totalLengthM
            }

        smashes =
            others
                |> List.concatMap
                    (\p ->
                        Physics.remoteSmashes race.track
                            (lookOf p.vehicleId |> Maybe.map .spec |> Maybe.withDefault race.vehicle)
                            (telemetryCar p)
                    )

        ( smashed, smashCmds ) =
            List.foldl (roomSmash race.physics.position) ( race, [] ) smashes

        cooled =
            { smashed | roomHitCooldown = max 0.0 (smashed.roomHitCooldown - dt) }
    in
    if cooled.subPhase == ActiveDriving then
        List.foldl (bumpRoomCar lookOf) ( cooled, smashCmds ) (List.filter (\p -> not p.finished) others)

    else
        ( cooled, smashCmds )


{-| Another racer's smash, seen on this page: the piece goes and its bricks
fly, with a pop that fades with distance from this car. No banner or camera
shake: those are for the racer who hit it.
-}
roomSmash :
    Vec3
    -> PhysicsEvent
    -> ( RaceState, List (Cmd Msg) )
    -> ( RaceState, List (Cmd Msg) )
roomSmash listener ev ( race, cmds ) =
    case ev of
        ScenerySmashed smash ->
            case shatterScenery smash race of
                Just ( shattered, at ) ->
                    let
                        loudness =
                            clamp 0.0 1.0 (1.0 - Vec3.distance listener at / 40.0)
                    in
                    ( shattered
                    , if loudness > 0.05 then
                        Ports.playSfx { kind = "smash", intensity = loudness * clamp 0.3 1.0 (smash.impactSpeed / 14.0) } :: cmds

                      else
                        cmds
                    )

                Nothing ->
                    ( race, cmds )

        _ ->
            ( race, cmds )


{-| This car running into another racer's car, as last seen in the room: this
car takes its half of the push and the damage, and both cars' bricks fly. A
short cooldown keeps one bump from counting again while the other car's next
telemetry is still on its way.
-}
bumpRoomCar :
    (String -> Maybe { spec : VehicleSpec, meshes : CarMeshBundle })
    -> Multiplayer.RemotePlayer
    -> ( RaceState, List (Cmd Msg) )
    -> ( RaceState, List (Cmd Msg) )
bumpRoomCar lookOf p ( race, cmds ) =
    let
        local =
            race.physics

        other =
            { local
                | position = p.position
                , velocity = Vec3.scale p.speed (Physics.forwardVector (Vec3.getY p.rotation))
            }

        res =
            resolveCarsCollision local other
    in
    if res.collided && race.roomHitCooldown <= 0.0 then
        let
            midPos =
                Vec3.scale 0.5 (Vec3.add res.p1.position p.position)

            told =
                tellRoom
                    (bumpEvent race.elapsedTime
                        midPos
                        { target = p.slot
                        , impulse = Vec3.sub res.p2.velocity other.velocity
                        , damage = carBumpDamage res.impactSpeed
                        , impactSpeed = res.impactSpeed
                        }
                    )
                    race
        in
        if res.impactSpeed > 1.8 then
            let
                palette =
                    race.meshes.debrisColors
                        ++ (lookOf p.vehicleId |> Maybe.map (\look -> look.meshes.debrisColors) |> Maybe.withDefault [])

                ( pieces, nextSeed ) =
                    spawnDebrisBurst race.debrisSeed midPos res.p1.progressS palette
            in
            ( { told
                | physics = res.p1
                , debris = List.take maxDebrisPieces (pieces ++ race.debris)
                , debrisSeed = nextSeed
                , shake = min 1.0 (race.shake + 0.5)
                , roomHitCooldown = roomHitCooldownSeconds
              }
            , Ports.playSfx { kind = "smash", intensity = clamp 0.4 1.0 (res.impactSpeed / 8.0) } :: cmds
            )

        else
            ( { told | physics = res.p1, roomHitCooldown = roomHitCooldownSeconds }, cmds )

    else if res.collided then
        -- Still cooling down from the last bump: only keep the cars apart.
        ( { race | physics = { local | position = res.p1.position } }, cmds )

    else
        ( { race | physics = res.p1 }, cmds )


{-| Seconds after a bump with another room car before the next one counts.
-}
roomHitCooldownSeconds : Float
roomHitCooldownSeconds =
    0.35


{-| Keep a room event of this car's to send this frame (`sendRoomEvents`);
nothing outside an online room.
-}
tellRoom : Encode.Value -> RaceState -> RaceState
tellRoom event race =
    case race.multiplayer of
        Just _ ->
            { race | roomEventsOut = event :: race.roomEventsOut }

        Nothing ->
            race


{-| Send the room events this frame made (`Game.Ports.sendMultiplayerEvents`):
the runtime gives each its id and sends it with this car's telemetry.
-}
sendRoomEvents : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendRoomEvents ( model, cmd ) =
    case model.page of
        RacePage race ->
            if List.isEmpty race.roomEventsOut then
                ( model, cmd )

            else
                ( { model | page = RacePage { race | roomEventsOut = [] } }
                , Cmd.batch [ cmd, GamePorts.sendMultiplayerEvents (List.reverse race.roomEventsOut) ]
                )

        _ ->
            ( model, cmd )


{-| This car smashed a scenery piece (by its id in the course, which is the
same on every page): what the other pages need to shatter it the same way.
-}
smashEvent : Float -> Vec3 -> { obstacleId : Int, impactSpeed : Float, direction : Vec3 } -> Encode.Value
smashEvent at position smash =
    Multiplayer.encodeEvent
        { kind = "smash"
        , at = at
        , position = position
        , fields =
            [ ( "obstacleId", Encode.int smash.obstacleId )
            , ( "impactSpeed", Encode.float smash.impactSpeed )
            , ( "direction", encodeVec3 smash.direction )
            ]
        }


{-| This car bumped the car of room slot `target`: the change of velocity
(m/s) and the damage this page worked out for that car.
-}
bumpEvent : Float -> Vec3 -> { target : Int, impulse : Vec3, damage : Float, impactSpeed : Float } -> Encode.Value
bumpEvent at position bump =
    Multiplayer.encodeEvent
        { kind = "bump"
        , at = at
        , position = position
        , fields =
            [ ( "target", Encode.int bump.target )
            , ( "impulse", encodeVec3 bump.impulse )
            , ( "damage", Encode.float bump.damage )
            , ( "impactSpeed", Encode.float bump.impactSpeed )
            ]
        }


encodeVec3 : Vec3 -> Encode.Value
encodeVec3 v =
    Encode.list Encode.float [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]


decodeVec3 : Decoder Vec3
decodeVec3 =
    Decode.list Decode.float
        |> Decode.andThen
            (\list ->
                case list of
                    [ x, y, z ] ->
                        Decode.succeed (vec3 x y z)

                    _ ->
                        Decode.fail "expected [x, y, z]"
            )


{-| A room event as this game reads it.
-}
type RoomCollision
    = RoomSmash { obstacleId : Int, impactSpeed : Float, direction : Vec3 }
    | RoomBump { target : Int, impulse : Vec3, damage : Float, impactSpeed : Float }


roomCollisionOf : Multiplayer.RoomEvent -> Maybe RoomCollision
roomCollisionOf ev =
    let
        decoder =
            case ev.kind of
                "smash" ->
                    Decode.map3 (\id speed dir -> RoomSmash { obstacleId = id, impactSpeed = speed, direction = dir })
                        (Decode.field "obstacleId" Decode.int)
                        (Decode.field "impactSpeed" Decode.float)
                        (Decode.field "direction" decodeVec3)

                "bump" ->
                    Decode.map4 (\target impulse damage speed -> RoomBump { target = target, impulse = impulse, damage = damage, impactSpeed = speed })
                        (Decode.field "target" Decode.int)
                        (Decode.field "impulse" decodeVec3)
                        (Decode.field "damage" Decode.float)
                        (Decode.field "impactSpeed" Decode.float)

                _ ->
                    Decode.fail "not a collision"
    in
    Decode.decodeValue decoder ev.data |> Result.toMaybe


{-| Replay the other racers' collision events of a room state on this page.
The runtime hands each event over once.

  - A smash shatters the same piece, with its bricks and a pop (`roomSmash`);
    a piece this page already smashed (seen through `Physics.remoteSmashes`)
    stays gone.
  - A bump of this car pushes it by the change of velocity the other page
    worked out and costs the same damage, unless this page already counted
    the bump itself (the cooldown is running): the first of the two wins.
  - A bump between two other cars scatters their bricks here too.

-}
applyRoomEvents : Model -> RoomState -> RaceState -> ( RaceState, List (Cmd Msg) )
applyRoomEvents model room race =
    let
        lookOf =
            remoteCarLook model

        colorsOfSlot slot =
            room.players
                |> List.filter (\p -> p.slot == slot)
                |> List.head
                |> Maybe.andThen (\p -> lookOf p.vehicleId)
                |> Maybe.map (\look -> look.meshes.debrisColors)
                |> Maybe.withDefault []

        burst at palette impactSpeed ( r, cmds ) =
            let
                ( pieces, nextSeed ) =
                    spawnDebrisBurst r.debrisSeed at r.physics.progressS palette

                loudness =
                    clamp 0.0 1.0 (1.0 - Vec3.distance r.physics.position at / 40.0)
            in
            ( { r | debris = List.take maxDebrisPieces (pieces ++ r.debris), debrisSeed = nextSeed }
            , if loudness > 0.05 then
                Ports.playSfx { kind = "smash", intensity = loudness * clamp 0.4 1.0 (impactSpeed / 8.0) } :: cmds

              else
                cmds
            )

        apply ev ( r, cmds ) =
            case roomCollisionOf ev of
                Just (RoomSmash smash) ->
                    roomSmash r.physics.position (ScenerySmashed smash) ( r, cmds )

                Just (RoomBump bump) ->
                    if bump.target == room.mySlot then
                        if r.subPhase == ActiveDriving && r.roomHitCooldown <= 0.0 then
                            let
                                phys =
                                    r.physics

                                pushed =
                                    { r
                                        | physics =
                                            { phys
                                                | velocity = Vec3.add phys.velocity bump.impulse
                                                , integrity = max 0.0 (phys.integrity - bump.damage)
                                            }
                                        , roomHitCooldown = roomHitCooldownSeconds
                                    }
                            in
                            if bump.damage > 0.0 then
                                burst ev.position (r.meshes.debrisColors ++ colorsOfSlot ev.slot) bump.impactSpeed ( { pushed | shake = min 1.0 (r.shake + 0.5) }, cmds )

                            else
                                ( pushed, cmds )

                        else
                            ( r, cmds )

                    else if bump.damage > 0.0 then
                        burst ev.position (colorsOfSlot ev.slot ++ colorsOfSlot bump.target) bump.impactSpeed ( r, cmds )

                    else
                        ( r, cmds )

                Nothing ->
                    ( r, cmds )
    in
    List.foldl apply ( race, [] ) room.events


stepRaceFrame : Float -> RaceState -> Model -> ( Model, Cmd Msg )
stepRaceFrame dt race model =
    let
        nextSplitFlash =
            race.splitFlash
                |> Maybe.andThen
                    (\f ->
                        let
                            rem =
                                f.timer - dt
                        in
                        if rem > 0 then
                            Just { f | timer = rem }

                        else
                            Nothing
                    )

        nextAlert =
            race.hudAlert
                |> Maybe.andThen
                    (\a ->
                        let
                            rem =
                                a.timer - dt
                        in
                        if rem > 0.0 then
                            Just { a | timer = rem }

                        else
                            Nothing
                    )

        steppedDebris =
            stepDebrisParticles dt race.track race.debris

        decayedShake =
            race.shake * max 0.0 (1.0 - dt * 5.0)
    in
    case race.subPhase of
        CountdownPhase rem lastBeep ->
            let
                nextRem =
                    rem - dt

                nextCam =
                    RacerCamera.updateRaceCamera dt race.cameraMode race.physics race.vehicle race.camera

                curInt =
                    max 0 (ceiling nextRem)

                ( sfxCmd, nextBeep ) =
                    if nextRem <= 0.0 then
                        ( Ports.playSfx { kind = "go", intensity = 1.0 }, 0 )

                    else if curInt < lastBeep && curInt >= 1 then
                        ( Ports.playSfx { kind = "countdown", intensity = 0.7 }, curInt )

                    else
                        ( Cmd.none, lastBeep )

                nextSubPhase =
                    if nextRem <= 0.0 then
                        ActiveDriving

                    else
                        CountdownPhase nextRem nextBeep

                nextP2 =
                    race.player2
                        |> Maybe.map
                            (\p2 ->
                                let
                                    p2Cam =
                                        RacerCamera.updateRaceCamera dt race.cameraMode p2.physics p2.vehicle p2.camera
                                in
                                { p2
                                    | subPhase = nextSubPhase
                                    , camera = p2Cam
                                }
                            )

                nextRace =
                    { race
                        | subPhase = nextSubPhase
                        , hudAlert = nextAlert
                        , splitFlash = nextSplitFlash
                        , debris = steppedDebris
                        , shake = decayedShake
                        , camera = nextCam
                        , player2 = nextP2
                    }

                nextModel =
                    { model | page = RacePage nextRace }
            in
            ( nextModel, Cmd.batch [ sfxCmd, raceAudioCmd nextModel ] )

        CrashRecoveryPhase rem reason ->
            let
                nextRem =
                    rem - dt

                nextCam =
                    RacerCamera.updateRaceCamera dt race.cameraMode race.physics race.vehicle race.camera
            in
            if nextRem <= 0.0 then
                let
                    respawned =
                        Physics.respawnAtCheckpoint race.physics

                    resetCam =
                        RacerCamera.initRaceCamera model.viewportHeight respawned race.vehicle

                    nextRace =
                        { race
                            | physics = respawned
                            , accumulator = 0.0
                            , subPhase = ActiveDriving
                            , hudAlert = nextAlert
                            , debris = steppedDebris
                            , shake = 0.0
                            , camera = resetCam
                        }

                    nextModel =
                        { model | page = RacePage nextRace }
                in
                ( nextModel, raceAudioCmd nextModel )

            else
                let
                    nextRace =
                        { race
                            | subPhase = CrashRecoveryPhase nextRem reason
                            , hudAlert = nextAlert
                            , debris = steppedDebris
                            , shake = decayedShake
                            , camera = applyShake decayedShake nextRem nextCam
                        }

                    nextModel =
                        { model | page = RacePage nextRace }
                in
                ( nextModel, silenceAudioCmd )

        CelebrationPhase celeb ->
            let
                nextTimer =
                    celeb.timer + dt

                brakeFactor =
                    max 0.0 (1.0 - dt * 2.2)

                p1Phys =
                    race.physics

                p1StoppedVel =
                    Vec3.scale brakeFactor p1Phys.velocity

                p1StoppedPos =
                    Vec3.add p1Phys.position (Vec3.scale dt p1StoppedVel)

                nextP1Phys =
                    { p1Phys
                        | position = p1StoppedPos
                        , velocity = p1StoppedVel
                    }

                nextCam =
                    RacerCamera.updateRaceCamera dt race.cameraMode nextP1Phys race.vehicle race.camera

                ( extraDebris, nextSeed ) =
                    if nextTimer < 2.5 && modBy 8 (round (nextTimer * 60.0)) == 0 then
                        spawnConfettiBurst race.debrisSeed p1StoppedPos p1Phys.progressS

                    else
                        ( [], race.debrisSeed )

                nextP2 =
                    case race.player2 of
                        Just p2 ->
                            let
                                p2Phys =
                                    p2.physics

                                p2StoppedVel =
                                    Vec3.scale brakeFactor p2Phys.velocity

                                p2StoppedPos =
                                    Vec3.add p2Phys.position (Vec3.scale dt p2StoppedVel)

                                nextP2Phys =
                                    { p2Phys
                                        | position = p2StoppedPos
                                        , velocity = p2StoppedVel
                                    }

                                p2Cam =
                                    RacerCamera.updateRaceCamera dt race.cameraMode nextP2Phys p2.vehicle p2.camera
                            in
                            Just { p2 | physics = nextP2Phys, camera = p2Cam }

                        Nothing ->
                            Nothing

                nextRace =
                    { race
                        | subPhase = CelebrationPhase { celeb | timer = nextTimer }
                        , physics = nextP1Phys
                        , camera = nextCam
                        , player2 = nextP2
                        , debris = List.take maxDebrisPieces (extraDebris ++ steppedDebris)
                        , debrisSeed = nextSeed
                    }
            in
            ( { model | page = RacePage nextRace }, Cmd.none )

        ActiveDriving ->
            let
                input =
                    sampleP1Input model

                ( nextAccum, nextPhysics, events ) =
                    Physics.stepFrame dt race.accumulator race.track race.vehicle input race.physics

                nextElapsed =
                    race.elapsedTime + dt

                instSpeedKmh =
                    Vec3.length nextPhysics.velocity * 3.6

                nextTopSpeed =
                    max race.topSpeedKmh instSpeedKmh

                nextCam =
                    RacerCamera.updateRaceCamera dt race.cameraMode nextPhysics race.vehicle race.camera

                ( nextP2, p2Sfx, p2Smashes ) =
                    case race.player2 of
                        Nothing ->
                            ( Nothing, [], [] )

                        Just p2 ->
                            case p2.subPhase of
                                CelebrationPhase _ ->
                                    ( Just p2, [], [] )

                                CountdownPhase _ _ ->
                                    ( Just { p2 | subPhase = ActiveDriving }, [], [] )

                                CrashRecoveryPhase p2Rem p2Reason ->
                                    let
                                        p2NextRem =
                                            p2Rem - dt

                                        p2Cam =
                                            RacerCamera.updateRaceCamera dt race.cameraMode p2.physics p2.vehicle p2.camera
                                    in
                                    if p2NextRem <= 0.0 then
                                        let
                                            p2Respawned =
                                                Physics.respawnAtCheckpoint p2.physics

                                            isWide =
                                                model.viewportWidth >= model.viewportHeight

                                            p2H =
                                                if not isWide then
                                                    model.viewportHeight - (model.viewportHeight // 2)

                                                else
                                                    model.viewportHeight

                                            p2ResetCam =
                                                RacerCamera.initRaceCamera p2H p2Respawned p2.vehicle
                                        in
                                        ( Just
                                            { p2
                                                | physics = p2Respawned
                                                , accumulator = 0.0
                                                , subPhase = ActiveDriving
                                                , camera = p2ResetCam
                                            }
                                        , []
                                        , []
                                        )

                                    else
                                        ( Just
                                            { p2
                                                | subPhase = CrashRecoveryPhase p2NextRem p2Reason
                                                , camera = p2Cam
                                            }
                                        , []
                                        , []
                                        )

                                ActiveDriving ->
                                    let
                                        p2Input =
                                            sampleP2Input model

                                        ( p2Accum, p2NextPhys, p2Events ) =
                                            Physics.stepFrame dt p2.accumulator race.track p2.vehicle p2Input p2.physics

                                        p2SpeedKmh =
                                            Vec3.length p2NextPhys.velocity * 3.6

                                        p2TopSpeed =
                                            max p2.topSpeedKmh p2SpeedKmh

                                        p2Cam =
                                            RacerCamera.updateRaceCamera dt race.cameraMode p2NextPhys p2.vehicle p2.camera

                                        p2Crash =
                                            List.filterMap
                                                (\ev ->
                                                    case ev of
                                                        Crashed r ->
                                                            Just r

                                                        _ ->
                                                            Nothing
                                                )
                                                p2Events
                                                |> List.head

                                        ( p2NextSubphase, crashSfx ) =
                                            case p2Crash of
                                                Just r ->
                                                    ( CrashRecoveryPhase (crashRecoverySeconds r) r
                                                    , [ Ports.playSfx { kind = "impact", intensity = 1.0 } ]
                                                    )

                                                Nothing ->
                                                    ( ActiveDriving, [] )
                                    in
                                    ( Just
                                        { p2
                                            | physics = p2NextPhys
                                            , accumulator = p2Accum
                                            , topSpeedKmh = p2TopSpeed
                                            , subPhase = p2NextSubphase
                                            , camera = p2Cam
                                        }
                                    , crashSfx
                                    , List.filter isScenerySmash p2Events
                                    )

                carHit =
                    case nextP2 of
                        Just p2Active ->
                            if p2Active.subPhase == ActiveDriving then
                                let
                                    res =
                                        resolveCarsCollision nextPhysics p2Active.physics
                                in
                                { p1 = res.p1
                                , p2 = Just { p2Active | physics = res.p2 }
                                , impactSpeed = res.impactSpeed
                                , collided = res.collided
                                }

                            else
                                { p1 = nextPhysics, p2 = nextP2, impactSpeed = 0.0, collided = False }

                        Nothing ->
                            { p1 = nextPhysics, p2 = Nothing, impactSpeed = 0.0, collided = False }

                resolvedP1 =
                    carHit.p1

                resolvedP2 =
                    carHit.p2

                carImpactSpeed =
                    carHit.impactSpeed

                carCollided =
                    carHit.collided

                ( carCollisionDebris, debrisSeedAfterCollision ) =
                    if carCollided && carImpactSpeed > 1.8 then
                        let
                            p2Pos =
                                resolvedP2
                                    |> Maybe.map (\p2A -> p2A.physics.position)
                                    |> Maybe.withDefault resolvedP1.position

                            midPos =
                                Vec3.scale 0.5 (Vec3.add resolvedP1.position p2Pos)

                            debrisPal =
                                race.meshes.debrisColors
                                    ++ (resolvedP2 |> Maybe.map (\p2A -> p2A.meshes.debrisColors) |> Maybe.withDefault [])
                        in
                        spawnDebrisBurst race.debrisSeed midPos resolvedP1.progressS debrisPal

                    else
                        ( [], race.debrisSeed )

                collisionSfx =
                    if carCollided && carImpactSpeed > 1.8 then
                        [ Ports.playSfx { kind = "smash", intensity = clamp 0.4 1.0 (carImpactSpeed / 8.0) } ]

                    else
                        []

                updatedEvents =
                    if resolvedP1.integrity <= 0.0 then
                        Crashed StructuralCollapse :: events ++ p2Smashes

                    else
                        events ++ p2Smashes

                finalP2 =
                    case resolvedP2 of
                        Just p2A ->
                            if p2A.physics.integrity <= 0.0 && p2A.subPhase == ActiveDriving then
                                Just { p2A | subPhase = CrashRecoveryPhase (crashRecoverySeconds StructuralCollapse) StructuralCollapse }

                            else
                                Just p2A

                        Nothing ->
                            Nothing

                ( p2WreckPieces, debrisSeedAfterP2 ) =
                    case ( race.player2, finalP2 ) of
                        ( Just before, Just after ) ->
                            if before.subPhase == ActiveDriving && after.subPhase == CrashRecoveryPhase (crashRecoverySeconds StructuralCollapse) StructuralCollapse then
                                spawnWreckDebris debrisSeedAfterCollision after.physics after.vehicle after.meshes.debrisColors

                            else
                                ( [], debrisSeedAfterCollision )

                        _ ->
                            ( [], debrisSeedAfterCollision )

                collidedShake =
                    if carCollided && carImpactSpeed > 1.8 then
                        min 1.0 (decayedShake + 0.5)

                    else
                        decayedShake

                p2WinEvents =
                    case finalP2 of
                        Just p2A ->
                            if p2A.physics.progressS >= race.track.totalLengthM && not (List.member FinishedRace updatedEvents) then
                                [ FinishedRace ]

                            else
                                []

                        Nothing ->
                            []

                baseRace =
                    { race
                        | physics = resolvedP1
                        , accumulator = nextAccum
                        , elapsedTime = nextElapsed
                        , topSpeedKmh = nextTopSpeed
                        , hudAlert = nextAlert
                        , debris = List.take maxDebrisPieces (p2WreckPieces ++ carCollisionDebris ++ steppedDebris)
                        , debrisSeed = debrisSeedAfterP2
                        , shake = collidedShake
                        , camera = applyShake collidedShake nextElapsed nextCam
                        , player2 = finalP2
                    }
            in
            processPhysicsEvents (p2WinEvents ++ updatedEvents) (collisionSfx ++ p2Sfx) baseRace model


processPhysicsEvents : List PhysicsEvent -> List (Cmd Msg) -> RaceState -> Model -> ( Model, Cmd Msg )
processPhysicsEvents events extraCmds race model =
    if List.member FinishedRace events then
        let
            finalSplits =
                appendFinishSplit events race.elapsedTime race.splitTimes

            entry =
                { mapCode = Map.leaderboardKey race.track.config.map
                , vehicleName = race.vehicle.name
                , timeSeconds = race.elapsedTime
                , topSpeedKmh = race.topSpeedKmh
                , wheelCount = List.length race.vehicle.wheels
                , steeringLabel = RI18n.steeringMechName model.language race.vehicle.steering
                }

            ( nextLb, isNewBest ) =
                Leaderboard.recordRun entry model.leaderboard

            results =
                { document = race.document
                , title = race.title
                , vehicle = race.vehicle
                , meshes = race.meshes
                , map = race.track.config.map
                , finishTimeS = race.elapsedTime
                , topSpeedKmh = race.topSpeedKmh
                , finalIntegrityPct = clamp 0 100 (round race.physics.integrity)
                , splitTimes = finalSplits
                , isNewBest = isNewBest
                }

            winnerName =
                case race.player2 of
                    Just p2 ->
                        if race.physics.progressS >= race.track.totalLengthM then
                            case model.language of
                                Fi ->
                                    "Pelaaja 1 (" ++ race.title ++ ")"

                                En ->
                                    "Player 1 (" ++ race.title ++ ")"

                        else
                            case model.language of
                                Fi ->
                                    "Pelaaja 2 (" ++ p2.vehicle.name ++ ")"

                                En ->
                                    "Player 2 (" ++ p2.vehicle.name ++ ")"

                    Nothing ->
                        race.title

            ( confettiPieces, nextSeed ) =
                spawnConfettiBurst race.debrisSeed race.physics.position race.physics.progressS

            celebPhase =
                CelebrationPhase
                    { timer = 0.0
                    , winner = winnerName
                    , confettiSpawned = True
                    , results = results
                    }

            updatedRace =
                { race
                    | subPhase = celebPhase
                    , debris = List.take maxDebrisPieces (confettiPieces ++ race.debris)
                    , debrisSeed = nextSeed
                }
        in
        ( { model
            | leaderboard = nextLb
            , lastMap = Just race.track.config.map
            , page = RacePage updatedRace
            , pressedKeys = Set.empty
            , touch = emptyTouch
          }
        , Cmd.batch
            ([ Ports.playSfx { kind = "finish", intensity = 1.0 }
             , Ports.playSfx { kind = "fanfare", intensity = 1.0 }
             , silenceAudioCmd
             , GamePorts.saveLeaderboard (Leaderboard.encodeLeaderboard nextLb)
             ]
                ++ extraCmds
            )
        )

    else
        let
            stepEv ev ( rAcc, cmdAcc ) =
                case ev of
                    Impact { impulse, scrape } ->
                        let
                            bumped =
                                { rAcc | shake = min 1.0 (rAcc.shake + impactShake scrape impulse) }
                        in
                        if not scrape && impulse > 1.2 then
                            ( bumped
                            , Ports.playSfx { kind = "impact", intensity = clamp 0.15 1.0 (impulse / 18.0) } :: cmdAcc
                            )

                        else
                            ( bumped, cmdAcc )

                    Landing { impulse } ->
                        let
                            bumped =
                                { rAcc | shake = min 1.0 (rAcc.shake + landingShake impulse) }
                        in
                        if impulse > 1.0 then
                            ( bumped
                            , Ports.playSfx { kind = "landing", intensity = clamp 0.15 1.0 (impulse / 16.0) } :: cmdAcc
                            )

                        else
                            ( bumped, cmdAcc )

                    BrickDetached _ ->
                        let
                            ( spawned, nextSeed ) =
                                spawnDebrisBurst rAcc.debrisSeed rAcc.physics.position rAcc.physics.progressS rAcc.meshes.debrisColors
                        in
                        ( { rAcc
                            | debris = List.take maxDebrisPieces (spawned ++ rAcc.debris)
                            , debrisSeed = nextSeed
                          }
                        , Ports.playSfx { kind = "pop", intensity = 0.85 } :: cmdAcc
                        )

                    ScenerySmashed smash ->
                        smashScenery model.language smash ( rAcc, cmdAcc )

                    CheckpointCrossed { index } ->
                        let
                            splitT =
                                rAcc.elapsedTime

                            frac =
                                toFloat (index + 1) / 4.0

                            refTrackTime =
                                Leaderboard.bestForMap rAcc.track.config.map model.leaderboard
                                    |> Maybe.map .timeSeconds
                                    |> Maybe.withDefault (rAcc.track.totalLengthM / 22.0)

                            delta =
                                splitT - refTrackTime * frac

                            alert =
                                { title = RI18n.checkpointBanner model.language (index + 1)
                                , subtitle = Just (Leaderboard.formatRaceTime splitT ++ " (" ++ Leaderboard.formatSplitDelta delta ++ ")")
                                , positive = delta <= 0.0
                                , timer = 2.0
                                }
                        in
                        ( { rAcc
                            | splitTimes = rAcc.splitTimes ++ [ splitT ]
                            , splitDelta = Just delta
                            , splitFlash = Just { time = splitT, timer = 3.0 }
                            , hudAlert = Just alert
                          }
                        , Ports.playSfx { kind = "checkpoint", intensity = 0.9 } :: cmdAcc
                        )

                    Crashed StructuralCollapse ->
                        -- Out of health: the car breaks apart into its bricks and is
                        -- rebuilt at the last checkpoint once the overlay has told the player.
                        let
                            ( pieces, nextSeed ) =
                                spawnWreckDebris rAcc.debrisSeed rAcc.physics rAcc.vehicle rAcc.meshes.debrisColors
                        in
                        ( { rAcc
                            | subPhase = CrashRecoveryPhase (crashRecoverySeconds StructuralCollapse) StructuralCollapse
                            , debris = List.take maxDebrisPieces (pieces ++ rAcc.debris)
                            , debrisSeed = nextSeed
                            , shake = 1.0
                          }
                        , Ports.playSfx { kind = "smash", intensity = 1.0 } :: Ports.playSfx { kind = "impact", intensity = 1.0 } :: cmdAcc
                        )

                    Crashed reason ->
                        ( { rAcc | subPhase = CrashRecoveryPhase (crashRecoverySeconds reason) reason }
                        , Ports.playSfx { kind = "impact", intensity = 1.0 } :: cmdAcc
                        )

                    FinishedRace ->
                        ( rAcc, cmdAcc )

            ( finalRace, sfxList ) =
                List.foldl stepEv ( race, [] ) events

            nextModel =
                { model | page = RacePage finalRace }

            audioCmd =
                raceAudioCmd nextModel
        in
        ( nextModel, Cmd.batch (audioCmd :: sfxList ++ extraCmds) )


appendFinishSplit : List PhysicsEvent -> Float -> List Float -> List Float
appendFinishSplit events elapsed existing =
    let
        hasFinishCp =
            List.any
                (\ev ->
                    case ev of
                        CheckpointCrossed { index } ->
                            index == 3

                        _ ->
                            False
                )
                events
    in
    if hasFinishCp then
        existing ++ [ elapsed ]

    else
        existing


{-| A selection with every part fixed to the map's values, so a shared
link shows its concrete map.
-}
exactSelection : MapParams -> Selection
exactSelection map =
    { biome = Exactly map.biome
    , season = Exactly map.season
    , weather = Exactly map.weather
    , seed = map.seed
    }


{-| Set the picker selection and the map it resolves to.
-}
withSelection : Selection -> Model -> Model
withSelection selection model =
    let
        resolved =
            Map.resolve selection
    in
    { model
        | selection = selection
        , map = resolved
        , seedText = Map.toCode resolved
        , shareCopiedNotice = False
    }


{-| Switch to a map (as from a link) when one is given.
-}
withMap : Maybe MapParams -> Model -> Model
withMap maybeMap model =
    case maybeMap of
        Just map ->
            withSelection (exactSelection map) model

        Nothing ->
            model


{-| Change the selection from the map picker and write the new map into the
URL hash while the garage is shown.
-}
applySelection : (Selection -> Selection) -> Model -> ( Model, Cmd Msg )
applySelection change model =
    let
        nextModel =
            withSelection (change model.selection) model
    in
    case nextModel.page of
        SetupPage garage ->
            ( nextModel, GamePorts.setUrlHash (Ingest.encodeRaceHash nextModel.map garage.document) )

        _ ->
            ( nextModel, Cmd.none )


{-| The nearest seed after the map's own (in dice order) whose course has the
other steepness, keeping the map's biome, season and weather; `Nothing` when
none of the next 64 seeds has it.
-}
seedWithOtherSteepness : MapParams -> Maybe Int
seedWithOtherSteepness map =
    let
        current =
            Course.steepness map

        search tries seed =
            if tries <= 0 then
                Nothing

            else if Course.steepness { map | seed = seed } /= current then
                Just seed

            else
                search (tries - 1) (nextRandomSeed seed)
    in
    search 64 (nextRandomSeed map.seed)


{-| The next seed from the dice button, 1 to 999999.
-}
nextRandomSeed : Int -> Int
nextRandomSeed seed =
    modBy 999999 (abs (seed * 1664525 + 1013904223)) + 1


loadDocumentIntoGarage : String -> Document -> Maybe MapParams -> Bool -> Model -> ( Model, Cmd Msg )
loadDocumentIntoGarage suggestedTitle doc maybeMap syncHash model0 =
    let
        model =
            withMap maybeMap model0

        analyzed =
            Analyze.analyzeDocument suggestedTitle doc model.partCache model.partIndex

        stats =
            reachablePartStats doc model.partCache

        markedCache =
            Resolve.markLoading stats.needed model.partCache

        garage =
            { document = doc
            , title = suggestedTitle
            , vehicle = analyzed.spec
            , meshes = analyzed.meshes
            , camera = RacerCamera.initGarageCamera model.viewportHeight analyzed.spec
            , wheelPreviewSpin = 0.0
            }

        hashCmd =
            if syncHash then
                GamePorts.setUrlHash (Ingest.encodeRaceHash model.map doc)

            else
                Cmd.none

        fetchCmd =
            fetchPartsCmd model.resolverConfig stats.needed
    in
    ( { model
        | partCache = markedCache
        , pasteError = Nothing
        , shareCopiedNotice = False
        , pressedKeys = Set.empty
        , touch = emptyTouch
        , page = SetupPage garage
      }
    , Cmd.batch [ fetchCmd, hashCmd, silenceAudioCmd ]
    )


startRaceFromDoc : String -> Document -> VehicleSpec -> CarMeshBundle -> Model -> ( Model, Cmd Msg )
startRaceFromDoc title doc vehicle meshes model =
    let
        ( cachedTrack, modelWithTrack ) =
            ensureTrackBuilt model.map model

        isSplit =
            localSplitScreen model

        physics =
            if isSplit then
                Physics.initPhysicsWithOffset -1.8 cachedTrack.track vehicle

            else
                case model.multiplayer of
                    Just room ->
                        -- Every room racer starts in their own spot on the line,
                        -- so the cars do not start inside each other.
                        Physics.initPhysicsWithOffset (roomStartOffset room) cachedTrack.track vehicle

                    Nothing ->
                        Physics.initPhysics cachedTrack.track vehicle

        isWide =
            modelWithTrack.viewportWidth >= modelWithTrack.viewportHeight

        p1H =
            if isSplit && not isWide then
                modelWithTrack.viewportHeight // 2

            else
                modelWithTrack.viewportHeight

        cam =
            RacerCamera.initRaceCamera p1H physics vehicle

        maybeP2 =
            if isSplit then
                let
                    p2Car =
                        model.playerTwoCar

                    p2Physics =
                        Physics.initPhysicsWithOffset 1.8 cachedTrack.track p2Car.vehicle

                    p2H =
                        if not isWide then
                            modelWithTrack.viewportHeight - (modelWithTrack.viewportHeight // 2)

                        else
                            modelWithTrack.viewportHeight

                    p2Cam =
                        RacerCamera.initRaceCamera p2H p2Physics p2Car.vehicle
                in
                Just
                    { vehicle = p2Car.vehicle
                    , meshes = p2Car.meshes
                    , physics = p2Physics
                    , camera = p2Cam
                    , accumulator = 0.0
                    , subPhase = CountdownPhase 3.0 4
                    , topSpeedKmh = 0.0
                    , splitTimes = []
                    , splitDelta = Nothing
                    , hudAlert = Nothing
                    , debris = []
                    , debrisSeed = max 1 (modelWithTrack.map.seed * 97 + p2Car.vehicle.placementCount)
                    }

            else
                Nothing

        race =
            { document = doc
            , title = title
            , vehicle = vehicle
            , meshes = meshes
            , track = cachedTrack.track
            , bundle = cachedTrack.bundle
            , physics = physics
            , accumulator = 0.0
            , subPhase = CountdownPhase 3.0 4
            , elapsedTime = 0.0
            , topSpeedKmh = 0.0
            , splitTimes = []
            , splitDelta = Nothing
            , splitFlash = Nothing
            , hudAlert = Nothing
            , cameraMode = ChaseCam
            , camera = cam
            , debris = []
            , debrisSeed = max 1 (modelWithTrack.map.seed * 73 + vehicle.placementCount)
            , shake = 0.0
            , player2 = maybeP2
            , multiplayer = model.multiplayer
            , roomHitCooldown = 0.0
            , roomEventsOut = []
            }

        nextModel =
            { modelWithTrack
                | page = RacePage race
                , pressedKeys = Set.empty
                , touch = emptyTouch
                , shareCopiedNotice = False
            }
    in
    ( { nextModel | tiltPitch = TiltPitch.calibrate nextModel.tiltPitch }
    , Cmd.batch
        [ Ports.playSfx { kind = "countdown", intensity = 0.7 }
        , silenceAudioCmd
        , GamePorts.calibrateTilt ()
        ]
    )


ensureTrackBuilt : MapParams -> Model -> ( CachedTrack, Model )
ensureTrackBuilt map model =
    case model.cachedTrack of
        Just cached ->
            if cached.mapCode == Map.toCode map then
                ( cached, model )

            else
                buildAndStoreTrack map model

        Nothing ->
            buildAndStoreTrack map model


buildAndStoreTrack : MapParams -> Model -> ( CachedTrack, Model )
buildAndStoreTrack map model =
    let
        track =
            Track.generate (Track.trackConfigFor map)

        bundle =
            TerrainMesh.buildTrackRenderBundle track

        cached =
            { mapCode = Map.toCode map
            , track = track
            , bundle = bundle
            }
    in
    ( cached, { model | cachedTrack = Just cached } )


reanalyzeActiveVehicle : Model -> Model
reanalyzeActiveVehicle model =
    case model.page of
        SetupPage garage ->
            let
                analyzed =
                    Analyze.analyzeDocument garage.title garage.document model.partCache model.partIndex
            in
            { model
                | page =
                    SetupPage
                        { garage
                            | vehicle = analyzed.spec
                            , meshes = analyzed.meshes
                        }
            }

        RacePage race ->
            let
                analyzed =
                    Analyze.analyzeDocument race.title race.document model.partCache model.partIndex
            in
            { model
                | page =
                    RacePage
                        { race
                            | vehicle = analyzed.spec
                            , meshes = analyzed.meshes
                        }
            }

        FinishLeaderboardPage res ->
            let
                analyzed =
                    Analyze.analyzeDocument res.title res.document model.partCache model.partIndex
            in
            { model
                | page =
                    FinishLeaderboardPage
                        { res
                            | vehicle = analyzed.spec
                            , meshes = analyzed.meshes
                        }
            }

        BuildPage _ ->
            model


{-| Parts of a build still to fetch, and how far the rest has loaded.
-}
reachablePartStats : Document -> PartCache -> PartLoading.PartProgress
reachablePartStats doc cache =
    PartLoading.progress (PartLoading.mainAndSubfileParts doc) doc cache


{-| Build another player's car from the race hash they shared, once per
vehicle id, and fetch the parts it needs. A model that does not decode or has
more than `maxRemoteParts` parts is remembered as unusable.
-}
addRemoteCar : String -> String -> Model -> ( Model, Cmd Msg )
addRemoteCar id payload model =
    case Ingest.decodeRaceInput Nothing payload of
        Ok decoded ->
            if List.length (Document.flattenedPlacements decoded.document) > maxRemoteParts then
                ( { model | remoteCars = Dict.insert id Nothing model.remoteCars }, Cmd.none )

            else
                let
                    stats =
                        reachablePartStats decoded.document model.partCache

                    analyzed =
                        Analyze.analyzeDocument decoded.suggestedName decoded.document model.partCache model.partIndex

                    car =
                        { title = decoded.suggestedName
                        , document = decoded.document
                        , vehicle = analyzed.spec
                        , meshes = analyzed.meshes
                        , settled = List.isEmpty stats.needed && stats.inFlight == 0
                        }
                in
                ( { model
                    | remoteCars = Dict.insert id (Just car) model.remoteCars
                    , partCache = Resolve.markLoading stats.needed model.partCache
                  }
                , fetchPartsCmd model.resolverConfig stats.needed
                )

        Err _ ->
            ( { model | remoteCars = Dict.insert id Nothing model.remoteCars }, Cmd.none )


{-| After a part arrives: fetch what other players' cars still need, and
rebuild a car once all of its parts are in.
-}
refreshRemoteCars : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshRemoteCars ( model, cmd ) =
    let
        step id maybeCar ( cars, cache, needed ) =
            case maybeCar of
                Just car ->
                    if car.settled then
                        ( Dict.insert id maybeCar cars, cache, needed )

                    else
                        let
                            stats =
                                reachablePartStats car.document cache
                        in
                        if List.isEmpty stats.needed && stats.inFlight == 0 then
                            let
                                analyzed =
                                    Analyze.analyzeDocument car.title car.document cache model.partIndex
                            in
                            ( Dict.insert id (Just { car | vehicle = analyzed.spec, meshes = analyzed.meshes, settled = True }) cars, cache, needed )

                        else
                            ( Dict.insert id maybeCar cars, Resolve.markLoading stats.needed cache, needed ++ stats.needed )

                Nothing ->
                    ( Dict.insert id Nothing cars, cache, needed )

        ( nextCars, nextCache, allNeeded ) =
            Dict.foldl step ( Dict.empty, model.partCache, [] ) model.remoteCars
    in
    ( { model | remoteCars = nextCars, partCache = nextCache }
    , Cmd.batch [ cmd, fetchPartsCmd model.resolverConfig allNeeded ]
    )


{-| The look of another player's car by vehicle id, once it is built.
-}
remoteCarLook : Model -> String -> Maybe { spec : VehicleSpec, meshes : CarMeshBundle }
remoteCarLook model id =
    Dict.get id model.remoteCars
        |> Maybe.andThen identity
        |> Maybe.map (\car -> { spec = car.vehicle, meshes = car.meshes })


{-| Build the brick scenery models whose parts are in, and fetch the parts
the others still need.
-}
refreshProps : Model -> ( Model, Cmd Msg )
refreshProps model =
    let
        r =
            Scenery.refresh model.partCache model.props
    in
    ( { model | props = r.props, partCache = r.cache }, fetchPartsCmd model.resolverConfig r.fetch )


withProps : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withProps ( model, cmd ) =
    let
        ( next, propsCmd ) =
            refreshProps model
    in
    ( next, Cmd.batch [ cmd, propsCmd ] )


fetchPartsCmd : ResolverConfig -> List String -> Cmd Msg
fetchPartsCmd config names =
    PartLoading.fetchParts config PartFetched names


samplePlayerInput : Model -> InputState
samplePlayerInput model =
    sampleP1Input model


sampleP1Input : Model -> InputState
sampleP1Input model =
    let
        held k =
            Set.member k model.pressedKeys

        isSplit =
            localSplitScreen model

        keyLeft =
            held "a" || (not isSplit && held "ArrowLeft") || model.touch.left

        keyRight =
            held "d" || (not isSplit && held "ArrowRight") || model.touch.right

        keySteer =
            (if keyRight then
                1.0

             else
                0.0
            )
                - (if keyLeft then
                    1.0

                   else
                    0.0
                  )

        tiltSteer =
            if model.tilt.enabled && model.tilt.available then
                model.tilt.steer

            else
                0.0

        steer =
            if abs keySteer > 0.05 then
                keySteer

            else
                tiltSteer

        tiltBraking =
            model.tiltPitch.zone == TiltPitch.Backward

        brake =
            if held "s" || held "S" || (not isSplit && (held "ArrowDown" || held "Down")) || model.touch.brake || tiltBraking then
                1.0

            else
                0.0

        aeroTuck =
            if held "w" || (not isSplit && held "ArrowUp") || model.touch.tuck then
                1.0

            else
                0.0

        handbrake =
            held " " || held "Spacebar" || model.touch.drift

        leanX =
            steer * 0.85
    in
    { steer = steer
    , brake = brake
    , handbrake = handbrake
    , aeroTuck = aeroTuck
    , leanX = leanX
    }


sampleP2Input : Model -> InputState
sampleP2Input model =
    let
        held k =
            Set.member k model.pressedKeys

        keyLeft =
            held "ArrowLeft"

        keyRight =
            held "ArrowRight"

        steer =
            (if keyRight then
                1.0

             else
                0.0
            )
                - (if keyLeft then
                    1.0

                   else
                    0.0
                  )

        brake =
            if held "ArrowDown" || held "Down" then
                1.0

            else
                0.0

        aeroTuck =
            if held "ArrowUp" then
                1.0

            else
                0.0

        handbrake =
            held "Enter" || held "Shift" || held "ShiftRight"

        leanX =
            steer * 0.85
    in
    { steer = steer
    , brake = brake
    , handbrake = handbrake
    , aeroTuck = aeroTuck
    , leanX = leanX
    }


{-| Hard cap on live debris bricks (racer pop-offs + smashed scenery). The oldest
pieces are dropped first, and all pieces share one GPU brick mesh, so a crash
stays cheap on phones.
-}
maxDebrisPieces : Int
maxDebrisPieces =
    72


{-| Integrate tumbling debris bricks: gravity, a few damped bounces on the
terrain, ground friction, and expiry after their short lifetime.
-}
stepDebrisParticles : Float -> GeneratedTrack -> List DebrisPiece -> List DebrisPiece
stepDebrisParticles dt track pieces =
    pieces
        |> List.filterMap
            (\p ->
                let
                    nextAge =
                        p.age + dt
                in
                if nextAge >= p.lifetime then
                    Nothing

                else
                    let
                        fallVel =
                            Vec3.add p.velocity (vec3 0 (-9.81 * dt) 0)

                        movedPos =
                            Vec3.add p.position (Vec3.scale dt fallVel)

                        halfHeight =
                            Vec3.getY p.size * 0.5

                        groundY =
                            if p.groundS < 0.0 then
                                -1.0e9

                            else
                                Track.groundHeightAt track p.groundS (Vec3.getX movedPos) (Vec3.getZ movedPos) + halfHeight
                    in
                    if Vec3.getY movedPos <= groundY then
                        let
                            vy =
                                Vec3.getY fallVel

                            bounceVy =
                                if vy < -1.2 then
                                    -vy * 0.32

                                else
                                    0.0
                        in
                        Just
                            { p
                                | position = vec3 (Vec3.getX movedPos) groundY (Vec3.getZ movedPos)
                                , velocity = vec3 (Vec3.getX fallVel * 0.8) bounceVy (Vec3.getZ fallVel * 0.8)
                                , rotation = p.rotation + p.spinRate * dt
                                , spinRate = p.spinRate * 0.75
                                , age = nextAge
                            }

                    else
                        Just
                            { p
                                | position = movedPos
                                , velocity = fallVel
                                , rotation = p.rotation + p.spinRate * dt
                                , age = nextAge
                            }
            )


type alias CarCollisionRes =
    { p1 : PhysicsState
    , p2 : PhysicsState
    , impactSpeed : Float
    , collided : Bool
    }


{-| Resolve elastic collision and mutual damage between two racing cars.
-}
resolveCarsCollision :
    PhysicsState
    -> PhysicsState
    -> CarCollisionRes
resolveCarsCollision p1 p2 =
    let
        pos1 =
            p1.position

        pos2 =
            p2.position

        delta =
            Vec3.sub pos1 pos2

        dist =
            Vec3.length delta

        minDist =
            1.25
    in
    if dist < minDist then
        let
            distSafe =
                if dist > 0.001 then
                    dist

                else
                    0.001

            normal =
                Vec3.scale (1.0 / distSafe) delta

            overlap =
                minDist - distSafe

            pos1Corr =
                Vec3.add pos1 (Vec3.scale (overlap * 0.5) normal)

            pos2Corr =
                Vec3.sub pos2 (Vec3.scale (overlap * 0.5) normal)

            relVel =
                Vec3.sub p1.velocity p2.velocity

            velAlongNormal =
                Vec3.dot relVel normal
        in
        if velAlongNormal < 0.0 then
            let
                restitution =
                    0.5

                impulse =
                    -(1.0 + restitution) * velAlongNormal * 0.5

                newVel1 =
                    Vec3.add p1.velocity (Vec3.scale impulse normal)

                newVel2 =
                    Vec3.sub p2.velocity (Vec3.scale impulse normal)

                impactSpeed =
                    abs velAlongNormal

                damage =
                    carBumpDamage impactSpeed

                nextInteg1 =
                    max 0.0 (p1.integrity - damage)

                nextInteg2 =
                    max 0.0 (p2.integrity - damage)
            in
            { p1 = { p1 | position = pos1Corr, velocity = newVel1, integrity = nextInteg1 }
            , p2 = { p2 | position = pos2Corr, velocity = newVel2, integrity = nextInteg2 }
            , impactSpeed = impactSpeed
            , collided = True
            }

        else
            { p1 = { p1 | position = pos1Corr }, p2 = { p2 | position = pos2Corr }, impactSpeed = 0.0, collided = False }

    else
        { p1 = p1, p2 = p2, impactSpeed = 0.0, collided = False }


{-| Health each car loses in a bump at `impactSpeed` m/s: none up to 1.8 m/s.
-}
carBumpDamage : Float -> Float
carBumpDamage impactSpeed =
    if impactSpeed > 1.8 then
        clamp 6.0 48.0 ((impactSpeed - 1.8) * 5.0)

    else
        0.0


confettiPalette : List { r : Float, g : Float, b : Float }
confettiPalette =
    [ { r = 0.98, g = 0.78, b = 0.04 }
    , { r = 0.92, g = 0.23, b = 0.23 }
    , { r = 0.22, g = 0.74, b = 0.97 }
    , { r = 0.93, g = 0.28, b = 0.6 }
    , { r = 0.22, g = 0.78, b = 0.44 }
    , { r = 0.98, g = 0.55, b = 0.15 }
    , { r = 0.95, g = 0.95, b = 0.98 }
    ]


{-| Spawn celebratory confetti brick particles flying up and showering down over the winner.
-}
spawnConfettiBurst : Int -> Vec3 -> Float -> ( List DebrisPiece, Int )
spawnConfettiBurst seed origin groundS =
    let
        pal =
            confettiPalette

        genPiece =
            Random.map5
                (\vx vz vy spin colIdx ->
                    let
                        col =
                            pal
                                |> List.drop (modBy (List.length pal) colIdx)
                                |> List.head
                                |> Maybe.withDefault { r = 0.98, g = 0.78, b = 0.04 }
                    in
                    { position = Vec3.add origin (vec3 (vx * 0.3) 0.5 (vz * 0.3))
                    , velocity = vec3 vx vy vz
                    , rotation = vx * 0.8
                    , spinRate = spin
                    , size = vec3 0.12 0.04 0.08
                    , color = vec3 col.r col.g col.b
                    , age = 0.0
                    , lifetime = 4.5
                    , groundS = groundS
                    }
                )
                (Random.float -7.5 7.5)
                (Random.float -7.5 7.5)
                (Random.float 6.0 14.0)
                (Random.float -16.0 16.0)
                (Random.int 0 99)

        genList =
            Random.list 28 genPiece
    in
    Random.step genList (Random.initialSeed seed)
        |> Tuple.mapSecond (\s -> Tuple.first (Random.step (Random.int 1 999999) s))


spawnDebrisBurst : Int -> Vec3 -> Float -> List { r : Float, g : Float, b : Float } -> ( List DebrisPiece, Int )
spawnDebrisBurst seed origin groundS palette =
    let
        palArray =
            if List.isEmpty palette then
                [ { r = 0.85, g = 0.16, b = 0.16 }
                , { r = 0.95, g = 0.78, b = 0.12 }
                , { r = 0.15, g = 0.42, b = 0.82 }
                ]

            else
                palette

        genPiece =
            Random.map5
                (\vx vz vy spin colIdx ->
                    let
                        col =
                            palArray
                                |> List.drop (modBy (List.length palArray) colIdx)
                                |> List.head
                                |> Maybe.withDefault { r = 0.85, g = 0.16, b = 0.16 }
                    in
                    { position = Vec3.add origin (vec3 0 0.32 0)
                    , velocity = vec3 vx vy vz
                    , rotation = vx * 0.4
                    , spinRate = spin
                    , size = vec3 0.14 0.08 0.1
                    , color = vec3 col.r col.g col.b
                    , age = 0.0
                    , lifetime = 2.4
                    , groundS = groundS
                    }
                )
                (Random.float -4.8 4.8)
                (Random.float -4.8 4.8)
                (Random.float 2.6 6.2)
                (Random.float -11.0 11.0)
                (Random.int 0 99)

        genList =
            Random.list 4 genPiece
    in
    Random.step genList (Random.initialSeed seed)
        |> Tuple.mapSecond (\s -> Tuple.first (Random.step (Random.int 1 999999) s))


{-| Seconds a crash keeps the car out before it is put back at the last
checkpoint. A wreck (out of health) lasts longer: the car has broken into
bricks and the player is told so before it is rebuilt.
-}
crashRecoverySeconds : CrashReason -> Float
crashRecoverySeconds reason =
    case reason of
        StructuralCollapse ->
            4.0

        _ ->
            2.2


{-| Break the whole car apart into loose bricks: pieces the size of 1x2 to
2x4 bricks in the car's own colours, scattered from inside its bounding box
and thrown up and outward.
-}
spawnWreckDebris : Int -> PhysicsState -> VehicleSpec -> List { r : Float, g : Float, b : Float } -> ( List DebrisPiece, Int )
spawnWreckDebris seed physics spec palette =
    let
        palArray =
            if List.isEmpty palette then
                [ { r = 0.85, g = 0.16, b = 0.16 }, { r = 0.95, g = 0.78, b = 0.12 } ]

            else
                palette

        lo =
            Vec3.scale (1.0 / 160.0) spec.boundsMinLdu

        hi =
            Vec3.scale (1.0 / 160.0) spec.boundsMaxLdu

        fwd =
            Physics.forwardVector physics.yaw

        right =
            Physics.rightVector physics.yaw

        carVel =
            Vec3.scale 0.45 physics.velocity

        pieceCount =
            clamp 10 36 spec.placementCount

        brickSizes =
            [ vec3 0.125 0.075 0.25, vec3 0.25 0.075 0.25, vec3 0.25 0.075 0.5, vec3 0.125 0.075 0.125 ]

        genPiece =
            Random.map5
                (\( fx, fy, fz ) ( vx, vz, vy ) spin sizeIdx colIdx ->
                    let
                        localX =
                            Vec3.getX lo + fx * (Vec3.getX hi - Vec3.getX lo)

                        localZ =
                            Vec3.getZ lo + fz * (Vec3.getZ hi - Vec3.getZ lo)

                        height =
                            0.1 + fy * max 0.2 (abs (Vec3.getY hi - Vec3.getY lo))

                        origin =
                            physics.position
                                |> Vec3.add (Vec3.scale localX right)
                                |> Vec3.add (Vec3.scale -localZ fwd)
                                |> Vec3.add (vec3 0 height 0)

                        col =
                            palArray
                                |> List.drop (modBy (List.length palArray) colIdx)
                                |> List.head
                                |> Maybe.withDefault { r = 0.85, g = 0.16, b = 0.16 }
                    in
                    { position = origin
                    , velocity = Vec3.add carVel (vec3 vx vy vz)
                    , rotation = vx * 0.5
                    , spinRate = spin
                    , size =
                        brickSizes
                            |> List.drop (modBy (List.length brickSizes) sizeIdx)
                            |> List.head
                            |> Maybe.withDefault (vec3 0.25 0.075 0.25)
                    , color = vec3 col.r col.g col.b
                    , age = 0.0
                    , lifetime = 4.2
                    , groundS = physics.progressS
                    }
                )
                (Random.map3 (\a b c -> ( a, b, c )) (Random.float 0 1) (Random.float 0 1) (Random.float 0 1))
                (Random.map3 (\a b c -> ( a, b, c )) (Random.float -3.5 3.5) (Random.float -3.5 3.5) (Random.float 1.8 5.5))
                (Random.float -9.0 9.0)
                (Random.int 0 3)
                (Random.int 0 99)
    in
    Random.step (Random.list pieceCount genPiece) (Random.initialSeed seed)
        |> Tuple.mapSecond (\s -> Tuple.first (Random.step (Random.int 1 999999) s))


{-| Shatter one scenery piece into loose bricks thrown along the racer's heading.
-}
spawnSceneryDebris : Int -> TrackObstacle -> Float -> Vec3 -> ( List DebrisPiece, Int )
spawnSceneryDebris seed obs impactSpeed direction =
    let
        style =
            TerrainMesh.sceneryDebrisStyle obs.kind

        colors =
            if List.isEmpty style.colors then
                [ vec3 0.5 0.5 0.5 ]

            else
                style.colors

        throw =
            clamp 2.0 14.0 impactSpeed

        genPiece =
            Random.map5
                (\( ox, oz, oy ) ( fwdMul, side ) up ( spin, life ) colIdx ->
                    let
                        sideDir =
                            vec3 -(Vec3.getZ direction) 0 (Vec3.getX direction)

                        vel =
                            Vec3.scale (throw * fwdMul) direction
                                |> Vec3.add (Vec3.scale side sideDir)
                                |> Vec3.add (vec3 0 up 0)
                    in
                    { position =
                        Vec3.add obs.position
                            (vec3 (ox * obs.radiusM) (oy * obs.heightM + 0.15) (oz * obs.radiusM))
                    , velocity = vel
                    , rotation = ox * 3.0
                    , spinRate = spin
                    , size = Vec3.scale style.scale (vec3 0.5 0.24 0.25)
                    , color =
                        colors
                            |> List.drop (modBy (List.length colors) colIdx)
                            |> List.head
                            |> Maybe.withDefault (vec3 0.5 0.5 0.5)
                    , age = 0.0
                    , lifetime = life
                    , groundS = obs.s
                    }
                )
                (Random.map3 (\a b c -> ( a, b, c )) (Random.float -0.8 0.8) (Random.float -0.8 0.8) (Random.float 0.0 1.0))
                (Random.map2 Tuple.pair (Random.float 0.3 0.85) (Random.float -2.6 2.6))
                (Random.float 2.0 5.5)
                (Random.map2 Tuple.pair (Random.float -12.0 12.0) (Random.float 3.2 4.4))
                (Random.int 0 99)
    in
    Random.step (Random.list style.count genPiece) (Random.initialSeed seed)
        |> Tuple.mapSecond (\s -> Tuple.first (Random.step (Random.int 1 999999) s))


{-| Remove a smashed scenery piece from the race track, rebuild just its chunk's
scenery mesh, scatter its bricks, and give a short camera shake, pop sound and
banner. Ignores pieces that are already gone (e.g. hit by both players). In an
online room the smash is also told to the other pages (`smashEvent`).
-}
smashScenery :
    Language
    -> { obstacleId : Int, impactSpeed : Float, direction : Vec3 }
    -> ( RaceState, List (Cmd Msg) )
    -> ( RaceState, List (Cmd Msg) )
smashScenery lang smash ( race, cmds ) =
    case shatterScenery smash race of
        Nothing ->
            ( race, cmds )

        Just ( shattered, at ) ->
            let
                alert =
                    case shattered.hudAlert of
                        Just existing ->
                            Just existing

                        Nothing ->
                            Just { title = RI18n.smashBanner lang, subtitle = Nothing, positive = True, timer = 0.7 }
            in
            ( { shattered
                | shake = min 1.0 (shattered.shake + 0.3 + smash.impactSpeed / 30.0)
                , hudAlert = alert
              }
                |> tellRoom (smashEvent race.elapsedTime at smash)
            , Ports.playSfx { kind = "smash", intensity = clamp 0.3 1.0 (smash.impactSpeed / 14.0) } :: cmds
            )


{-| Take a smashed scenery piece off the race track, rebuild just its chunk's
scenery mesh and scatter its bricks; with where the piece stood. `Nothing` when
the piece is already gone (e.g. hit by both players).
-}
shatterScenery :
    { obstacleId : Int, impactSpeed : Float, direction : Vec3 }
    -> RaceState
    -> Maybe ( RaceState, Vec3 )
shatterScenery smash race =
    List.filter (\o -> o.id == smash.obstacleId) race.track.obstacles
        |> List.head
        |> Maybe.map
            (\obs ->
                let
                    nextTrack =
                        Track.removeObstacles [ obs.id ] race.track

                    ( pieces, nextSeed ) =
                        spawnSceneryDebris race.debrisSeed obs smash.impactSpeed smash.direction
                in
                ( { race
                    | track = nextTrack
                    , bundle = TerrainMesh.rebuildChunkScenery nextTrack obs.s race.bundle
                    , debris = List.take maxDebrisPieces (pieces ++ race.debris)
                    , debrisSeed = nextSeed
                  }
                , obs.position
                )
            )


isScenerySmash : PhysicsEvent -> Bool
isScenerySmash ev =
    case ev of
        ScenerySmashed _ ->
            True

        _ ->
            False


{-| Camera shake (0–1) added by a knock of `impulse` (0.1–1, from the physics
events): light taps and wall scrapes barely move the view, a hard hit jolts it.
-}
impactShake : Bool -> Float -> Float
impactShake scrape impulse =
    if scrape then
        max 0.0 (impulse - 0.3) * 0.35

    else
        max 0.0 (impulse - 0.2) * 0.7


{-| Camera shake (0–1) added by a landing of `impulse` (0.1–1): ordinary ramp
hops are free, a drop that costs health gives a short thump.
-}
landingShake : Float -> Float
landingShake impulse =
    max 0.0 (impulse - 0.5) * 0.9


{-| Jitter the camera target by a decaying `shake` amount (0–1) for crash feedback.
-}
applyShake : Float -> Float -> Camera -> Camera
applyShake shake t cam =
    if shake < 0.01 then
        cam

    else
        let
            amp =
                shake * 0.14
        in
        { cam
            | target =
                Vec3.add cam.target
                    (vec3 (amp * sin (t * 53.0)) (amp * 0.6 * sin (t * 71.0)) (amp * cos (t * 61.0)))
        }


applyViewportSize : Int -> Int -> Model -> Model
applyViewportSize w h model =
    let
        nextPage =
            case model.page of
                SetupPage garage ->
                    let
                        canvasH =
                            if w >= 1024 then
                                h

                            else
                                max 280 (round (toFloat h * 0.45))
                    in
                    SetupPage { garage | camera = Camera.setViewportHeight (toFloat canvasH) garage.camera }

                RacePage race ->
                    let
                        isWide =
                            w >= h

                        p1H =
                            if race.player2 /= Nothing && not isWide then
                                h // 2

                            else
                                h

                        p2H =
                            if not isWide then
                                h - (h // 2)

                            else
                                h

                        p1Cam =
                            Camera.setViewportHeight (toFloat p1H) race.camera

                        nextP2 =
                            race.player2
                                |> Maybe.map (\p2 -> { p2 | camera = Camera.setViewportHeight (toFloat p2H) p2.camera })
                    in
                    RacePage { race | camera = p1Cam, player2 = nextP2 }

                other ->
                    other
    in
    { model
        | viewportWidth = w
        , viewportHeight = h
        , page = nextPage
    }


updateActiveCamera : (Camera -> Camera) -> Model -> ( Model, Cmd Msg )
updateActiveCamera fn model =
    case model.page of
        SetupPage garage ->
            ( { model | page = SetupPage { garage | camera = fn garage.camera } }
            , Cmd.none
            )

        RacePage race ->
            if race.cameraMode == OrbitCam then
                ( { model | page = RacePage { race | camera = fn race.camera } }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


raceAudioCmd : Model -> Cmd Msg
raceAudioCmd model =
    case model.page of
        RacePage race ->
            let
                isDriving =
                    case race.subPhase of
                        ActiveDriving ->
                            True

                        _ ->
                            False

                tel =
                    Audio.deriveAudioState isDriving model.audioMuted race.physics race.vehicle
            in
            Ports.updateAudioState tel

        _ ->
            silenceAudioCmd


silenceAudioCmd : Cmd Msg
silenceAudioCmd =
    Ports.updateAudioState
        { active = False
        , windGain = 0.0
        , windHz = 220.0
        , rumbleGain = 0.0
        , rumbleHz = 60.0
        , clatterGain = 0.0
        , scrapeGain = 0.0
        }


activeDocument : Page -> Maybe Document
activeDocument page =
    case page of
        SetupPage g ->
            Just g.document

        RacePage r ->
            Just r.document

        FinishLeaderboardPage r ->
            Just r.document

        BuildPage b ->
            Just (Build.document b)


getGarageDoc : Model -> Document
getGarageDoc model =
    activeDocument model.page
        |> Maybe.withDefault (Document.parse defaultPreset.ldrText)


nonEmptyMaybe : String -> Maybe String
nonEmptyMaybe raw =
    let
        t =
            String.trim raw
    in
    if String.isEmpty t then
        Nothing

    else
        Just t


presetTitle : Language -> PresetBuild -> String
presetTitle lang preset =
    case lang of
        Fi ->
            preset.nameFi

        En ->
            preset.nameEn


emptyTouch : TouchState
emptyTouch =
    { left = False
    , right = False
    , brake = False
    , tuck = False
    , drift = False
    }


{-| Subscribe to runtime ports, animation frames (when active in 3D view), keyboard, and visibility.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animSub =
            case model.page of
                SetupPage _ ->
                    Browser.Events.onAnimationFrameDelta AnimationFrame

                RacePage _ ->
                    Browser.Events.onAnimationFrameDelta AnimationFrame

                _ ->
                    Sub.none

        buildSub =
            case model.page of
                BuildPage build ->
                    Sub.map BuildMsg (Build.subscriptions build)

                _ ->
                    Sub.none
    in
    Sub.batch
        [ animSub
        , buildSub
        , GamePorts.viewportResized ViewportResized
        , GamePorts.fileContentReceived FileContentReceived
        , GamePorts.fileLoadError FileLoadError
        , GamePorts.binaryHashDecoded BinaryHashDecoded
        , GamePorts.hashChanged HashChanged
        , GamePorts.tiltStatus TiltStatusChanged
        , GamePorts.tiltSteer TiltSteerChanged
        , GamePorts.tiltOrientationReceived TiltOrientation
        , GamePorts.multiplayerRoomStateChanged MultiplayerRoomChanged
        , GamePorts.multiplayerError MultiplayerError
        , GamePorts.multiplayerVehicleLoaded MultiplayerVehicleLoaded
        , GamePorts.qrCodeScanned QrCodeScanned
        , GamePorts.qrScannerError QrScannerError
        , Browser.Events.onVisibilityChange VisibilityChanged
        , Browser.Events.onKeyDown (Decode.map KeyDown keyEventDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        ]


keyEventDecoder : Decoder Shortcuts.KeyEvent
keyEventDecoder =
    Decode.map8
        (\key shift ctrl meta alt repeat targetTag targetEditable ->
            { key = key
            , shift = shift
            , ctrl = ctrl
            , meta = meta
            , alt = alt
            , repeat = repeat
            , targetTag = targetTag
            , targetEditable = targetEditable
            , targetRole = ""
            }
        )
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)
        (Decode.field "repeat" Decode.bool)
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])
        (Decode.oneOf [ Decode.at [ "target", "isContentEditable" ] Decode.bool, Decode.succeed False ])


{-| Render the active application screen.
-}
view : Model -> Html Msg
view model =
    case model.page of
        SetupPage garage ->
            let
                stats =
                    reachablePartStats garage.document model.partCache
            in
            Garage.view
                { language = model.language
                , vehicle = garage.vehicle
                , meshes = garage.meshes
                , camera = garage.camera
                , plinthMesh = model.staticMeshes.plinth
                , shadowMesh = model.staticMeshes.shadow
                , wheelPreviewSpin = garage.wheelPreviewSpin
                , partsLoaded = stats.settled
                , partsTotal = stats.total
                , selection = model.selection
                , map = model.map
                , seedText = model.seedText
                , editingSeed = model.editingSeed
                , menuPanel = model.menuPanel
                , hasPreviousMap = model.lastMap /= Nothing && model.lastMap /= Just model.map
                , leaderboard = model.leaderboard
                , viewportWidth = model.viewportWidth
                , viewportHeight = model.viewportHeight
                , raceMode = model.raceMode
                , audioMuted = model.audioMuted
                , roomCode = model.roomCodeInput
                , playerModeTab = model.playerModeTab
                , multiplayer = model.multiplayer
                , shareCopiedNotice = model.shareCopiedNotice
                , baseUrl = model.baseUrl
                , activeSlot = model.activeSlot
                , playerOneTitle = model.playerOneCar |> Maybe.map .title |> Maybe.withDefault garage.title
                , playerTwoTitle = model.playerTwoCar.title
                }
                { onOpenBuilder = OpenBuilder
                , onStartRace = GarageToRace
                , onPickBiome = PickBiome
                , onPickSeason = PickSeason
                , onPickWeather = PickWeather
                , onToggleSteepness = ToggleSteepness
                , onChangeSeed = SeedDraftChanged
                , onStartSeedEdit = StartSeedEdit
                , onCommitSeed = CommitSeed
                , onCancelSeedEdit = CancelSeedEdit
                , onCopySeed = CopySeed
                , onSetMenuPanel = SetMenuPanel
                , onRandomSeed = RandomSeed
                , onUsePreviousMap = UsePreviousMap
                , onSetPlayerModeTab = SetPlayerModeTab
                , onSetRaceMode = SetRaceMode
                , onSelectPlayerSlot = SelectPlayerSlot
                , onToggleAudio = ToggleAudioMute
                , onStartHostMultiplayer = StartHostMultiplayer
                , onJoinMultiplayer = JoinMultiplayer
                , onRoomCodeChanged = RoomCodeChanged
                , onOpenQrScanner = OpenQrScanner
                , onCopyMultiplayerLink = CopyMultiplayerLink
                , onStartMultiplayerCountdown = StartMultiplayerCountdown
                , onLeaveMultiplayer = LeaveMultiplayerRoom
                , onMouseDown = CamMouseDown
                , onMouseMove = CamMouseMove
                , onMouseUp = CamMouseUp
                , onWheel = CamWheel
                }

        RacePage race ->
            let
                ( countdownMaybe, crashMaybe ) =
                    case race.subPhase of
                        CountdownPhase rem _ ->
                            ( Just rem, Nothing )

                        ActiveDriving ->
                            ( Nothing, Nothing )

                        CrashRecoveryPhase rem reason ->
                            ( Nothing, Just { timer = rem, reason = reason } )

                        CelebrationPhase _ ->
                            ( Nothing, Nothing )

                maybeSplit =
                    race.player2
                        |> Maybe.map
                            (\p2 ->
                                let
                                    ( p2CountdownMaybe, p2CrashMaybe ) =
                                        case p2.subPhase of
                                            CountdownPhase rem _ ->
                                                ( Just rem, Nothing )

                                            ActiveDriving ->
                                                ( Nothing, Nothing )

                                            CrashRecoveryPhase rem reason ->
                                                ( Nothing, Just { timer = rem, reason = reason } )

                                            CelebrationPhase _ ->
                                                ( Nothing, Nothing )
                                in
                                { p2Spec = p2.vehicle
                                , p2Meshes = p2.meshes
                                , p2Physics = p2.physics
                                , p2Camera = p2.camera
                                , p2HudData =
                                    { language = model.language
                                    , courseTitle = RI18n.mapTitle model.language race.track.config.map
                                    , map = race.track.config.map
                                    , vehicle = p2.vehicle
                                    , physics = p2.physics
                                    , nextCheckpointM = nextCheckpointMeters race.track.checkpoints p2.physics
                                    , elapsedTime = race.elapsedTime
                                    , countdown = p2CountdownMaybe
                                    , crashRecovery = p2CrashMaybe
                                    , audioMuted = model.audioMuted
                                    , splitDelta = p2.splitDelta
                                    , splitFlash = Nothing
                                    , hudAlert = p2.hudAlert
                                    , showTouchControls = False
                                    , tiltEnabled = False
                                    , tiltAvailable = False
                                    , touchLeftHeld = False
                                    , touchRightHeld = False
                                    , touchBrakeHeld = False
                                    , touchTuckHeld = False
                                    , touchDriftHeld = False
                                    }
                                }
                            )

                raceView =
                    Race.view
                        race.bundle
                        race.track
                        race.vehicle
                        race.meshes
                        model.props
                        model.staticMeshes.shadow
                        race.physics
                        race.debris
                        race.camera
                        model.viewportWidth
                        model.viewportHeight
                        { language = model.language
                        , courseTitle = RI18n.mapTitle model.language race.track.config.map
                        , map = race.track.config.map
                        , vehicle = race.vehicle
                        , physics = race.physics
                        , nextCheckpointM = nextCheckpointMeters race.track.checkpoints race.physics
                        , elapsedTime = race.elapsedTime
                        , countdown = countdownMaybe
                        , crashRecovery = crashMaybe
                        , audioMuted = model.audioMuted
                        , splitDelta = race.splitDelta
                        , splitFlash = race.splitFlash |> Maybe.map .time
                        , hudAlert = race.hudAlert
                        , showTouchControls = model.showTouchControls
                        , tiltEnabled = model.tilt.enabled
                        , tiltAvailable = model.tilt.available
                        , touchLeftHeld = model.touch.left
                        , touchRightHeld = model.touch.right
                        , touchBrakeHeld = model.touch.brake
                        , touchTuckHeld = model.touch.tuck
                        , touchDriftHeld = model.touch.drift
                        }
                        { onGoMenu = BackToMenu
                        , onRespawnCheckpoint = RespawnCheckpoint
                        , onToggleAudio = ToggleAudioMute
                        , onTouchSteerLeft = TouchSteerLeft
                        , onTouchSteerRight = TouchSteerRight
                        , onTouchBrake = TouchBrake
                        , onTouchTuck = TouchTuck
                        , onTouchDrift = TouchDrift
                        }
                        maybeSplit
                        race.multiplayer
                        { onStartCountdown = StartMultiplayerCountdown
                        , onCopyShareLink = CopyMultiplayerLink
                        , remoteCar = remoteCarLook model
                        }
            in
            case race.subPhase of
                CelebrationPhase celeb ->
                    div [ Attr.style "position" "relative", Attr.style "width" "100%", Attr.style "height" "100%", Attr.style "overflow" "hidden" ]
                        [ raceView
                        , viewCelebrationOverlay model.language celeb
                        ]

                _ ->
                    raceView

        BuildPage build ->
            Html.map BuildMsg (Build.view build)

        FinishLeaderboardPage res ->
            Results.view
                { language = model.language
                , map = res.map
                , vehicle = res.vehicle
                , finishTimeS = res.finishTimeS
                , topSpeedKmh = res.topSpeedKmh
                , finalIntegrityPct = res.finalIntegrityPct
                , splitTimes = res.splitTimes
                , isNewBest = res.isNewBest
                , leaderboard = model.leaderboard
                , shareCopiedNotice = model.shareCopiedNotice
                }
                { onRaceAgain = RestartRace
                , onNewMap = NewMap
                , onCopyShareLink = CopyShareLink
                , onInspectGarage = InspectInGarage
                , onBackToMenu = BackToMenu
                }


{-| Open the car editor on a build optionally standing on `base`.
-}
openBuilder : Maybe Editor.Base -> Document -> Model -> ( Model, Cmd Msg )
openBuilder maybeBase doc model =
    let
        ( build, buildCmd ) =
            Build.init
                { ldrawBase = model.ldrawBase
                , ldrawFallbackBase = model.ldrawFallbackBase
                , supportsPointerEvents = model.supportsPointerEvents
                , language = model.language
                , width = model.viewportWidth
                , height = model.viewportHeight
                }
                maybeBase
                doc
                |> Tuple.mapFirst (Build.setPartCacheAndIndex model.partCache model.partIndex)

        hashCmd =
            GamePorts.setUrlHash (Ingest.encodeGarageHash model.map doc)
    in
    ( { model
        | page = BuildPage build
        , pressedKeys = Set.empty
        , touch = emptyTouch
        , shareCopiedNotice = False
        , pasteError = Nothing
      }
    , Cmd.batch [ Cmd.map BuildMsg buildCmd, silenceAudioCmd, hashCmd ]
    )


{-| The name of a car built in the editor: the base it stands on.
-}
builtCarTitle : Language -> Maybe Editor.Base -> String
builtCarTitle lang maybeBase =
    case maybeBase of
        Just base ->
            case lang of
                Fi ->
                    "Oma auto: " ++ base.nameFi

                En ->
                    "My " ++ base.nameEn

        Nothing ->
            case lang of
                Fi ->
                    "Oma mäkiauto"

                En ->
                    "Custom Soapbox"


enterGarageFromRaceOrResults : Document -> String -> VehicleSpec -> CarMeshBundle -> Model -> ( Model, Cmd Msg )
enterGarageFromRaceOrResults doc title vehicle meshes model =
    let
        garage =
            { document = doc
            , title = title
            , vehicle = vehicle
            , meshes = meshes
            , camera = RacerCamera.initGarageCamera model.viewportHeight vehicle
            , wheelPreviewSpin = 0.0
            }
    in
    ( { model
        | page = SetupPage garage
        , pressedKeys = Set.empty
        , touch = emptyTouch
        , shareCopiedNotice = False
      }
    , silenceAudioCmd
    )


{-| Celebration overlay presented on top of the 3D race viewport when a car crosses the finish line.
-}
viewCelebrationOverlay :
    Language
    -> { timer : Float, winner : String, confettiSpawned : Bool, results : ResultsState }
    -> Html Msg
viewCelebrationOverlay lang celeb =
    let
        timeFormatted =
            Leaderboard.formatRaceTime celeb.results.finishTimeS

        speedFormatted =
            String.fromInt (round celeb.results.topSpeedKmh) ++ " km/h"

        integFormatted =
            String.fromInt celeb.results.finalIntegrityPct ++ "%"
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "inset" "0"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "16px"
        , Attr.style "background" "radial-gradient(ellipse at center, rgba(5, 19, 29, 0.45) 0%, rgba(5, 19, 29, 0.85) 100%)"
        , Attr.style "z-index" "50"
        , Attr.style "pointer-events" "auto"
        , Attr.style "padding" "24px"
        , Attr.style "box-sizing" "border-box"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "6px"
            , Attr.style "text-align" "center"
            ]
            [ div [ Attr.style "font-size" "48px", Attr.style "filter" "drop-shadow(0 4px 12px rgba(250, 200, 10, 0.6))" ]
                [ text "🏆" ]
            , h1
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "clamp(24px, 5vw, 36px)"
                , Attr.style "font-weight" "900"
                , Attr.style "color" Brand.legoYellow
                , Attr.style "letter-spacing" "1px"
                , Attr.style "text-transform" "uppercase"
                , Attr.style "text-shadow" "0 2px 10px rgba(0, 0, 0, 0.8)"
                ]
                [ text
                    (case lang of
                        Fi ->
                            "Maali! Voittaja:"

                        En ->
                            "Finish! Winner:"
                    )
                ]
            , div
                [ Attr.style "font-size" "clamp(18px, 4vw, 26px)"
                , Attr.style "font-weight" "800"
                , Attr.style "color" Brand.legoWhite
                , Attr.style "text-shadow" "0 2px 8px rgba(0, 0, 0, 0.8)"
                ]
                [ text celeb.winner ]
            , if celeb.results.isNewBest then
                div
                    [ Attr.style "display" "inline-flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "6px"
                    , Attr.style "background" "rgba(250, 200, 10, 0.25)"
                    , Attr.style "border" ("1px solid " ++ Brand.legoYellow)
                    , Attr.style "color" Brand.legoYellow
                    , Attr.style "padding" "4px 14px"
                    , Attr.style "border-radius" "20px"
                    , Attr.style "font-size" "13px"
                    , Attr.style "font-weight" "800"
                    , Attr.style "margin-top" "4px"
                    ]
                    [ text
                        (case lang of
                            Fi ->
                                "⭐ Uusi ennätys!"

                            En ->
                                "⭐ New Record!"
                        )
                    ]

              else
                text ""
            ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "gap" "12px"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "justify-content" "center"
            , Attr.style "max-width" "520px"
            , Attr.style "width" "100%"
            ]
            [ celebStatCard "⏱️"
                (case lang of
                    Fi ->
                        "Aika"

                    En ->
                        "Time"
                )
                timeFormatted
            , celebStatCard "⚡"
                (case lang of
                    Fi ->
                        "Huippunopeus"

                    En ->
                        "Top Speed"
                )
                speedFormatted
            , celebStatCard "🛠️"
                (case lang of
                    Fi ->
                        "Kunto"

                    En ->
                        "Integrity"
                )
                integFormatted
            ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "gap" "10px"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "justify-content" "center"
            , Attr.style "margin-top" "8px"
            ]
            [ button
                [ Attr.type_ "button"
                , onClick RestartRace
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "background" Brand.legoYellow
                , Attr.style "color" Brand.legoBlack
                , Attr.style "border" "none"
                , Attr.style "padding" "12px 22px"
                , Attr.style "border-radius" "12px"
                , Attr.style "font-weight" "900"
                , Attr.style "font-size" "15px"
                , Attr.style "cursor" "pointer"
                , Attr.style "box-shadow" "0 4px 14px rgba(250, 200, 10, 0.4)"
                ]
                [ text
                    (case lang of
                        Fi ->
                            "🔄 Aja uudelleen"

                        En ->
                            "🔄 Race Again"
                    )
                ]
            , button
                [ Attr.type_ "button"
                , onClick OpenBuilder
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "background" "rgba(255, 255, 255, 0.12)"
                , Attr.style "color" Brand.legoWhite
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.25)"
                , Attr.style "padding" "12px 18px"
                , Attr.style "border-radius" "12px"
                , Attr.style "font-weight" "800"
                , Attr.style "font-size" "14px"
                , Attr.style "cursor" "pointer"
                ]
                [ text
                    (case lang of
                        Fi ->
                            "🔨 Muokkaa autoa"

                        En ->
                            "🔨 Edit Car"
                    )
                ]
            , button
                [ Attr.type_ "button"
                , onClick InspectInGarage
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "background" "rgba(255, 255, 255, 0.12)"
                , Attr.style "color" Brand.legoWhite
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.25)"
                , Attr.style "padding" "12px 18px"
                , Attr.style "border-radius" "12px"
                , Attr.style "font-weight" "800"
                , Attr.style "font-size" "14px"
                , Attr.style "cursor" "pointer"
                ]
                [ text
                    (case lang of
                        Fi ->
                            "🏠 Valikko"

                        En ->
                            "🏠 Menu"
                    )
                ]
            , button
                [ Attr.type_ "button"
                , onClick (ShowLeaderboardPage celeb.results)
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "background" "rgba(255, 255, 255, 0.08)"
                , Attr.style "color" "#cbd5e1"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
                , Attr.style "padding" "12px 16px"
                , Attr.style "border-radius" "12px"
                , Attr.style "font-weight" "700"
                , Attr.style "font-size" "13px"
                , Attr.style "cursor" "pointer"
                ]
                [ text
                    (case lang of
                        Fi ->
                            "📊 Tulostaulukko"

                        En ->
                            "📊 Leaderboard"
                    )
                ]
            ]
        ]


{-| Single statistic badge in the celebration overlay.
-}
celebStatCard : String -> String -> String -> Html msg
celebStatCard icon label val =
    div
        [ Attr.style "flex" "1 1 120px"
        , Attr.style "background" "rgba(15, 23, 42, 0.85)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "12px"
        , Attr.style "padding" "10px"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "2px"
        ]
        [ div [ Attr.style "font-size" "18px" ] [ text icon ]
        , div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "text-transform" "uppercase" ] [ text label ]
        , div [ Attr.style "font-size" "16px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoWhite ] [ text val ]
        ]
