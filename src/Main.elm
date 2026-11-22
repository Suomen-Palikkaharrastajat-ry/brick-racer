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
import Dict
import Game.Multiplayer as Multiplayer exposing (Role(..), RoomState, RoomStatus(..))
import Html exposing (Html)
import Http
import I18n exposing (Language(..))
import Json.Decode as Decode exposing (Decoder)
import LDraw.Catalog as Catalog exposing (PartIndex)
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..), ResolverConfig)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Garage as Garage
import Pages.Menu as Menu
import Pages.Race as Race
import Pages.Results as Results
import Ports
import Racer.Analyze as Analyze exposing (CarMeshBundle)
import Racer.Audio as Audio
import Racer.Camera as RacerCamera
import Racer.HUD as HUD exposing (HudAlert)
import Racer.I18n as RI18n
import Racer.Ingest as Ingest
import Racer.Leaderboard as Leaderboard exposing (Leaderboard)
import Racer.Physics as Physics
import Racer.Render as Render
import Racer.TerrainMesh as TerrainMesh exposing (TrackRenderBundle)
import Racer.TerrainShader as TerrainShader exposing (DebrisPiece, TerrainVertex)
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
        , TrackId(..)
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
    }


{-| Top-level active screen state.
-}
type Page
    = MenuPage
    | LoadGaragePage GarageState
    | RacePage RaceState
    | FinishLeaderboardPage ResultsState


{-| Vehicle inspection state for `LoadGaragePage`.
-}
type alias GarageState =
    { document : Document
    , title : String
    , vehicle : VehicleSpec
    , meshes : CarMeshBundle
    , camera : Camera
    , wheelPreviewSpin : Float
    }


{-| Subphase inside `RacePage` preserved cleanly across Pause toggles.
-}
type ActiveRaceSubphase
    = CountdownPhase Float Int
    | ActiveDriving
    | CrashRecoveryPhase Float CrashReason


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
    , isPaused : Bool
    , elapsedTime : Float
    , topSpeedKmh : Float
    , splitTimes : List Float
    , splitDelta : Maybe Float
    , hudAlert : Maybe HudAlert
    , cameraMode : CameraMode
    , camera : Camera
    , debris : List DebrisPiece
    , debrisSeed : Int
    , shake : Float
    , player2 : Maybe PlayerTwoState
    , multiplayer : Maybe RoomState
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
    , trackId : TrackId
    , seed : Int
    , finishTimeS : Float
    , topSpeedKmh : Float
    , finalIntegrityPct : Int
    , splitTimes : List Float
    , isNewBest : Bool
    }


{-| Cached track geometry and spatial chunk bundle keyed by track and seed.
-}
type alias CachedTrack =
    { trackId : TrackId
    , seed : Int
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


{-| Device orientation tilt-steering calibration and live angle.
-}
type alias TiltState =
    { enabled : Bool
    , available : Bool
    , zeroGamma : Maybe Float
    , gamma : Float
    }


{-| Static shared WebGL utility meshes allocated once at startup.
-}
type alias StaticMeshes =
    { plinth : WebGL.Mesh TerrainVertex
    , cube : WebGL.Mesh TerrainVertex
    , shadow : WebGL.Mesh TerrainVertex
    }


{-| Full Brick Racer TEA application model.
-}
type alias Model =
    { resolverConfig : ResolverConfig
    , ldrawBase : String
    , indicesBase : String
    , partCache : PartCache
    , partIndex : PartIndex
    , partIndexLoaded : Bool
    , staticMeshes : StaticMeshes
    , trackId : TrackId
    , seed : Int
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
    , shareCopiedNotice : Bool
    , raceMode : RaceMode
    , multiplayer : Maybe RoomState
    , page : Page
    }


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
    | StartHostMultiplayer
    | JoinMultiplayer
    | MultiplayerRoomChanged Decode.Value
    | MultiplayerError String
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
    | SelectTrack TrackId
    | ChangeSeed String
    | RandomSeed
    | BackToMenu
    | CopyShareLink
    | OpenQrScanner
    | QrCodeScanned String
    | QrScannerError String
    | GarageToRace
    | InspectInGarage
    | RestartRace
    | CycleCamera
    | TogglePause
    | ResumeRace
    | RespawnCheckpoint
    | ToggleTouchUi
    | RequestTilt
    | TiltOrientationReceived { available : Bool, gamma : Float, beta : Float }
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
            Resolve.resolverConfig flags.ldrawBase (nonEmptyMaybe flags.ldrawFallbackBase)

        parsedInit =
            Ingest.parseHash flags.initialHash

        initTrackId =
            Maybe.withDefault FarmLane parsedInit.trackId

        initSeed =
            Maybe.withDefault 1 parsedInit.seed

        baseModel =
            { resolverConfig = resolverCfg
            , ldrawBase = flags.ldrawBase
            , indicesBase = flags.indicesBase
            , partCache = Resolve.initCache
            , partIndex = Catalog.emptyPartIndex
            , partIndexLoaded = False
            , staticMeshes =
                { plinth = TerrainMesh.buildGaragePlinthMesh
                , cube = TerrainShader.buildUnitCubeMesh
                , shadow = TerrainShader.buildShadowQuadMesh
                }
            , trackId = initTrackId
            , seed = max 1 initSeed
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
                , zeroGamma = Nothing
                , gamma = 0.0
                }
            , shareCopiedNotice = False
            , raceMode = SinglePlayer
            , multiplayer = Nothing
            , page = MenuPage
            }

        startupCmds =
            [ Task.perform GotDomViewport Dom.getViewport
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
    if bytesCandidate /= Nothing || not (String.isEmpty (String.replace "#" "" trimmedHash)) && (parsedInit.base64Payload /= Nothing || parsedInit.hasModelParam) then
        case Ingest.decodeRaceInput bytesCandidate trimmedHash of
            Ok decoded ->
                let
                    ( nextModel, enterCmd ) =
                        loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.trackId decoded.seed False baseModel
                in
                ( nextModel, Cmd.batch (enterCmd :: startupCmds) )

            Err _ ->
                ( baseModel, Cmd.batch startupCmds )

    else
        ( baseModel, Cmd.batch startupCmds )


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
            ( applyViewportSize (max 320 width) (max 240 height) model, Cmd.none )

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
            , Ports.storeLanguage code
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
            , Cmd.batch [ Ports.setAudioMuted nextMuted, audioCmd ]
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

        StartHostMultiplayer ->
            let
                vHash =
                    case activeDocument model.page of
                        Just doc ->
                            Ingest.encodeRaceHash model.trackId model.seed doc

                        Nothing ->
                            ""
            in
            ( model
            , Ports.createMultiplayerRoom
                { roomId = ""
                , courseId = Racer.Types.trackIdToString model.trackId
                , name = "Host Commentator"
                , vehicleHash = vHash
                }
            )

        JoinMultiplayer ->
            let
                vHash =
                    case activeDocument model.page of
                        Just doc ->
                            Ingest.encodeRaceHash model.trackId model.seed doc

                        Nothing ->
                            ""
            in
            ( model
            , Ports.joinMultiplayerRoom
                { roomId = ""
                , name = "Racer"
                , vehicleHash = vHash
                }
            )

        StartMultiplayerCountdown ->
            ( model, Ports.startMultiplayerCountdown () )

        CopyMultiplayerLink ->
            let
                roomArg =
                    case model.multiplayer of
                        Just r ->
                            "#room=" ++ r.roomId

                        Nothing ->
                            ""
            in
            ( model, Ports.copyShareUrl roomArg )

        MultiplayerRoomChanged rawJson ->
            case Decode.decodeValue Multiplayer.decodeRoomState rawJson of
                Ok room ->
                    case model.page of
                        RacePage race ->
                            let
                                updatedRace =
                                    { race | multiplayer = Just room }
                            in
                            ( { model | page = RacePage updatedRace, multiplayer = Just room }, Cmd.none )

                        LoadGaragePage garage ->
                            let
                                ( nextModel, enterCmd ) =
                                    startRaceFromDoc garage.title garage.document garage.vehicle garage.meshes { model | multiplayer = Just room }
                            in
                            ( nextModel
                            , Cmd.batch
                                [ enterCmd
                                , Ports.setUrlHash ("#room=" ++ room.roomId)
                                ]
                            )

                        _ ->
                            ( { model | multiplayer = Just room }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        MultiplayerError _ ->
            ( model, Cmd.none )

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
                        loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.trackId decoded.seed True model

                    Err Ingest.InvalidBinaryPayload ->
                        ( { model | pendingBinaryPaste = Just trimmed }
                        , Ports.decodeBinaryHash trimmed
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
                            loadDocumentIntoGarage decoded.suggestedName
                                decoded.document
                                decoded.trackId
                                decoded.seed
                                True
                                { model | pendingBinaryPaste = Nothing, pasteError = Nothing }

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
            case Ingest.decodeRaceInput Nothing rawLdr of
                Ok decoded ->
                    loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.trackId decoded.seed True model

                Err _ ->
                    ( { model | pasteError = Just (RI18n.pasteErrorFallback model.language) }
                    , Cmd.none
                    )

        FileLoadError errText ->
            ( { model | page = MenuPage, pasteError = Just errText }
            , silenceAudioCmd
            )

        HashChanged rawHash ->
            let
                trimmed =
                    String.trim rawHash
            in
            if String.isEmpty trimmed || trimmed == "#" then
                ( model, Cmd.none )

            else
                case Ingest.decodeRaceInput Nothing trimmed of
                    Ok decoded ->
                        loadDocumentIntoGarage decoded.suggestedName decoded.document decoded.trackId decoded.seed False model

                    Err _ ->
                        ( model, Cmd.none )

        RequestFileUpload ->
            ( model, Ports.requestFileUpload () )

        SelectPreset preset ->
            let
                doc =
                    Document.parse preset.ldrText

                title =
                    presetTitle model.language preset
            in
            loadDocumentIntoGarage title doc Nothing Nothing True model

        SelectTrack nextTrackId ->
            let
                updatedModel =
                    { model
                        | trackId = nextTrackId
                        , shareCopiedNotice = False
                    }
            in
            case updatedModel.page of
                LoadGaragePage garage ->
                    ( updatedModel
                    , Ports.setUrlHash (Ingest.encodeRaceHash updatedModel.trackId updatedModel.seed garage.document)
                    )

                _ ->
                    ( updatedModel, Cmd.none )

        ChangeSeed rawStr ->
            let
                nextSeed =
                    String.toInt rawStr
                        |> Maybe.map (clamp 1 999999)
                        |> Maybe.withDefault model.seed
            in
            ( { model | seed = nextSeed, shareCopiedNotice = False }, Cmd.none )

        RandomSeed ->
            let
                nextSeed =
                    modBy 9999 (model.seed * 1664525 + 1013904223) + 1
            in
            ( { model | seed = abs nextSeed, shareCopiedNotice = False }, Cmd.none )

        BackToMenu ->
            ( { model
                | page = MenuPage
                , pressedKeys = Set.empty
                , touch = emptyTouch
                , shareCopiedNotice = False
                , multiplayer = Nothing
              }
            , Cmd.batch [ silenceAudioCmd, Ports.leaveMultiplayerRoom () ]
            )

        CopyShareLink ->
            case activeDocument model.page of
                Nothing ->
                    ( model, Cmd.none )

                Just doc ->
                    let
                        hashStr =
                            Ingest.encodeRaceHash model.trackId model.seed doc
                    in
                    ( { model | shareCopiedNotice = True }
                    , Cmd.batch [ Ports.setUrlHash hashStr, Ports.copyShareUrl hashStr ]
                    )

        OpenQrScanner ->
            ( model, Ports.openQrScanner () )

        QrCodeScanned scannedHash ->
            ( { model | pendingBinaryPaste = Just scannedHash }
            , Ports.decodeBinaryHash scannedHash
            )

        QrScannerError _ ->
            ( model, Cmd.none )

        GarageToRace ->
            case model.page of
                LoadGaragePage garage ->
                    if garage.vehicle.isDrivable then
                        startRaceFromDoc garage.title garage.document garage.vehicle garage.meshes model

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
                    ( nextModel, Cmd.batch [ cmd, Ports.leaveMultiplayerRoom () ] )

                FinishLeaderboardPage res ->
                    let
                        ( nextModel, cmd ) =
                            enterGarageFromRaceOrResults res.document res.title res.vehicle res.meshes cleanModel
                    in
                    ( nextModel, Cmd.batch [ cmd, Ports.leaveMultiplayerRoom () ] )

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

        CycleCamera ->
            case model.page of
                RacePage race ->
                    let
                        nextMode =
                            RacerCamera.cycleMode race.cameraMode

                        cam0 =
                            race.camera

                        nextCam =
                            case nextMode of
                                OrbitCam ->
                                    { cam0
                                        | elevation = 0.34
                                        , distance = 5.2
                                    }

                                _ ->
                                    cam0
                    in
                    ( { model
                        | page =
                            RacePage
                                { race
                                    | cameraMode = nextMode
                                    , camera = nextCam
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TogglePause ->
            case model.page of
                RacePage race ->
                    let
                        nextModel =
                            { model
                                | page = RacePage { race | isPaused = not race.isPaused }
                                , pressedKeys = Set.empty
                                , touch = emptyTouch
                            }
                    in
                    ( nextModel, raceAudioCmd nextModel )

                _ ->
                    ( model, Cmd.none )

        ResumeRace ->
            case model.page of
                RacePage race ->
                    let
                        nextModel =
                            { model | page = RacePage { race | isPaused = False } }
                    in
                    ( nextModel, raceAudioCmd nextModel )

                _ ->
                    ( model, Cmd.none )

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
                                , isPaused = False
                                , camera = nextCam
                            }

                        nextModel =
                            { model | page = RacePage nextRace }
                    in
                    ( nextModel, raceAudioCmd nextModel )

                _ ->
                    ( model, Cmd.none )

        ToggleTouchUi ->
            ( { model | showTouchControls = not model.showTouchControls }, Cmd.none )

        RequestTilt ->
            if model.tilt.enabled then
                let
                    oldTilt =
                        model.tilt
                in
                ( { model | tilt = { oldTilt | enabled = False, zeroGamma = Nothing } }, Cmd.none )

            else
                let
                    oldTilt =
                        model.tilt
                in
                ( { model | tilt = { oldTilt | enabled = True, zeroGamma = Nothing } }
                , Ports.requestTiltPermission ()
                )

        TiltOrientationReceived orient ->
            let
                oldTilt =
                    model.tilt

                zeroRef =
                    if orient.available then
                        Just 0.0

                    else
                        Nothing
            in
            ( { model
                | tilt =
                    { oldTilt
                        | available = orient.available
                        , zeroGamma = zeroRef
                        , gamma = orient.gamma
                    }
              }
            , Cmd.none
            )

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
                ( Browser.Events.Hidden, RacePage race ) ->
                    if not race.isPaused then
                        let
                            nextModel =
                                { model
                                    | page = RacePage { race | isPaused = True }
                                    , pressedKeys = Set.empty
                                    , touch = emptyTouch
                                }
                        in
                        ( nextModel, silenceAudioCmd )

                    else
                        ( model, Cmd.none )

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
            RacePage race ->
                if not keyEvent.repeat && (norm == "Escape" || norm == "p") then
                    let
                        nextModel =
                            { model
                                | page = RacePage { race | isPaused = not race.isPaused }
                                , pressedKeys = Set.empty
                                , touch = emptyTouch
                            }
                    in
                    ( nextModel, raceAudioCmd nextModel )

                else if not keyEvent.repeat && norm == "c" && not race.isPaused then
                    update CycleCamera model

                else if not keyEvent.repeat && norm == "r" && not race.isPaused then
                    update RespawnCheckpoint model

                else
                    ( { model | pressedKeys = Set.insert norm model.pressedKeys }, Cmd.none )

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
        LoadGaragePage garage ->
            let
                nextCam =
                    RacerCamera.updateGarageCamera dt garage.camera

                nextSpin =
                    garage.wheelPreviewSpin + dt * 2.8
            in
            ( { model
                | page =
                    LoadGaragePage
                        { garage
                            | camera = nextCam
                            , wheelPreviewSpin = nextSpin
                        }
              }
            , Cmd.none
            )

        RacePage race ->
            if race.isPaused then
                ( model, Cmd.none )

            else
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
                                Ports.sendMultiplayerTelemetry
                                    { pos = [ Vec3.getX p.position, Vec3.getY p.position, Vec3.getZ p.position ]
                                    , rot = [ p.pitch, p.yaw, p.roll ]
                                    , speed = spd
                                    , progress = prog
                                    , finished = isFin
                                    }

                            Nothing ->
                                Cmd.none
                in
                ( nextModel, Cmd.batch [ raceCmd, telemetryCmd ] )

        _ ->
            ( model, Cmd.none )


stepRaceFrame : Float -> RaceState -> Model -> ( Model, Cmd Msg )
stepRaceFrame dt race model =
    let
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
                                                    ( CrashRecoveryPhase 2.2 r
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

                baseRace =
                    { race
                        | physics = nextPhysics
                        , accumulator = nextAccum
                        , elapsedTime = nextElapsed
                        , topSpeedKmh = nextTopSpeed
                        , hudAlert = nextAlert
                        , debris = steppedDebris
                        , shake = decayedShake
                        , camera = applyShake decayedShake nextElapsed nextCam
                        , player2 = nextP2
                    }
            in
            processPhysicsEvents (events ++ p2Smashes) p2Sfx baseRace model


processPhysicsEvents : List PhysicsEvent -> List (Cmd Msg) -> RaceState -> Model -> ( Model, Cmd Msg )
processPhysicsEvents events extraCmds race model =
    if List.member FinishedRace events then
        let
            finalSplits =
                appendFinishSplit events race.elapsedTime race.splitTimes

            entry =
                { trackId = race.track.config.id
                , vehicleName = race.vehicle.name
                , timeSeconds = race.elapsedTime
                , topSpeedKmh = race.topSpeedKmh
                , wheelCount = List.length race.vehicle.wheels
                , steeringLabel = RI18n.steeringMechName model.language race.vehicle.steering
                , seed = race.track.config.seed
                }

            ( nextLb, isNewBest ) =
                Leaderboard.recordRun entry model.leaderboard

            results =
                { document = race.document
                , title = race.title
                , vehicle = race.vehicle
                , meshes = race.meshes
                , trackId = race.track.config.id
                , seed = race.track.config.seed
                , finishTimeS = race.elapsedTime
                , topSpeedKmh = race.topSpeedKmh
                , finalIntegrityPct = clamp 0 100 (round race.physics.integrity)
                , splitTimes = finalSplits
                , isNewBest = isNewBest
                }
        in
        ( { model
            | leaderboard = nextLb
            , page = FinishLeaderboardPage results
            , pressedKeys = Set.empty
            , touch = emptyTouch
          }
        , Cmd.batch
            ([ Ports.playSfx { kind = "finish", intensity = 1.0 }
             , silenceAudioCmd
             , Ports.saveLeaderboard (Leaderboard.encodeLeaderboard nextLb)
             ]
                ++ extraCmds
            )
        )

    else
        let
            stepEv ev ( rAcc, cmdAcc ) =
                case ev of
                    Impact { impulse, scrape } ->
                        if not scrape && impulse > 1.2 then
                            ( rAcc
                            , Ports.playSfx { kind = "impact", intensity = clamp 0.15 1.0 (impulse / 18.0) } :: cmdAcc
                            )

                        else
                            ( rAcc, cmdAcc )

                    Landing { impulse } ->
                        if impulse > 1.0 then
                            ( rAcc
                            , Ports.playSfx { kind = "landing", intensity = clamp 0.15 1.0 (impulse / 16.0) } :: cmdAcc
                            )

                        else
                            ( rAcc, cmdAcc )

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
                                Leaderboard.bestForTrack rAcc.track.config.id model.leaderboard
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
                            , hudAlert = Just alert
                          }
                        , Ports.playSfx { kind = "checkpoint", intensity = 0.9 } :: cmdAcc
                        )

                    Crashed reason ->
                        ( { rAcc | subPhase = CrashRecoveryPhase 2.2 reason }
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


loadDocumentIntoGarage : String -> Document -> Maybe TrackId -> Maybe Int -> Bool -> Model -> ( Model, Cmd Msg )
loadDocumentIntoGarage suggestedTitle doc maybeTrack maybeSeed syncHash model =
    let
        nextTrackId =
            Maybe.withDefault model.trackId maybeTrack

        nextSeed =
            Maybe.withDefault model.seed (Maybe.map (clamp 1 999999) maybeSeed)

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
                Ports.setUrlHash (Ingest.encodeRaceHash nextTrackId nextSeed doc)

            else
                Cmd.none

        fetchCmd =
            fetchPartsCmd model.resolverConfig stats.needed
    in
    ( { model
        | trackId = nextTrackId
        , seed = nextSeed
        , partCache = markedCache
        , pasteError = Nothing
        , shareCopiedNotice = False
        , pressedKeys = Set.empty
        , touch = emptyTouch
        , page = LoadGaragePage garage
      }
    , Cmd.batch [ fetchCmd, hashCmd, silenceAudioCmd ]
    )


startRaceFromDoc : String -> Document -> VehicleSpec -> CarMeshBundle -> Model -> ( Model, Cmd Msg )
startRaceFromDoc title doc vehicle meshes model =
    let
        ( cachedTrack, modelWithTrack ) =
            ensureTrackBuilt model.trackId model.seed model

        isSplit =
            model.raceMode == SplitScreenTwoPlayer

        physics =
            if isSplit then
                Physics.initPhysicsWithOffset -1.8 cachedTrack.track vehicle

            else
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
                    p2PresetId =
                        if String.contains "turntable" (String.toLower vehicle.name) then
                            "pinewood-classic"

                        else
                            "turntable-kart"

                    p2Preset =
                        case Ingest.findPresetById p2PresetId of
                            Just p ->
                                p

                            Nothing ->
                                case List.head Ingest.presets of
                                    Just p ->
                                        p

                                    Nothing ->
                                        { id = "default", nameEn = "Pinewood Plank", nameFi = "Mäkiauto", summaryEn = "", summaryFi = "", ldrText = "" }

                    p2Doc =
                        Document.parse p2Preset.ldrText

                    p2Analyzed =
                        Analyze.analyzeDocument p2Preset.nameEn p2Doc model.partCache model.partIndex

                    p2Physics =
                        Physics.initPhysicsWithOffset 1.8 cachedTrack.track p2Analyzed.spec

                    p2H =
                        if not isWide then
                            modelWithTrack.viewportHeight - (modelWithTrack.viewportHeight // 2)

                        else
                            modelWithTrack.viewportHeight

                    p2Cam =
                        RacerCamera.initRaceCamera p2H p2Physics p2Analyzed.spec
                in
                Just
                    { vehicle = p2Analyzed.spec
                    , meshes = p2Analyzed.meshes
                    , physics = p2Physics
                    , camera = p2Cam
                    , accumulator = 0.0
                    , subPhase = CountdownPhase 3.0 4
                    , topSpeedKmh = 0.0
                    , splitTimes = []
                    , splitDelta = Nothing
                    , hudAlert = Nothing
                    , debris = []
                    , debrisSeed = max 1 (modelWithTrack.seed * 97 + p2Analyzed.spec.placementCount)
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
            , isPaused = False
            , elapsedTime = 0.0
            , topSpeedKmh = 0.0
            , splitTimes = []
            , splitDelta = Nothing
            , hudAlert = Nothing
            , cameraMode = ChaseCam
            , camera = cam
            , debris = []
            , debrisSeed = max 1 (modelWithTrack.seed * 73 + vehicle.placementCount)
            , shake = 0.0
            , player2 = maybeP2
            , multiplayer = model.multiplayer
            }

        nextModel =
            { modelWithTrack
                | page = RacePage race
                , pressedKeys = Set.empty
                , touch = emptyTouch
                , shareCopiedNotice = False
            }
    in
    ( nextModel
    , Cmd.batch
        [ Ports.playSfx { kind = "countdown", intensity = 0.7 }
        , silenceAudioCmd
        ]
    )


ensureTrackBuilt : TrackId -> Int -> Model -> ( CachedTrack, Model )
ensureTrackBuilt trackId seed model =
    case model.cachedTrack of
        Just cached ->
            if cached.trackId == trackId && cached.seed == seed then
                ( cached, model )

            else
                buildAndStoreTrack trackId seed model

        Nothing ->
            buildAndStoreTrack trackId seed model


buildAndStoreTrack : TrackId -> Int -> Model -> ( CachedTrack, Model )
buildAndStoreTrack trackId seed model =
    let
        cfg =
            Track.trackConfigFor trackId seed

        track =
            Track.generate cfg

        bundle =
            TerrainMesh.buildTrackRenderBundle track

        cached =
            { trackId = trackId
            , seed = seed
            , track = track
            , bundle = bundle
            }
    in
    ( cached, { model | cachedTrack = Just cached } )


reanalyzeActiveVehicle : Model -> Model
reanalyzeActiveVehicle model =
    case model.page of
        LoadGaragePage garage ->
            let
                analyzed =
                    Analyze.analyzeDocument garage.title garage.document model.partCache model.partIndex
            in
            { model
                | page =
                    LoadGaragePage
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

        MenuPage ->
            model


reachablePartStats : Document -> PartCache -> { needed : List String, total : Int, settled : Int, inFlight : Int }
reachablePartStats doc cache =
    let
        subfileDict =
            Document.subfiles doc
                |> Dict.fromList

        seedNames =
            (Document.mainPlacements doc |> List.map .part)
                ++ (Document.subfiles doc |> List.concatMap (\( _, lines ) -> Resolve.pendingParts lines Dict.empty))

        step frontier visited neededAcc totalAcc settledAcc inFlightAcc =
            case frontier of
                [] ->
                    { needed = List.reverse neededAcc
                    , total = totalAcc
                    , settled = settledAcc
                    , inFlight = inFlightAcc
                    }

                rawName :: rest ->
                    let
                        name =
                            String.toLower (String.replace "\\" "/" (String.trim rawName))
                    in
                    if String.isEmpty name || Set.member name visited then
                        step rest visited neededAcc totalAcc settledAcc inFlightAcc

                    else
                        let
                            nextVisited =
                                Set.insert name visited
                        in
                        case Dict.get name subfileDict of
                            Just subLines ->
                                let
                                    subRefs =
                                        Resolve.pendingParts subLines Dict.empty
                                in
                                step (subRefs ++ rest) nextVisited neededAcc totalAcc settledAcc inFlightAcc

                            Nothing ->
                                case Dict.get name cache of
                                    Nothing ->
                                        step rest nextVisited (name :: neededAcc) (totalAcc + 1) settledAcc inFlightAcc

                                    Just Loading ->
                                        step rest nextVisited neededAcc (totalAcc + 1) settledAcc (inFlightAcc + 1)

                                    Just (Failed _) ->
                                        step rest nextVisited neededAcc (totalAcc + 1) (settledAcc + 1) inFlightAcc

                                    Just (Loaded lines) ->
                                        let
                                            subRefs =
                                                Resolve.pendingParts lines Dict.empty
                                        in
                                        step (subRefs ++ rest) nextVisited neededAcc (totalAcc + 1) (settledAcc + 1) inFlightAcc
    in
    step seedNames Set.empty [] 0 0 0


fetchPartsCmd : ResolverConfig -> List String -> Cmd Msg
fetchPartsCmd config names =
    if List.isEmpty names then
        Cmd.none

    else
        Cmd.batch (List.map (\n -> Resolve.fetchPart config n PartFetched) names)


samplePlayerInput : Model -> InputState
samplePlayerInput model =
    sampleP1Input model


sampleP1Input : Model -> InputState
sampleP1Input model =
    let
        held k =
            Set.member k model.pressedKeys

        isSplit =
            model.raceMode == SplitScreenTwoPlayer

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
                let
                    zero =
                        Maybe.withDefault 0.0 model.tilt.zeroGamma

                    deltaDeg =
                        model.tilt.gamma - zero

                    deadZone =
                        if abs deltaDeg < 3.0 then
                            0.0

                        else
                            deltaDeg - 3.0 * (deltaDeg / abs deltaDeg)
                in
                clamp -1.0 1.0 (deadZone / 28.0)

            else
                0.0

        steer =
            if abs keySteer > 0.05 then
                keySteer

            else
                tiltSteer

        brake =
            if held "s" || (not isSplit && held "ArrowDown") || model.touch.brake then
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
            if held "ArrowDown" then
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
banner. Ignores pieces that are already gone (e.g. hit by both players).
-}
smashScenery :
    Language
    -> { obstacleId : Int, impactSpeed : Float, direction : Vec3 }
    -> ( RaceState, List (Cmd Msg) )
    -> ( RaceState, List (Cmd Msg) )
smashScenery lang smash ( race, cmds ) =
    case List.filter (\o -> o.id == smash.obstacleId) race.track.obstacles |> List.head of
        Nothing ->
            ( race, cmds )

        Just obs ->
            let
                nextTrack =
                    Track.removeObstacles [ obs.id ] race.track

                ( pieces, nextSeed ) =
                    spawnSceneryDebris race.debrisSeed obs smash.impactSpeed smash.direction

                alert =
                    case race.hudAlert of
                        Just existing ->
                            Just existing

                        Nothing ->
                            Just { title = RI18n.smashBanner lang, subtitle = Nothing, positive = True, timer = 0.7 }
            in
            ( { race
                | track = nextTrack
                , bundle = TerrainMesh.rebuildChunkScenery nextTrack obs.s race.bundle
                , debris = List.take maxDebrisPieces (pieces ++ race.debris)
                , debrisSeed = nextSeed
                , shake = min 1.0 (race.shake + 0.3 + smash.impactSpeed / 30.0)
                , hudAlert = alert
              }
            , Ports.playSfx { kind = "smash", intensity = clamp 0.3 1.0 (smash.impactSpeed / 14.0) } :: cmds
            )


isScenerySmash : PhysicsEvent -> Bool
isScenerySmash ev =
    case ev of
        ScenerySmashed _ ->
            True

        _ ->
            False


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
                LoadGaragePage garage ->
                    let
                        canvasH =
                            if w >= 1024 then
                                h

                            else
                                max 280 (round (toFloat h * 0.45))
                    in
                    LoadGaragePage { garage | camera = Camera.setViewportHeight (toFloat canvasH) garage.camera }

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
        LoadGaragePage garage ->
            ( { model | page = LoadGaragePage { garage | camera = fn garage.camera } }
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
                    Audio.deriveAudioState isDriving race.isPaused model.audioMuted race.physics race.vehicle
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
        MenuPage ->
            Nothing

        LoadGaragePage g ->
            Just g.document

        RacePage r ->
            Just r.document

        FinishLeaderboardPage r ->
            Just r.document


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
                LoadGaragePage _ ->
                    Browser.Events.onAnimationFrameDelta AnimationFrame

                RacePage race ->
                    if race.isPaused then
                        Sub.none

                    else
                        Browser.Events.onAnimationFrameDelta AnimationFrame

                _ ->
                    Sub.none
    in
    Sub.batch
        [ animSub
        , Ports.viewportResized ViewportResized
        , Ports.fileContentReceived FileContentReceived
        , Ports.fileLoadError FileLoadError
        , Ports.binaryHashDecoded BinaryHashDecoded
        , Ports.hashChanged HashChanged
        , Ports.tiltOrientationReceived TiltOrientationReceived
        , Ports.multiplayerRoomStateChanged MultiplayerRoomChanged
        , Ports.multiplayerError MultiplayerError
        , Ports.qrCodeScanned QrCodeScanned
        , Ports.qrScannerError QrScannerError
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
        MenuPage ->
            Menu.view
                { language = model.language
                , pasteText = model.pasteText
                , pasteError = model.pasteError
                , selectedTrack = model.trackId
                , seed = model.seed
                , leaderboard = model.leaderboard
                , audioMuted = model.audioMuted
                , raceMode = model.raceMode
                }
                { onPasteChanged = PasteChanged
                , onSubmitPaste = SubmitPaste
                , onRequestFileUpload = RequestFileUpload
                , onSelectPreset = SelectPreset
                , onSelectTrack = SelectTrack
                , onChangeSeed = ChangeSeed
                , onRandomSeed = RandomSeed
                , onToggleLanguage = ToggleLanguage
                , onToggleAudio = ToggleAudioMute
                , onToggleRaceMode = ToggleRaceMode
                }

        LoadGaragePage garage ->
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
                , selectedTrack = model.trackId
                , seed = model.seed
                , viewportWidth = model.viewportWidth
                , viewportHeight = model.viewportHeight
                , shareCopiedNotice = model.shareCopiedNotice
                , raceMode = model.raceMode
                }
                { onBackToMenu = BackToMenu
                , onStartRace = GarageToRace
                , onStartHostMultiplayer = StartHostMultiplayer
                , onJoinMultiplayer = JoinMultiplayer
                , onSelectTrack = SelectTrack
                , onCopyShareLink = CopyShareLink
                , onOpenQrScanner = OpenQrScanner
                , onToggleRaceMode = ToggleRaceMode
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
                                in
                                { p2Spec = p2.vehicle
                                , p2Meshes = p2.meshes
                                , p2Physics = p2.physics
                                , p2Camera = p2.camera
                                , p2HudData =
                                    { language = model.language
                                    , physics = p2.physics
                                    , trackLengthM = race.track.totalLengthM
                                    , trackTotalDropM = race.track.totalDropM
                                    , elapsedTime = race.elapsedTime
                                    , countdown = p2CountdownMaybe
                                    , isPaused = race.isPaused
                                    , crashRecovery = p2CrashMaybe
                                    , audioMuted = model.audioMuted
                                    , cameraMode = race.cameraMode
                                    , splitDelta = p2.splitDelta
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
            in
            Race.view
                race.bundle
                race.track
                race.vehicle
                race.meshes
                model.staticMeshes.cube
                model.staticMeshes.shadow
                race.physics
                race.debris
                race.camera
                model.viewportWidth
                model.viewportHeight
                { language = model.language
                , physics = race.physics
                , trackLengthM = race.track.totalLengthM
                , trackTotalDropM = race.track.totalDropM
                , elapsedTime = race.elapsedTime
                , countdown = countdownMaybe
                , isPaused = race.isPaused
                , crashRecovery = crashMaybe
                , audioMuted = model.audioMuted
                , cameraMode = race.cameraMode
                , splitDelta = race.splitDelta
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
                { onTogglePause = TogglePause
                , onResume = ResumeRace
                , onRestartRace = RestartRace
                , onGoGarage = InspectInGarage
                , onGoMenu = BackToMenu
                , onRespawnCheckpoint = RespawnCheckpoint
                , onCycleCamera = CycleCamera
                , onToggleAudio = ToggleAudioMute
                , onToggleTouchUi = ToggleTouchUi
                , onRequestTilt = RequestTilt
                , onTouchSteerLeft = TouchSteerLeft
                , onTouchSteerRight = TouchSteerRight
                , onTouchBrake = TouchBrake
                , onTouchTuck = TouchTuck
                , onTouchDrift = TouchDrift
                }
                { onMouseDown = CamMouseDown
                , onMouseMove = CamMouseMove
                , onMouseUp = CamMouseUp
                , onWheel = CamWheel
                }
                maybeSplit
                race.multiplayer
                { onStartCountdown = StartMultiplayerCountdown
                , onCopyShareLink = CopyMultiplayerLink
                }

        FinishLeaderboardPage res ->
            Results.view
                { language = model.language
                , trackId = res.trackId
                , seed = res.seed
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
                , onCopyShareLink = CopyShareLink
                , onInspectGarage = InspectInGarage
                , onBackToMenu = BackToMenu
                }


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
        | page = LoadGaragePage garage
        , pressedKeys = Set.empty
        , touch = emptyTouch
        , shareCopiedNotice = False
      }
    , silenceAudioCmd
    )
