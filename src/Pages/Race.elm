module Pages.Race exposing (RaceOrbitActions, SplitScreenData, view)

{-| Live downhill 3D racing viewport composition (`RacePage`).
Supports single-player full-bleed racing and dual-player local split-screen driving.
-}

import FeatherIcons
import Game.Camera as GameCamera
import Game.Multiplayer as Multiplayer exposing (RemotePlayer, Role(..), RoomState, RoomStatus(..))
import Game.MultiplayerUI as MultiplayerUI
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (vec3)
import Racer.Analyze exposing (CarMeshBundle)
import Racer.HUD as HUD exposing (HudActions, HudViewData)
import Racer.Render as Render exposing (RenderCarSlot)
import Racer.TerrainMesh exposing (TrackRenderBundle)
import Racer.TerrainShader exposing (DebrisPiece, TerrainVertex)
import Racer.Track as Track
import Racer.Types exposing (CameraMode(..), GeneratedTrack, PhysicsState, SurfaceType(..), VehicleSpec)
import Render.Camera exposing (Camera)
import Tailwind as Tw
import Tailwind.Theme as Th
import WebGL


{-| Telemetry and render bundle for Player 2 during local two-player split-screen races.
-}
type alias SplitScreenData =
    { p2Spec : VehicleSpec
    , p2Meshes : CarMeshBundle
    , p2Physics : PhysicsState
    , p2Camera : Camera
    , p2HudData : HudViewData
    }


{-| Optional mouse/wheel camera handlers active while viewing in `OrbitCam`.
-}
type alias RaceOrbitActions msg =
    { onMouseDown : Float -> Float -> msg
    , onMouseMove : Float -> Float -> msg
    , onMouseUp : msg
    , onWheel : Float -> msg
    }


{-| Render the full-viewport WebGL racing scene and overlaid HUD, with optional
Player 2 split-screen dual viewports or 4-player online multiplayer commentator split-view.
-}
view :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> WebGL.Mesh TerrainVertex
    -> WebGL.Mesh TerrainVertex
    -> PhysicsState
    -> List DebrisPiece
    -> Camera
    -> Int
    -> Int
    -> HudViewData
    -> HudActions msg
    -> RaceOrbitActions msg
    -> Maybe SplitScreenData
    -> Maybe RoomState
    -> { onStartCountdown : msg, onCopyShareLink : msg }
    -> Html msg
view bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions orbitActions maybeSplit maybeMulti multiActions =
    case maybeMulti of
        Just room ->
            case room.role of
                HostCommentator ->
                    viewFourPlayerSplitScreen bundle track spec meshes cubeMesh shadowMesh physics debris width height hudData hudActions room multiActions

                JoinedPlayer ->
                    viewMultiplayerSinglePlayer bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions orbitActions room multiActions

        Nothing ->
            case maybeSplit of
                Nothing ->
                    viewSinglePlayer bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions orbitActions

                Just p2 ->
                    viewTwoPlayerSplitScreen bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions p2


viewMultiplayerSinglePlayer :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> WebGL.Mesh TerrainVertex
    -> WebGL.Mesh TerrainVertex
    -> PhysicsState
    -> List DebrisPiece
    -> Camera
    -> Int
    -> Int
    -> HudViewData
    -> HudActions msg
    -> RaceOrbitActions msg
    -> RoomState
    -> { onStartCountdown : msg, onCopyShareLink : msg }
    -> Html msg
viewMultiplayerSinglePlayer bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions orbitActions room multiActions =
    let
        localCar =
            { spec = spec
            , meshes = meshes
            , physics = physics
            }

        remoteCars =
            List.filter (\p -> p.slot /= room.mySlot) room.players
                |> List.map
                    (\p ->
                        let
                            remY =
                                (Vec3.toRecord p.rotation).y

                            remP =
                                (Vec3.toRecord p.rotation).x

                            remR =
                                (Vec3.toRecord p.rotation).z

                            remPhysics =
                                { position = p.position
                                , velocity = vec3 0 0 p.speed
                                , yaw = remY
                                , yawRate = 0.0
                                , pitch = remP
                                , pitchRate = 0.0
                                , roll = remR
                                , rollRate = 0.0
                                , steerAngle = 0.0
                                , leanInput = { x = 0, y = 0 }
                                , wheelSpinAngles = [ 0, 0, 0, 0 ]
                                , suspensionCompressions = [ 0, 0, 0, 0 ]
                                , groundedWheels = 4
                                , airborneTime = 0.0
                                , surfaceUnderCar = AsphaltTrack
                                , integrity = 1.0
                                , chassisScraping = False
                                , offTrack = False
                                , highSpeedBonus = False
                                , rolloverTimer = 0.0
                                , progressS = p.progress * track.totalLengthM
                                , checkpointIndex = 0
                                , checkpointAnchor = { s = 0, position = p.position, yaw = remY }
                                , lastImpactCooldown = 0.0
                                , nextPopThreshold = 1.0
                                , gradeRatio = 0.0
                                }
                        in
                        { spec = spec
                        , meshes = meshes
                        , physics = remPhysics
                        }
                    )

        allCars =
            localCar :: remoteCars

        mouseDecoder msgCtor =
            Decode.map2 msgCtor
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)

        canvasAttrs =
            case hudData.cameraMode of
                OrbitCam ->
                    [ Html.Events.on "mousedown" (mouseDecoder orbitActions.onMouseDown)
                    , Html.Events.on "mousemove" (mouseDecoder orbitActions.onMouseMove)
                    , Html.Events.on "mouseup" (Decode.succeed orbitActions.onMouseUp)
                    , Html.Events.on "mouseleave" (Decode.succeed orbitActions.onMouseUp)
                    , Html.Events.preventDefaultOn "wheel"
                        (Decode.field "deltaY" Decode.float |> Decode.map (\dy -> ( orbitActions.onWheel dy, True )))
                    ]

                _ ->
                    []
    in
    div
        [ Tw.classes [ Tw.relative, Tw.w_screen, Tw.h_screen, Tw.overflow_hidden ]
        , Attr.style "background" "#0f172a"
        ]
        [ Render.renderRaceViewportMulti bundle track allCars cubeMesh shadowMesh debris camera width height canvasAttrs
        , div
            [ Tw.classes [ Tw.absolute, Tw.z_30, Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_xl, Tw.border ]
            , Attr.style "top" "1rem"
            , Attr.style "left" "1rem"
            , Attr.style "background" "rgba(15, 23, 42, 0.88)"
            , Attr.style "border-color" (Multiplayer.slotColor room.mySlot)
            ]
            [ span
                [ Tw.classes [ Tw.rounded_full ]
                , Attr.style "width" "10px"
                , Attr.style "height" "10px"
                , Attr.style "background" (Multiplayer.slotColor room.mySlot)
                ]
                []
            , span [ Tw.classes [ Tw.text_xs, Tw.font_extrabold, Tw.text_simple Th.white ] ]
                [ text ("P" ++ String.fromInt room.mySlot ++ " • ROOM: " ++ room.roomId) ]
            , button
                [ Attr.type_ "button"
                , onClick multiActions.onCopyShareLink
                , Tw.classes [ Tw.text_xs, Tw.font_bold, Tw.px Th.s2, Tw.py Th.s0_dot_5, Tw.rounded_lg, Tw.border ]
                , Attr.style "background" "rgba(30, 41, 59, 0.8)"
                , Attr.style "border-color" "rgba(148, 163, 184, 0.3)"
                , Attr.style "color" "#94a3b8"
                , Attr.style "cursor" "pointer"
                ]
                [ text "Copy" ]
            ]
        , HUD.viewHudOverlay hudData hudActions
        , case room.status of
            Countdown n ->
                MultiplayerUI.viewCountdownOverlay { secondsRemaining = n }

            _ ->
                text ""
        ]


viewFourPlayerSplitScreen :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> WebGL.Mesh TerrainVertex
    -> WebGL.Mesh TerrainVertex
    -> PhysicsState
    -> List DebrisPiece
    -> Int
    -> Int
    -> HudViewData
    -> HudActions msg
    -> RoomState
    -> { onStartCountdown : msg, onCopyShareLink : msg }
    -> Html msg
viewFourPlayerSplitScreen bundle track spec meshes cubeMesh shadowMesh physics debris width height hudData hudActions room multiActions =
    let
        qW =
            max 160 (width // 2)

        qH =
            max 120 (height // 2)

        allCars =
            List.map
                (\p ->
                    let
                        remY =
                            (Vec3.toRecord p.rotation).y

                        remP =
                            (Vec3.toRecord p.rotation).x

                        remR =
                            (Vec3.toRecord p.rotation).z

                        remPhysics =
                            { position = p.position
                            , velocity = vec3 0 0 p.speed
                            , yaw = remY
                            , yawRate = 0.0
                            , pitch = remP
                            , pitchRate = 0.0
                            , roll = remR
                            , rollRate = 0.0
                            , steerAngle = 0.0
                            , leanInput = { x = 0, y = 0 }
                            , wheelSpinAngles = [ 0, 0, 0, 0 ]
                            , suspensionCompressions = [ 0, 0, 0, 0 ]
                            , groundedWheels = 4
                            , airborneTime = 0.0
                            , surfaceUnderCar = AsphaltTrack
                            , integrity = 1.0
                            , chassisScraping = False
                            , offTrack = False
                            , highSpeedBonus = False
                            , rolloverTimer = 0.0
                            , progressS = p.progress * track.totalLengthM
                            , checkpointIndex = 0
                            , checkpointAnchor = { s = 0, position = p.position, yaw = remY }
                            , lastImpactCooldown = 0.0
                            , nextPopThreshold = 1.0
                            , gradeRatio = 0.0
                            }
                    in
                    { spec = spec
                    , meshes = meshes
                    , physics = remPhysics
                    }
                )
                room.players

        renderQuadrant slot =
            let
                playerMaybe =
                    List.filter (\p -> p.slot == slot) room.players
                        |> List.head
            in
            case playerMaybe of
                Just p ->
                    let
                        yawRad =
                            (Vec3.toRecord p.rotation).y

                        fwd =
                            vec3 (sin yawRad) 0.0 (cos yawRad)

                        camEye =
                            GameCamera.chasePosition p.position fwd 9.0 3.2

                        cam =
                            Camera.init
                                |> Camera.setViewportHeight (toFloat (max 1 qH))
                                |> Camera.setSceneRadius 450.0
                                |> (\c ->
                                        { c
                                            | azimuth = yawRad + pi
                                            , elevation = 0.28
                                            , distance = 5.0
                                            , target = p.position
                                        }
                                   )
                    in
                    div
                        [ Tw.classes [ Tw.relative, Tw.w_full, Tw.h_full, Tw.overflow_hidden, Tw.border ]
                        , Attr.style "border-color" "rgba(148, 163, 184, 0.2)"
                        , Attr.style "background" "#020617"
                        ]
                        [ Render.renderRaceViewportMulti bundle track allCars cubeMesh shadowMesh debris cam qW qH []
                        , div
                            [ Tw.classes [ Tw.absolute, Tw.z_20, Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.border ]
                            , Attr.style "top" "0.5rem"
                            , Attr.style "left" "0.5rem"
                            , Attr.style "background" "rgba(15, 23, 42, 0.85)"
                            , Attr.style "border-color" (Multiplayer.slotColor slot)
                            ]
                            [ span
                                [ Tw.classes [ Tw.rounded_full ]
                                , Attr.style "width" "8px"
                                , Attr.style "height" "8px"
                                , Attr.style "background" (Multiplayer.slotColor slot)
                                ]
                                []
                            , span [ Tw.classes [ Tw.text_xs, Tw.font_black, Tw.text_simple Th.white ] ]
                                [ text ("P" ++ String.fromInt slot ++ ": " ++ p.name) ]
                            , span [ Tw.classes [ Tw.text_xs, Tw.font_mono, Tw.font_bold ], Attr.style "color" "#38bdf8" ]
                                [ text (String.fromInt (round (p.speed * 3.6)) ++ " km/h") ]
                            , if p.finished then
                                span [ Tw.classes [ Tw.text_xs, Tw.font_black ], Attr.style "color" "#4ade80" ]
                                    [ text "FINISH" ]

                              else
                                text ""
                            ]
                        ]

                Nothing ->
                    div
                        [ Tw.classes [ Tw.relative, Tw.w_full, Tw.h_full, Tw.flex, Tw.flex_col, Tw.items_center, Tw.justify_center, Tw.border ]
                        , Attr.style "border-color" "rgba(148, 163, 184, 0.15)"
                        , Attr.style "background" "rgba(15, 23, 42, 0.6)"
                        ]
                        [ span
                            [ Tw.classes [ Tw.text_xs, Tw.font_black, Tw.px Th.s2, Tw.py Th.s0_dot_5, Tw.rounded_md, Tw.border, Tw.mb Th.s1 ]
                            , Attr.style "border-color" (Multiplayer.slotColor slot)
                            , Attr.style "color" (Multiplayer.slotColor slot)
                            ]
                            [ text ("SLOT " ++ String.fromInt slot) ]
                        , span [ Tw.classes [ Tw.text_xs, Tw.font_semibold ], Attr.style "color" "#94a3b8" ]
                            [ text "Waiting for racer to join..." ]
                        , span [ Tw.classes [ Tw.text_xs, Tw.font_mono, Tw.mt Th.s0_dot_5 ], Attr.style "color" "#64748b" ]
                            [ text ("Invite: #room=" ++ room.roomId) ]
                        ]
    in
    div
        [ Tw.classes [ Tw.relative, Tw.w_screen, Tw.h_screen, Tw.overflow_hidden ]
        , Attr.style "background" "#020617"
        ]
        [ -- 2x2 Grid Viewports
          div [ Tw.classes [ Tw.grid, Tw.grid_cols_2, Tw.w_full, Tw.h_full ] ]
            [ renderQuadrant 1
            , renderQuadrant 2
            , renderQuadrant 3
            , renderQuadrant 4
            ]

        -- Top Commentator Header
        , MultiplayerUI.viewCommentatorHeader
            { room = room
            , startLabel = "START RACE"
            , onCopyShareLink = multiActions.onCopyShareLink
            , onStartCountdown = multiActions.onStartCountdown
            }
        , case room.status of
            Countdown n ->
                MultiplayerUI.viewCountdownOverlay { secondsRemaining = n }

            _ ->
                text ""
        ]


viewSinglePlayer :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> WebGL.Mesh TerrainVertex
    -> WebGL.Mesh TerrainVertex
    -> PhysicsState
    -> List DebrisPiece
    -> Camera
    -> Int
    -> Int
    -> HudViewData
    -> HudActions msg
    -> RaceOrbitActions msg
    -> Html msg
viewSinglePlayer bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions orbitActions =
    let
        mouseDecoder msgCtor =
            Decode.map2 msgCtor
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)

        canvasAttrs =
            case hudData.cameraMode of
                OrbitCam ->
                    [ Html.Events.on "mousedown" (mouseDecoder orbitActions.onMouseDown)
                    , Html.Events.on "mousemove" (mouseDecoder orbitActions.onMouseMove)
                    , Html.Events.on "mouseup" (Decode.succeed orbitActions.onMouseUp)
                    , Html.Events.on "mouseleave" (Decode.succeed orbitActions.onMouseUp)
                    , Html.Events.preventDefaultOn "wheel"
                        (Decode.field "deltaY" Decode.float |> Decode.map (\dy -> ( orbitActions.onWheel dy, True )))
                    ]

                _ ->
                    []
    in
    div
        [ Tw.classes [ Tw.relative, Tw.w_screen, Tw.h_screen, Tw.overflow_hidden ]
        , Attr.style "background" "#0f172a"
        ]
        [ Render.renderRaceViewport bundle track spec meshes cubeMesh shadowMesh physics debris camera width height canvasAttrs
        , HUD.viewHudOverlay hudData hudActions
        ]


viewTwoPlayerSplitScreen :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> WebGL.Mesh TerrainVertex
    -> WebGL.Mesh TerrainVertex
    -> PhysicsState
    -> List DebrisPiece
    -> Camera
    -> Int
    -> Int
    -> HudViewData
    -> HudActions msg
    -> SplitScreenData
    -> Html msg
viewTwoPlayerSplitScreen bundle track p1Spec p1Meshes cubeMesh shadowMesh p1Physics debris p1Camera width height p1HudData hudActions p2 =
    let
        isWide =
            width >= height

        ( ( p1W, p1H ), ( p2W, p2H ) ) =
            if isWide then
                let
                    halfW =
                        width // 2
                in
                ( ( halfW, height ), ( width - halfW, height ) )

            else
                let
                    halfH =
                        height // 2
                in
                ( ( width, halfH ), ( width, height - halfH ) )

        bothCars =
            [ { spec = p1Spec, meshes = p1Meshes, physics = p1Physics }
            , { spec = p2.p2Spec, meshes = p2.p2Meshes, physics = p2.p2Physics }
            ]

        p2FirstCars =
            [ { spec = p2.p2Spec, meshes = p2.p2Meshes, physics = p2.p2Physics }
            , { spec = p1Spec, meshes = p1Meshes, physics = p1Physics }
            ]

        playerBadge label colorStr keyStr =
            div
                [ Tw.classes [ Tw.absolute, Tw.z_20, Tw.flex, Tw.items_center, Tw.gap Th.s1_dot_5, Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.border, Tw.shadow_lg ]
                , Attr.style "top" "0.75rem"
                , Attr.style "left" "0.75rem"
                , Attr.style "background" "rgba(15, 23, 42, 0.82)"
                , Attr.style "border-color" colorStr
                ]
                [ span
                    [ Tw.classes [ Tw.rounded_full ]
                    , Attr.style "width" "10px"
                    , Attr.style "height" "10px"
                    , Attr.style "background" colorStr
                    ]
                    []
                , span [ Tw.classes [ Tw.text_xs, Tw.font_black, Tw.text_simple Th.white ] ] [ text label ]
                , span [ Tw.classes [ Tw.text_xs, Tw.font_mono ], Attr.style "color" "#94a3b8" ] [ text ("[" ++ keyStr ++ "]") ]
                ]
    in
    div
        [ Tw.classes
            ([ Tw.relative, Tw.w_screen, Tw.h_screen, Tw.overflow_hidden, Tw.flex ]
                ++ (if isWide then
                        [ Tw.flex_row ]

                    else
                        [ Tw.flex_col ]
                   )
            )
        , Attr.style "background" "#020617"
        ]
        [ -- Player 1 Viewport + HUD
          div
            [ Tw.classes
                ([ Tw.relative, Tw.overflow_hidden ]
                    ++ (if isWide then
                            [ Tw.w_1over2, Tw.h_full, Tw.border_r_2 ]

                        else
                            [ Tw.w_full, Tw.h_1over2, Tw.border_b_2 ]
                       )
                )
            , Attr.style "border-color" "#f59e0b"
            ]
            [ Render.renderRaceViewportMulti bundle track bothCars cubeMesh shadowMesh debris p1Camera p1W p1H []
            , playerBadge "PLAYER 1" "#38bdf8" "WASD"
            , HUD.viewHudOverlay p1HudData hudActions
            ]

        -- Player 2 Viewport + HUD
        , div
            [ Tw.classes
                ([ Tw.relative, Tw.overflow_hidden ]
                    ++ (if isWide then
                            [ Tw.w_1over2, Tw.h_full ]

                        else
                            [ Tw.w_full, Tw.h_1over2 ]
                       )
                )
            ]
            [ Render.renderRaceViewportMulti bundle track p2FirstCars cubeMesh shadowMesh debris p2.p2Camera p2W p2H []
            , playerBadge "PLAYER 2" "#f43f5e" "ARROWS"
            , HUD.viewHudOverlay p2.p2HudData hudActions
            ]
        ]
