module Pages.Race exposing (RaceOrbitActions, SplitScreenData, view)

{-| Live downhill 3D racing viewport composition (`RacePage`).
Supports single-player full-bleed racing, dual-player local split-screen driving
(two players sharing one device), and online rooms. In an online room every
player races on their own device, so their own chase view fills the whole
screen and the other racers are cars in that view.
-}

import DesignTokens.Colors as Brand
import FeatherIcons
import Game.Grid as Grid
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
import Racer.Scenery exposing (Props)
import Racer.TerrainMesh exposing (TrackRenderBundle)
import Racer.TerrainShader exposing (DebrisPiece, TerrainVertex)
import Racer.Track as Track
import Racer.Types exposing (CameraMode(..), GeneratedTrack, PhysicsState, SurfaceType(..), VehicleSpec)
import Render.Camera as Camera exposing (Camera)
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
Player 2 split-screen dual viewports, or an online room's cars and room bar.
-}
view :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> Props
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
    -> MultiplayerActions msg
    -> Html msg
view bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions maybeSplit maybeMulti multiActions =
    div [ Attr.class "is-racing relative w-full h-full overflow-hidden" ]
        [ case maybeMulti of
            Just room ->
                viewRoomRace bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions room multiActions

            Nothing ->
                case maybeSplit of
                    Nothing ->
                        viewSinglePlayer bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions

                    Just p2 ->
                        viewTwoPlayerSplitScreen bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions p2
        ]


{-| Room callbacks, plus the look of another player's car by vehicle id once
their model has arrived (`Nothing` until then: the car shows with the local
car's look).
-}
type alias MultiplayerActions msg =
    { onStartCountdown : msg
    , onCopyShareLink : msg
    , remoteCar : String -> Maybe { spec : VehicleSpec, meshes : CarMeshBundle }
    }


{-| Spec and meshes to draw a room player's car with.
-}
lookFor : MultiplayerActions msg -> VehicleSpec -> CarMeshBundle -> RemotePlayer -> ( VehicleSpec, CarMeshBundle )
lookFor actions spec meshes p =
    case actions.remoteCar p.vehicleId of
        Just look ->
            ( look.spec, look.meshes )

        Nothing ->
            ( spec, meshes )


{-| An online room's race on this device: the local player's chase view fills
the screen, with the same camera, size and aspect as a single-player race, and
every other racer is a car in it.
-}
viewRoomRace :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> Props
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
    -> MultiplayerActions msg
    -> Html msg
viewRoomRace bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions room multiActions =
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
                                , surfaceUnderCar = DirtRoad
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

                            ( remSpec, remMeshes ) =
                                lookFor multiActions spec meshes p
                        in
                        { spec = remSpec
                        , meshes = remMeshes
                        , physics = remPhysics
                        }
                    )

        allCars =
            localCar :: remoteCars

        portrait =
            width < height

        -- Name, colour and speed of every racer, the local player's first.
        racerBadges =
            Grid.selfFirst .slot room.mySlot room.players
                |> List.map (racerBadge room)

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
        , Attr.style "background" Brand.legoBlack
        ]
        [ Render.renderRaceViewportMulti bundle track allCars props shadowMesh debris camera width height canvasAttrs
        , if portrait then
            -- Portrait is too narrow for floating badges beside the timer and
            -- the health bar: stack them into the HUD column instead.
            HUD.viewHudOverlayWith
                { belowTopBar =
                    [ roomBar room multiActions
                    , div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.gap Th.s2, Tw.w_full ] ] racerBadges
                    ]
                , aboveFooter = rosterSlot room
                }
                hudData
                hudActions

          else
            div []
                [ roomHeader room multiActions
                , HUD.viewHudOverlay hudData hudActions
                , div
                    [ Tw.classes [ Tw.absolute, Tw.z_20, Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5, Tw.pointer_events_none ]
                    , Attr.style "top" "4.5rem"
                    , Attr.style "left" "1rem"
                    ]
                    racerBadges
                , rosterStrip room
                ]
        , case room.status of
            Countdown n ->
                MultiplayerUI.viewCountdownOverlay { secondsRemaining = n }

            _ ->
                text ""
        ]


{-| Top bar of an online room: the commentator controls (count, copy link,
start) for the host, a room badge for everyone else.
-}
roomHeader : RoomState -> MultiplayerActions msg -> Html msg
roomHeader room multiActions =
    case room.role of
        HostCommentator ->
            MultiplayerUI.viewCommentatorHeader
                { room = room
                , startLabel = "START RACE"
                , onCopyShareLink = multiActions.onCopyShareLink
                , onStartCountdown = multiActions.onStartCountdown
                }

        JoinedPlayer ->
            joinerPill
                [ Tw.classes [ Tw.absolute, Tw.z_30 ]
                , Attr.style "top" "1rem"
                , Attr.style "left" "1rem"
                ]
                room
                multiActions


{-| `roomHeader` laid out in the HUD column (portrait): the joiner's pill in
flow under the timer, and the host's commentator bar, whose shared markup
floats it, pinned back into flow by `.racer-room-bar` (main.css).
-}
roomBar : RoomState -> MultiplayerActions msg -> Html msg
roomBar room multiActions =
    case room.role of
        HostCommentator ->
            div [ Attr.class "racer-room-bar", Tw.classes [ Tw.w_full ] ]
                [ roomHeader room multiActions ]

        JoinedPlayer ->
            joinerPill [ Tw.classes [ Tw.self_start, Tw.pointer_events_auto ] ] room multiActions


{-| Room badge of a joined player: their colour, slot, the room code and a
copy-link button. `placement` positions it.
-}
joinerPill : List (Html.Attribute msg) -> RoomState -> MultiplayerActions msg -> Html msg
joinerPill placement room multiActions =
    div
        (placement
            ++ [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_xl, Tw.border ]
               , Attr.style "background" "rgba(15, 23, 42, 0.88)"
               , Attr.style "border-color" (Multiplayer.slotColor room.mySlot)
               ]
        )
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


{-| Who is in the room, shown while the room waits in its lobby.
-}
rosterStrip : RoomState -> Html msg
rosterStrip room =
    case room.status of
        Lobby ->
            div
                [ Tw.classes [ Tw.absolute, Tw.z_30, Tw.px Th.s3, Tw.py Th.s1_dot_5 ]
                , Attr.style "left" "0"
                , Attr.style "right" "0"
                , Attr.style "bottom" "1rem"
                ]
                [ MultiplayerUI.viewRoster room ]

        _ ->
            text ""


{-| The lobby roster in the HUD column (portrait), above the health bar.
-}
rosterSlot : RoomState -> List (Html msg)
rosterSlot room =
    case room.status of
        Lobby ->
            [ MultiplayerUI.viewRoster room ]

        _ ->
            []


{-| Name, colour and speed of a racer in the room, the name cut short rather
than running into the next badge.
-}
racerBadge : RoomState -> RemotePlayer -> Html msg
racerBadge room p =
    let
        isMe =
            p.slot == room.mySlot
    in
    div
        [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.border, Tw.w_fit, Tw.max_w_full, Tw.overflow_hidden, Tw.whitespace_nowrap ]
        , Attr.style "background" "rgba(15, 23, 42, 0.85)"
        , Attr.style "border-color" (Multiplayer.slotColor p.slot)
        ]
        [ span
            [ Tw.classes [ Tw.rounded_full, Tw.shrink_0 ]
            , Attr.style "width" "8px"
            , Attr.style "height" "8px"
            , Attr.style "background" (Multiplayer.slotColor p.slot)
            ]
            []
        , span [ Tw.classes [ Tw.text_xs, Tw.font_black, Tw.text_simple Th.white, Tw.min_w Th.s0, Tw.truncate ] ]
            [ text
                ("P"
                    ++ String.fromInt p.slot
                    ++ ": "
                    ++ p.name
                    ++ (if isMe then
                            " (you)"

                        else
                            ""
                       )
                )
            ]
        , span [ Tw.classes [ Tw.text_xs, Tw.font_mono, Tw.font_bold, Tw.shrink_0 ], Attr.style "color" "#38bdf8" ]
            [ text (String.fromInt (round (p.speed * 3.6)) ++ " km/h") ]
        , if p.finished then
            span [ Tw.classes [ Tw.text_xs, Tw.font_black ], Attr.style "color" "#4ade80" ]
                [ text "FINISH" ]

          else
            text ""
        ]


viewSinglePlayer :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> Props
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
viewSinglePlayer bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions =
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
        , Attr.style "background" Brand.legoBlack
        ]
        [ Render.renderRaceViewport bundle track spec meshes props shadowMesh physics debris camera width height canvasAttrs
        , HUD.viewHudOverlay hudData hudActions
        ]


viewTwoPlayerSplitScreen :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> Props
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
viewTwoPlayerSplitScreen bundle track p1Spec p1Meshes props shadowMesh p1Physics debris p1Camera width height p1HudData hudActions p2 =
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

        playerBadge placement label colorStr keyStr =
            div
                (placement
                    ++ [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s1_dot_5, Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.border, Tw.shadow_lg ]
                       , Attr.style "background" "rgba(15, 23, 42, 0.82)"
                       , Attr.style "border-color" colorStr
                       ]
                )
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

        -- Side by side, each badge floats in its view's top-left corner. Stacked
        -- (portrait), the halves are too narrow for it beside the timer and
        -- toolbar, so it goes into the HUD column under them.
        viewHalf hudData label colorStr keyStr =
            if isWide then
                [ playerBadge
                    [ Tw.classes [ Tw.absolute, Tw.z_20 ]
                    , Attr.style "top" "0.75rem"
                    , Attr.style "left" "0.75rem"
                    ]
                    label
                    colorStr
                    keyStr
                , HUD.viewHudOverlay hudData hudActions
                ]

            else
                [ HUD.viewHudOverlayWith
                    { belowTopBar = [ playerBadge [ Tw.classes [ Tw.self_start ] ] label colorStr keyStr ]
                    , aboveFooter = []
                    }
                    hudData
                    hudActions
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
        , Attr.style "background" Brand.legoBlack
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
            , Attr.style "border-color" Brand.legoYellow
            ]
            (Render.renderRaceViewportMulti bundle track bothCars props shadowMesh debris p1Camera p1W p1H []
                :: viewHalf p1HudData "PLAYER 1" "#38bdf8" "WASD"
            )

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
            (Render.renderRaceViewportMulti bundle track p2FirstCars props shadowMesh debris p2.p2Camera p2W p2H []
                :: viewHalf p2.p2HudData "PLAYER 2" "#f43f5e" "ARROWS"
            )
        ]
