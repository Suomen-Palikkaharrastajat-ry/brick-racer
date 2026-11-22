module Pages.Race exposing (RaceOrbitActions, SplitScreenData, view)

{-| Live downhill 3D racing viewport composition (`RacePage`).
Supports single-player full-bleed racing, dual-player local split-screen driving,
and online rooms: one full view alone, a grid of every player's view otherwise.
-}

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
Player 2 split-screen dual viewports or online room grids.
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
    case maybeMulti of
        Just room ->
            viewMultiplayerSinglePlayer bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions room multiActions

        Nothing ->
            case maybeSplit of
                Nothing ->
                    viewSinglePlayer bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions

                Just p2 ->
                    viewTwoPlayerSplitScreen bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions p2


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


viewMultiplayerSinglePlayer :
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
viewMultiplayerSinglePlayer bundle track spec meshes props shadowMesh physics debris camera width height hudData hudActions orbitActions room multiActions =
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

        layout =
            Grid.layoutFor (Grid.orientation width height) (List.length room.players)

        portrait =
            Grid.orientation width height == Grid.Portrait

        -- In portrait the first row of tiles lies under the timer and toolbar,
        -- so its badges move into the HUD column, one per grid column.
        badgesInTiles shape index =
            not portrait || index >= shape.cols

        firstRowBadges =
            case ( portrait, layout ) of
                ( True, Grid.Tiles shape ) ->
                    [ div
                        [ Attr.style "display" "grid"
                        , Attr.style "grid-template-columns" ("repeat(" ++ String.fromInt shape.cols ++ ", minmax(0, 1fr))")
                        , Tw.classes [ Tw.gap Th.s2, Tw.w_full ]
                        ]
                        (Grid.selfFirst .slot room.mySlot room.players
                            |> List.take shape.cols
                            |> List.map (\p -> div [ Tw.classes [ Tw.min_w Th.s0 ] ] [ tileBadge inFlowBadge room p ])
                        )
                    ]

                _ ->
                    []

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
        [ case layout of
            Grid.Tiles shape ->
                viewPlayerGrid bundle track allCars props shadowMesh debris physics width height room (badgesInTiles shape)

            Grid.SingleView ->
                Render.renderRaceViewportMulti bundle track allCars props shadowMesh debris camera width height canvasAttrs
        , if portrait then
            -- Portrait is too narrow for floating badges beside the timer and
            -- the health bar: stack them into the HUD column instead.
            HUD.viewHudOverlayWith
                { belowTopBar = roomBar room multiActions :: firstRowBadges
                , aboveFooter = rosterSlot room
                }
                hudData
                hudActions

          else
            div []
                [ roomHeader room multiActions
                , HUD.viewHudOverlay hudData hudActions
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


{-| Every player's view in one grid that fills the screen (`Grid.gridShape`),
the local player's first. Each tile follows one car from behind.
-}
viewPlayerGrid :
    TrackRenderBundle
    -> GeneratedTrack
    -> List RenderCarSlot
    -> Props
    -> WebGL.Mesh TerrainVertex
    -> List DebrisPiece
    -> PhysicsState
    -> Int
    -> Int
    -> RoomState
    -> (Int -> Bool)
    -> Html msg
viewPlayerGrid bundle track cars props shadowMesh debris physics width height room badgeInTile =
    let
        shape =
            Grid.gridShape (Grid.orientation width height) (List.length room.players)

        tileW =
            max 160 (width // shape.cols)

        tileH =
            max 120 (height // shape.rows)

        tile index p =
            let
                ( yaw, pos ) =
                    if p.slot == room.mySlot then
                        ( physics.yaw, physics.position )

                    else
                        ( (Vec3.toRecord p.rotation).y, p.position )
            in
            div
                [ Tw.classes [ Tw.relative, Tw.w_full, Tw.h_full, Tw.overflow_hidden, Tw.border ]
                , Attr.style "border-color" "rgba(148, 163, 184, 0.2)"
                , Attr.style "background" "#020617"
                ]
                [ Render.renderRaceViewportMulti bundle track cars props shadowMesh debris (tileCamera tileH pos yaw) tileW tileH []
                , if badgeInTile index then
                    tileBadge overTile room p

                  else
                    text ""
                ]
    in
    div
        [ Tw.classes [ Tw.absolute, Tw.w_full, Tw.h_full ]
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" ("repeat(" ++ String.fromInt shape.cols ++ ", minmax(0, 1fr))")
        , Attr.style "grid-template-rows" ("repeat(" ++ String.fromInt shape.rows ++ ", minmax(0, 1fr))")
        ]
        (List.indexedMap tile (Grid.selfFirst .slot room.mySlot room.players))


{-| Chase camera for one tile: behind the car at `target`, facing `yaw`.
-}
tileCamera : Int -> Vec3.Vec3 -> Float -> Camera
tileCamera tileH target yaw =
    Camera.init
        |> Camera.setViewportHeight (toFloat (max 1 tileH))
        |> Camera.setSceneRadius 450.0
        |> (\c ->
                { c
                    | azimuth = yaw + pi
                    , elevation = 0.28
                    , distance = 5.0
                    , target = target
                }
           )


{-| Tile badge in the top-left corner of its tile.
-}
overTile : List (Html.Attribute msg)
overTile =
    [ Tw.classes [ Tw.absolute, Tw.z_20 ]
    , Attr.style "top" "0.5rem"
    , Attr.style "left" "0.5rem"
    ]


{-| Tile badge in the HUD column: as wide as its grid column at most, the
name cut short rather than running into the next badge.
-}
inFlowBadge : List (Html.Attribute msg)
inFlowBadge =
    [ Tw.classes [ Tw.w_fit, Tw.max_w_full, Tw.overflow_hidden, Tw.whitespace_nowrap ] ]


{-| Name, colour and speed of a player, on their tile. `placement` positions it.
-}
tileBadge : List (Html.Attribute msg) -> RoomState -> RemotePlayer -> Html msg
tileBadge placement room p =
    let
        isMe =
            p.slot == room.mySlot
    in
    div
        (placement
            ++ [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.border ]
               , Attr.style "background" "rgba(15, 23, 42, 0.85)"
               , Attr.style "border-color" (Multiplayer.slotColor p.slot)
               ]
        )
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
        , Attr.style "background" "#0f172a"
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
