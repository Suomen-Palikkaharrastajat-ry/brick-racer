module Pages.Race exposing (RaceOrbitActions, SplitScreenData, view)

{-| Live downhill 3D racing viewport composition (`RacePage`).
Supports single-player full-bleed racing and dual-player local split-screen driving.
-}

import Html exposing (Html, div, span, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Racer.Analyze exposing (CarMeshBundle)
import Racer.HUD as HUD exposing (HudActions, HudViewData)
import Racer.Render as Render exposing (RenderCarSlot)
import Racer.TerrainMesh exposing (TrackRenderBundle)
import Racer.TerrainShader exposing (DebrisPiece, TerrainVertex)
import Racer.Types exposing (CameraMode(..), GeneratedTrack, PhysicsState, VehicleSpec)
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
Player 2 split-screen dual viewports.
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
    -> Html msg
view bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions orbitActions maybeSplit =
    case maybeSplit of
        Nothing ->
            viewSinglePlayer bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions orbitActions

        Just p2 ->
            viewTwoPlayerSplitScreen bundle track spec meshes cubeMesh shadowMesh physics debris camera width height hudData hudActions p2


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
