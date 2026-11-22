module Pages.Garage exposing
    ( GarageActions
    , GarageViewData
    , view
    )

{-| Garage inspection & mechanical validation screen (`LoadGaragePage`).

Displays a 3D turntable preview of the loaded brick soapbox build, reports async
part resolution progress, lists mechanical specs (wheels, steering type, weight,
ground clearance) and diagnostic findings, and launches into the selected course.

-}

import FeatherIcons
import Html exposing (Html, button, div, h1, h2, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import I18n exposing (Language)
import Json.Decode as Decode
import Racer.Analyze exposing (CarMeshBundle)
import Racer.I18n as RI18n
import Racer.Render as Render
import Racer.TerrainShader exposing (TerrainVertex)
import Racer.Types exposing (RaceMode(..), TrackId, ValidationIssue, ValidationSeverity(..), VehicleSpec, allTrackIds)
import Render.Camera exposing (Camera)
import Tailwind as Tw
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Th
import UI.Button as Button
import WebGL


{-| View-model snapshot for rendering the Garage inspector.
-}
type alias GarageViewData =
    { language : Language
    , vehicle : VehicleSpec
    , meshes : CarMeshBundle
    , camera : Camera
    , plinthMesh : WebGL.Mesh TerrainVertex
    , shadowMesh : WebGL.Mesh TerrainVertex
    , wheelPreviewSpin : Float
    , partsLoaded : Int
    , partsTotal : Int
    , selectedTrack : TrackId
    , seed : Int
    , viewportWidth : Int
    , viewportHeight : Int
    , shareCopiedNotice : Bool
    , raceMode : RaceMode
    }


{-| Actions dispatched by Garage controls and viewport mouse/wheel events.
-}
type alias GarageActions msg =
    { onBackToMenu : msg
    , onStartRace : msg
    , onStartHostMultiplayer : msg
    , onJoinMultiplayer : msg
    , onSelectTrack : TrackId -> msg
    , onCopyShareLink : msg
    , onOpenQrScanner : msg
    , onToggleRaceMode : msg
    , onMouseDown : Float -> Float -> msg
    , onMouseMove : Float -> Float -> msg
    , onMouseUp : msg
    , onWheel : Float -> msg
    }


{-| Render the full Garage inspection view.
-}
view : GarageViewData -> GarageActions msg -> Html msg
view data actions =
    let
        lang =
            data.language

        canvasWidth =
            if data.viewportWidth >= 1024 then
                max 320 (data.viewportWidth - 400)

            else
                max 320 data.viewportWidth

        canvasHeight =
            if data.viewportWidth >= 1024 then
                max 360 data.viewportHeight

            else
                max 280 (round (toFloat data.viewportHeight * 0.45))

        mouseDecoder msgCtor =
            Decode.map2 msgCtor
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)

        wheelDecoder =
            Decode.field "deltaY" Decode.float
                |> Decode.map (\dy -> ( actions.onWheel dy, True ))

        canvasEvents =
            [ Html.Events.on "mousedown" (mouseDecoder actions.onMouseDown)
            , Html.Events.on "mousemove" (mouseDecoder actions.onMouseMove)
            , Html.Events.on "mouseup" (Decode.succeed actions.onMouseUp)
            , Html.Events.on "mouseleave" (Decode.succeed actions.onMouseUp)
            , Html.Events.preventDefaultOn "wheel" wheelDecoder
            ]
    in
    div
        [ Tw.classes [ Tw.min_h_screen, Bp.lg [ Tw.h_screen ], Tw.w_full, Tw.flex, Tw.flex_col, Bp.lg [ Tw.flex_row ], Tw.overflow_hidden ]
        , Attr.style "background" "#0f172a"
        , Attr.style "color" "#f8fafc"
        ]
        [ -- 3D Inspection Viewport
          div
            [ Tw.classes [ Tw.relative, Tw.flex_1 ]
            , Attr.style "min-height" "260px"
            ]
            [ Render.renderGarageViewport
                data.selectedTrack
                data.plinthMesh
                data.shadowMesh
                data.wheelPreviewSpin
                data.vehicle
                data.meshes
                data.camera
                canvasWidth
                canvasHeight
                canvasEvents
            , div
                [ Tw.classes [ Tw.absolute, Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.pointer_events_none ]
                , Attr.style "top" "14px"
                , Attr.style "left" "14px"
                ]
                [ span
                    [ Tw.classes [ Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_xl, Tw.text_xs, Tw.font_semibold, Tw.border ]
                    , Attr.style "background" "rgba(15, 23, 42, 0.78)"
                    , Attr.style "border-color" "rgba(148, 163, 184, 0.3)"
                    ]
                    [ text (RI18n.garageInspectTitle lang) ]
                , if data.partsTotal > 0 && data.partsLoaded < data.partsTotal then
                    span
                        [ Tw.classes [ Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_xl, Tw.text_xs, Tw.font_semibold ]
                        , Attr.style "background" "rgba(2, 132, 199, 0.88)"
                        ]
                        [ text (RI18n.garageResolveProgress lang data.partsLoaded data.partsTotal) ]

                  else
                    text ""
                ]
            ]

        -- Right Sidebar Diagnostics & Launch
        , div
            [ Tw.classes [ Tw.w_full, Tw.p Th.s5, Tw.flex, Tw.flex_col, Tw.gap Th.s4, Tw.overflow_y_auto, Tw.border ]
            , Attr.style "max-width" "26rem"
            , Attr.style "background" "#0f172a"
            , Attr.style "border-color" "rgba(148, 163, 184, 0.22)"
            ]
            [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.gap Th.s2 ] ]
                [ Button.secondary actions.onBackToMenu (RI18n.backToMenuLabel lang)
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onOpenQrScanner
                    , Attr.title "Scan Brick Play QR code with camera"
                    , Tw.classes [ Tw.px Th.s3, Tw.py Th.s2, Tw.rounded_xl, Tw.font_bold, Tw.text_xs, Tw.border ]
                    , Attr.style "cursor" "pointer"
                    , Attr.style "background" "rgba(14, 165, 233, 0.22)"
                    , Attr.style "border-color" "#38bdf8"
                    , Attr.style "color" "#7dd3fc"
                    ]
                    [ text "📷 Scan QR" ]
                , Button.secondary actions.onCopyShareLink (RI18n.shareRaceUrlButton lang)
                ]
            , if data.shareCopiedNotice then
                div
                    [ Tw.classes [ Tw.text_xs, Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_lg ]
                    , Attr.style "background" "rgba(34, 197, 94, 0.2)"
                    , Attr.style "color" "#86efac"
                    ]
                    [ text (RI18n.shareUrlCopiedNotice lang) ]

              else
                text ""
            , div []
                [ h1 [ Tw.classes [ Tw.text_n2xl, Tw.font_extrabold ] ] [ text data.vehicle.name ]
                , p [ Tw.classes [ Tw.text_xs, Tw.mt Th.s0_dot_5 ], Attr.style "color" "#94a3b8" ]
                    [ text (String.fromInt data.vehicle.placementCount ++ " parts • Seed #" ++ String.fromInt data.seed) ]
                ]

            -- Quick course switcher
            , div [ Tw.classes [ Tw.flex, Tw.gap Th.s1_dot_5, Tw.flex_wrap ] ]
                (List.map (trackChip lang data.selectedTrack actions.onSelectTrack) allTrackIds)

            -- Specs Grid
            , viewSpecsGrid data

            -- Validation checklist
            , div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s2 ] ]
                (if List.isEmpty data.vehicle.issues then
                    [ div
                        [ Tw.classes [ Tw.p Th.s3, Tw.rounded_xl, Tw.text_xs, Tw.font_bold ]
                        , Attr.style "background" "rgba(34, 197, 94, 0.16)"
                        , Attr.style "color" "#86efac"
                        ]
                        [ text (RI18n.garageReadyToRoll lang) ]
                    ]

                 else
                    List.map (viewIssueCard lang) data.vehicle.issues
                )

            -- Primary Launch CTA
            , div [ Tw.classes [ Tw.mt Th.s2, Tw.flex, Tw.flex_col, Tw.gap Th.s2 ] ]
                (if data.vehicle.isDrivable then
                    let
                        hasWarnings =
                            List.any (\i -> i.severity == Warning) data.vehicle.issues

                        ctaLabel =
                            if hasWarnings then
                                RI18n.startRaceButton lang ++ " (" ++ RI18n.raceAnywayButton lang ++ ")"

                            else
                                RI18n.startRaceButton lang
                    in
                    [ -- Online Multiplayer Controls
                      div
                        [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5, Tw.p Th.s2_dot_5, Tw.rounded_xl, Tw.border ]
                        , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                        , Attr.style "border-color" "rgba(56, 189, 248, 0.25)"
                        ]
                        [ span [ Tw.classes [ Tw.text_xs, Tw.font_extrabold, Tw.uppercase ], Attr.style "color" "#38bdf8" ]
                            [ text "Online Multiplayer" ]
                        , div [ Tw.classes [ Tw.flex, Tw.gap Th.s2 ] ]
                            [ button
                                [ Attr.type_ "button"
                                , onClick actions.onStartHostMultiplayer
                                , Tw.classes [ Tw.flex_1, Tw.py Th.s2, Tw.px Th.s2_dot_5, Tw.rounded_lg, Tw.font_bold, Tw.text_xs, Tw.border ]
                                , Attr.style "cursor" "pointer"
                                , Attr.style "background" "rgba(14, 165, 233, 0.18)"
                                , Attr.style "border-color" "#38bdf8"
                                , Attr.style "color" "#7dd3fc"
                                ]
                                [ text "Host (4-Split)" ]
                            , button
                                [ Attr.type_ "button"
                                , onClick actions.onJoinMultiplayer
                                , Tw.classes [ Tw.flex_1, Tw.py Th.s2, Tw.px Th.s2_dot_5, Tw.rounded_lg, Tw.font_bold, Tw.text_xs, Tw.border ]
                                , Attr.style "cursor" "pointer"
                                , Attr.style "background" "rgba(30, 41, 59, 0.8)"
                                , Attr.style "border-color" "rgba(148, 163, 184, 0.3)"
                                , Attr.style "color" "#e2e8f0"
                                ]
                                [ text "Join Race" ]
                            ]
                        ]
                    , button
                        [ Attr.type_ "button"
                        , onClick actions.onToggleRaceMode
                        , Tw.classes [ Tw.w_full, Tw.py Th.s2, Tw.px Th.s4, Tw.rounded_xl, Tw.font_bold, Tw.text_sm, Tw.border ]
                        , Attr.style "cursor" "pointer"
                        , Attr.style "background"
                            (if data.raceMode == SplitScreenTwoPlayer then
                                "rgba(245, 158, 11, 0.25)"

                             else
                                "rgba(30, 41, 59, 0.6)"
                            )
                        , Attr.style "border-color"
                            (if data.raceMode == SplitScreenTwoPlayer then
                                "#f59e0b"

                             else
                                "rgba(148, 163, 184, 0.3)"
                            )
                        , Attr.style "color"
                            (if data.raceMode == SplitScreenTwoPlayer then
                                "#fbbf24"

                             else
                                "#e2e8f0"
                            )
                        ]
                        [ text
                            (if data.raceMode == SplitScreenTwoPlayer then
                                "🏁 MODE: 2P SPLIT SCREEN (Active) [2]"

                             else
                                "🏎️ MODE: 1 PLAYER (Press 2 for Split Screen)"
                            )
                        ]
                    , button
                        [ Attr.type_ "button"
                        , onClick actions.onStartRace
                        , Tw.classes [ Tw.w_full, Tw.py Th.s3_dot_5, Tw.px Th.s4, Tw.rounded_xl, Tw.font_extrabold, Tw.text_base, Tw.text_simple Th.white ]
                        , Attr.style "cursor" "pointer"
                        , Attr.style "background" "linear-gradient(90deg, #16a34a, #22c55e)"
                        ]
                        [ text ctaLabel ]
                    ]

                 else
                    [ button
                        [ Attr.type_ "button"
                        , Attr.disabled True
                        , Tw.classes [ Tw.w_full, Tw.py Th.s3_dot_5, Tw.px Th.s4, Tw.rounded_xl, Tw.font_bold, Tw.text_sm ]
                        , Attr.style "background" "rgba(51, 65, 85, 0.7)"
                        , Attr.style "color" "#94a3b8"
                        ]
                        [ text (RI18n.startRaceButton lang) ]
                    , p [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#fca5a5" ]
                        [ text (RI18n.garageTooFewWheelsHint lang) ]
                    ]
                )
            ]
        ]


trackChip : Language -> TrackId -> (TrackId -> msg) -> TrackId -> Html msg
trackChip lang activeId onSelect id =
    let
        active =
            id == activeId
    in
    button
        [ Attr.type_ "button"
        , onClick (onSelect id)
        , Tw.classes [ Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.border, Tw.text_xs, Tw.font_semibold ]
        , Attr.style "cursor" "pointer"
        , Attr.style "background"
            (if active then
                "rgba(14, 165, 233, 0.25)"

             else
                "rgba(30, 41, 59, 0.8)"
            )
        , Attr.style "border-color"
            (if active then
                "#38bdf8"

             else
                "rgba(148, 163, 184, 0.3)"
            )
        , Attr.style "color" "#f8fafc"
        ]
        [ text (RI18n.trackTitle lang id) ]


viewSpecsGrid : GarageViewData -> Html msg
viewSpecsGrid data =
    let
        lang =
            data.language

        spec =
            data.vehicle

        rubberCount =
            List.filter .hasRubberTire spec.wheels |> List.length

        wheelsVal =
            String.fromInt (List.length spec.wheels)
                ++ " ("
                ++ String.fromInt rubberCount
                ++ " rubber)"

        massVal =
            String.fromInt (round spec.totalMassKg)
                ++ " kg ("
                ++ String.fromFloat (toFloat (round (spec.brickMassGrams * 10)) / 10.0)
                ++ " g)"

        clearanceVal =
            if spec.groundClearanceLdu < -0.5 then
                String.fromInt (round spec.groundClearanceLdu) ++ " LDU (Scrape)"

            else
                "+" ++ String.fromInt (round spec.groundClearanceLdu) ++ " LDU"
    in
    div [ Tw.classes [ Tw.grid, Tw.grid_cols_2, Tw.gap Th.s2_dot_5 ] ]
        [ statTile (RI18n.garageWheelsLabel lang) wheelsVal
        , statTile (RI18n.garageSteeringLabel lang) (RI18n.steeringMechName lang spec.steering)
        , statTile (RI18n.garageMassLabel lang) massVal
        , statTile (RI18n.garageClearanceLabel lang) clearanceVal
        ]


statTile : String -> String -> Html msg
statTile labelText valText =
    div
        [ Tw.classes [ Tw.p Th.s3, Tw.rounded_xl, Tw.border ]
        , Attr.style "background" "rgba(30, 41, 59, 0.72)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.2)"
        ]
        [ div [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ] [ text labelText ]
        , div [ Tw.classes [ Tw.text_sm, Tw.font_bold, Tw.mt Th.s0_dot_5 ] ] [ text valText ]
        ]


viewIssueCard : Language -> ValidationIssue -> Html msg
viewIssueCard lang issue =
    let
        ( bgCol, borderCol, ic ) =
            case issue.severity of
                Blocker ->
                    ( "rgba(239, 68, 68, 0.16)", "#ef4444", FeatherIcons.xCircle )

                Warning ->
                    ( "rgba(245, 158, 11, 0.16)", "#f59e0b", FeatherIcons.alertTriangle )

                Info ->
                    ( "rgba(56, 189, 248, 0.14)", "#38bdf8", FeatherIcons.info )

                Bonus ->
                    ( "rgba(34, 197, 94, 0.16)", "#22c55e", FeatherIcons.checkCircle )
    in
    div
        [ Tw.classes [ Tw.p Th.s3, Tw.rounded_xl, Tw.border, Tw.flex, Tw.items_start, Tw.gap Th.s2 ]
        , Attr.style "background" bgCol
        , Attr.style "border-color" borderCol
        ]
        [ div [ Tw.classes [ Tw.mt Th.s0_dot_5 ] ] [ ic |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]
        , div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s0_dot_5 ] ]
            [ div [ Tw.classes [ Tw.text_xs, Tw.font_bold ] ] [ text (RI18n.validationTitle lang issue.code) ]
            , div [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#cbd5e1" ] [ text issue.detail ]
            ]
        ]
