module Racer.HUD exposing
    ( HudActions
    , HudAlert
    , HudSlots
    , HudViewData
    , noSlots
    , touchPadsVisible
    , viewHudOverlay
    , viewHudOverlayWith
    )

{-| Pointer-friendly Tailwind gameplay HUD overlay: timer, checkpoint splits, course progress,
speedometer, surface badge, integrity bar, pause & recovery modals, and touch/tilt pads.
-}

import FeatherIcons
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import I18n exposing (Language)
import Json.Decode as Decode
import Math.Vector3 as Vec3
import Racer.I18n as RI18n
import Racer.Leaderboard as Leaderboard
import Racer.Types exposing (CameraMode(..), CrashReason(..), PhysicsState)
import Tailwind as Tw
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Th
import UI.Button as Button
import UI.Theme as Theme


{-| Transient celebratory or split banner centered near upper HUD.
-}
type alias HudAlert =
    { title : String
    , subtitle : Maybe String
    , positive : Bool
    , timer : Float
    }


{-| Read-only HUD telemetry snapshot rendered over the 3D race canvas.
-}
type alias HudViewData =
    { language : Language
    , physics : PhysicsState
    , trackLengthM : Float
    , trackTotalDropM : Float
    , elapsedTime : Float
    , countdown : Maybe Float
    , isPaused : Bool
    , crashRecovery : Maybe { timer : Float, reason : CrashReason }
    , audioMuted : Bool
    , cameraMode : CameraMode
    , splitDelta : Maybe Float
    , hudAlert : Maybe HudAlert
    , showTouchControls : Bool
    , tiltEnabled : Bool
    , tiltAvailable : Bool
    , touchLeftHeld : Bool
    , touchRightHeld : Bool
    , touchBrakeHeld : Bool
    , touchTuckHeld : Bool
    , touchDriftHeld : Bool
    }


{-| Interactive callbacks triggered from HUD buttons and mobile touch controls.
-}
type alias HudActions msg =
    { onTogglePause : msg
    , onResume : msg
    , onRestartRace : msg
    , onGoGarage : msg
    , onGoMenu : msg
    , onRespawnCheckpoint : msg
    , onCycleCamera : msg
    , onToggleAudio : msg
    , onToggleTouchUi : msg
    , onTouchSteerLeft : Bool -> msg
    , onTouchSteerRight : Bool -> msg
    , onTouchBrake : Bool -> msg
    , onTouchTuck : Bool -> msg
    , onTouchDrift : Bool -> msg
    }


{-| Extra controls laid out in the HUD's own column instead of floating over
it: `belowTopBar` sits under the timer and toolbar, `aboveFooter` above the
health bar and speedometer. Portrait screens use them for room and player
badges, which would otherwise land on top of the timer or the health bar.
-}
type alias HudSlots msg =
    { belowTopBar : List (Html msg)
    , aboveFooter : List (Html msg)
    }


{-| No extra controls.
-}
noSlots : HudSlots msg
noSlots =
    { belowTopBar = [], aboveFooter = [] }


{-| Render the full HUD, overlays, and responsive mobile controls above the race canvas.
-}
viewHudOverlay : HudViewData -> HudActions msg -> Html msg
viewHudOverlay =
    viewHudOverlayWith noSlots


{-| `viewHudOverlay` with extra controls stacked into the HUD (`HudSlots`).
-}
viewHudOverlayWith : HudSlots msg -> HudViewData -> HudActions msg -> Html msg
viewHudOverlayWith slots data actions =
    div
        [ Tw.classes
            [ Tw.absolute
            , Tw.pointer_events_none
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_between
            , Tw.select_none
            ]
        , Attr.style "inset" "0"

        -- Keep the controls clear of the notch and home indicator (the page
        -- itself runs edge to edge with viewport-fit=cover).
        , Attr.style "padding" "max(12px, env(safe-area-inset-top)) max(12px, env(safe-area-inset-right)) max(12px, env(safe-area-inset-bottom)) max(12px, env(safe-area-inset-left))"

        -- A tap on any HUD control may ask for the iOS tilt permission
        -- (tilt-steer.js) when tilt is on but not yet allowed.
        , Attr.attribute "data-tilt-start" ""
        ]
        [ viewTopArea slots.belowTopBar data actions
        , viewCenterOverlays data actions
        , viewBottomArea slots.aboveFooter data actions
        ]


viewTopArea : List (Html msg) -> HudViewData -> HudActions msg -> Html msg
viewTopArea belowTopBar data actions =
    case belowTopBar of
        [] ->
            viewTopBar data actions

        _ ->
            div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s2, Tw.w_full ] ]
                (viewTopBar data actions :: belowTopBar)


viewTopBar : HudViewData -> HudActions msg -> Html msg
viewTopBar data actions =
    let
        lang =
            data.language

        progressRatio =
            clamp 0.0 1.0 (data.physics.progressS / max 1.0 data.trackLengthM)

        progressPct =
            round (progressRatio * 100.0)

        dropRemaining =
            max 0 (round (data.trackTotalDropM * (1.0 - progressRatio)))

        cpTotal =
            4

        cpPassed =
            min cpTotal data.physics.checkpointIndex
    in
    div
        [ Tw.classes [ Tw.flex, Tw.items_start, Tw.justify_between, Tw.gap Th.s3, Tw.w_full ]
        ]
        [ -- Timer & Checkpoints
          div
            [ Tw.classes [ Tw.pointer_events_auto, Tw.rounded_xl, Tw.px Th.s3, Tw.py Th.s2, Tw.border ]
            , Attr.style "background" "rgba(15, 23, 42, 0.78)"
            , Attr.style "border-color" "rgba(148, 163, 184, 0.25)"
            , Attr.style "backdrop-filter" "blur(8px)"
            ]
            [ div
                [ Tw.classes [ Tw.text_n2xl, Tw.font_bold, Tw.tabular_nums, Tw.text_simple Th.white ]
                ]
                [ text (Leaderboard.formatRaceTime data.elapsedTime) ]
            , div
                [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.text_xs, Tw.mt Th.s0_dot_5 ]
                , Attr.style "color" "#cbd5e1"
                ]
                [ span [] [ text (String.fromInt cpPassed ++ "/" ++ String.fromInt cpTotal) ]
                , case data.splitDelta of
                    Nothing ->
                        text ""

                    Just d ->
                        span
                            [ Tw.classes [ Tw.font_semibold, Tw.tabular_nums ]
                            , Attr.style "color"
                                (if d <= 0.0 then
                                    "#4ade80"

                                 else
                                    "#fbbf24"
                                )
                            ]
                            [ text (Leaderboard.formatSplitDelta d) ]
                ]
            ]

        -- Center Course Progress
        , div
            [ Tw.classes [ Tw.hidden, Bp.sm [ Tw.flex ], Tw.flex_col, Tw.items_center, Tw.flex_1, Tw.rounded_xl, Tw.px Th.s4, Tw.py Th.s2, Tw.border ]
            , Attr.style "max-width" "26rem"
            , Attr.style "background" "rgba(15, 23, 42, 0.72)"
            , Attr.style "border-color" "rgba(148, 163, 184, 0.22)"
            , Attr.style "backdrop-filter" "blur(8px)"
            ]
            [ div [ Tw.classes [ Tw.flex, Tw.justify_between, Tw.w_full, Tw.text_xs, Tw.mb Th.s1 ], Attr.style "color" "#e2e8f0" ]
                [ span [ Tw.classes [ Tw.font_semibold ] ] [ text (String.fromInt progressPct ++ "%") ]
                , span [] [ text (RI18n.elevationRemainingLabel lang dropRemaining) ]
                ]
            , div
                [ Tw.classes [ Tw.w_full, Tw.h Th.s2, Tw.rounded_full, Tw.overflow_hidden ]
                , Attr.style "background" "rgba(255,255,255,0.14)"
                ]
                [ div
                    [ Tw.classes [ Tw.h_full, Tw.rounded_full ]
                    , Attr.style "width" (String.fromInt progressPct ++ "%")
                    , Attr.style "background" "linear-gradient(90deg, #38bdf8, #4ade80)"
                    ]
                    []
                ]
            ]

        -- Right Toolbar
        , div
            [ Tw.classes [ Tw.pointer_events_auto, Tw.flex, Tw.items_center, Tw.gap Th.s1_dot_5, Tw.flex_wrap, Tw.justify_end ]
            ]
            [ hudToolbarBtn actions.onCycleCamera (cameraIconName data.cameraMode) (cameraLabel lang data.cameraMode)
            , hudToolbarBtn actions.onToggleAudio
                (if data.audioMuted then
                    FeatherIcons.volumeX

                 else
                    FeatherIcons.volume2
                )
                (if data.audioMuted then
                    RI18n.audioMutedLabel lang

                 else
                    RI18n.audioUnmutedLabel lang
                )
            , hudToolbarBtn actions.onRespawnCheckpoint FeatherIcons.rotateCcw (RI18n.resetCheckpointLabel lang)
            , if tiltSteering data then
                text ""

              else
                hudToolbarBtn actions.onToggleTouchUi FeatherIcons.grid "Touch"
            , viewTiltToggle data
            , hudToolbarBtn actions.onTogglePause FeatherIcons.pause (RI18n.pauseLabel lang)
            ]
        ]


{-| Whether the phone is steering by tilt right now.
-}
tiltSteering : HudViewData -> Bool
tiltSteering data =
    data.tiltEnabled && data.tiltAvailable


{-| Whether the on-screen steering and brake pads are shown: only while the
player wants touch controls and is not steering by tilt, so tilt play keeps the
screen clear.
-}
touchPadsVisible : { a | showTouchControls : Bool, tiltEnabled : Bool, tiltAvailable : Bool } -> Bool
touchPadsVisible data =
    data.showTouchControls && not (data.tiltEnabled && data.tiltAvailable)


{-| Small icon-only toolbar toggle for tilt steering (green while on), shown
only on devices that can tilt. The tap is handled in `tilt-steer.js` (`data-tilt-toggle`) so the iOS
permission prompt runs inside the tap itself.
-}
viewTiltToggle : HudViewData -> Html msg
viewTiltToggle data =
    if data.tiltAvailable then
        let
            label =
                if data.tiltEnabled then
                    RI18n.tiltEnabledLabel data.language

                else
                    RI18n.tiltDisabledLabel data.language
        in
        button
            [ Attr.type_ "button"
            , Attr.attribute "data-tilt-toggle" ""
            , Attr.attribute "aria-pressed"
                (if data.tiltEnabled then
                    "true"

                 else
                    "false"
                )
            , Attr.attribute "aria-label" label
            , Attr.title label
            , Tw.classes
                [ Tw.flex
                , Tw.items_center
                , Tw.gap Th.s1
                , Tw.px Th.s2_dot_5
                , Tw.py Th.s1_dot_5
                , Tw.rounded_lg
                , Tw.border
                , Tw.text_xs
                , Tw.font_semibold
                , Tw.text_simple Th.white
                ]
            , Attr.style "cursor" "pointer"
            , Attr.style "background"
                (if data.tiltEnabled then
                    "rgba(22, 163, 74, 0.9)"

                 else
                    "rgba(15, 23, 42, 0.78)"
                )
            , Attr.style "border-color" "rgba(148, 163, 184, 0.28)"
            ]
            [ FeatherIcons.smartphone |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml [] ]

    else
        text ""


hudToolbarBtn : msg -> FeatherIcons.Icon -> String -> Html msg
hudToolbarBtn clickMsg ic labelText =
    button
        [ Attr.type_ "button"
        , onClick clickMsg
        , Attr.title labelText
        , Tw.classes
            [ Tw.flex
            , Tw.items_center
            , Tw.gap Th.s1
            , Tw.px Th.s2_dot_5
            , Tw.py Th.s1_dot_5
            , Tw.rounded_lg
            , Tw.border
            , Tw.text_xs
            , Tw.font_semibold
            , Tw.text_simple Th.white
            ]
        , Attr.style "cursor" "pointer"
        , Attr.style "background" "rgba(15, 23, 42, 0.78)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.28)"
        ]
        [ ic |> FeatherIcons.withSize 15 |> FeatherIcons.toHtml []
        , span [ Tw.classes [ Tw.hidden, Bp.md [ Tw.inline ] ] ] [ text labelText ]
        ]


cameraIconName : CameraMode -> FeatherIcons.Icon
cameraIconName mode =
    case mode of
        ChaseCam ->
            FeatherIcons.video

        HoodCam ->
            FeatherIcons.eye

        OrbitCam ->
            FeatherIcons.compass


cameraLabel : Language -> CameraMode -> String
cameraLabel lang mode =
    case mode of
        ChaseCam ->
            RI18n.camChaseLabel lang

        HoodCam ->
            RI18n.camHoodLabel lang

        OrbitCam ->
            RI18n.camOrbitLabel lang


viewCenterOverlays : HudViewData -> HudActions msg -> Html msg
viewCenterOverlays data actions =
    if data.isPaused then
        viewPauseModal data.language actions

    else
        case data.crashRecovery of
            Just rec ->
                viewCrashOverlay data.language rec actions

            Nothing ->
                case data.countdown of
                    Just c ->
                        let
                            bannerText =
                                if c <= 0.15 then
                                    RI18n.goBanner data.language

                                else
                                    String.fromInt (max 1 (ceiling c))
                        in
                        div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.justify_center, Tw.my_auto ] ]
                            [ div
                                [ Tw.classes [ Tw.text_n6xl, Tw.font_extrabold, Tw.px Th.s8, Tw.py Th.s4, Tw.rounded_n2xl, Tw.border ]
                                , Attr.style "background" "rgba(15, 23, 42, 0.84)"
                                , Attr.style "border-color" "#facc15"
                                , Attr.style "color" "#facc15"
                                ]
                                [ text bannerText ]
                            ]

                    Nothing ->
                        case data.hudAlert of
                            Just alert ->
                                div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.mt Th.s2 ] ]
                                    [ div
                                        [ Tw.classes [ Tw.px Th.s5, Tw.py Th.s2, Tw.rounded_xl, Tw.border, Tw.text_center ]
                                        , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                                        , Attr.style "border-color"
                                            (if alert.positive then
                                                "#4ade80"

                                             else
                                                "#f59e0b"
                                            )
                                        ]
                                        [ div [ Tw.classes [ Tw.text_lg, Tw.font_bold, Tw.text_simple Th.white ] ] [ text alert.title ]
                                        , case alert.subtitle of
                                            Just s ->
                                                div [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#cbd5e1" ] [ text s ]

                                            Nothing ->
                                                text ""
                                        ]
                                    ]

                            Nothing ->
                                text ""


viewPauseModal : Language -> HudActions msg -> Html msg
viewPauseModal lang actions =
    div
        [ Tw.classes [ Tw.pointer_events_auto, Tw.flex, Tw.items_center, Tw.justify_center, Tw.my_auto ]
        ]
        [ div
            [ Tw.classes [ Tw.rounded_n2xl, Tw.p Th.s6, Tw.border, Tw.w_full, Tw.flex, Tw.flex_col, Tw.gap Th.s3 ]
            , Attr.style "max-width" "24rem"
            , Attr.style "background" Theme.panelBackground
            , Attr.style "border-color" Theme.borderDefault
            , Attr.style "box-shadow" "0 20px 50px rgba(0,0,0,0.45)"
            ]
            [ div [ Tw.classes [ Tw.text_xl, Tw.font_bold, Tw.text_center ], Attr.style "color" Theme.textPrimary ]
                [ text (RI18n.pausedHeadline lang) ]
            , Button.primary actions.onResume (RI18n.resumeButton lang)
            , Button.secondary actions.onRestartRace (RI18n.raceAgainButton lang)
            , Button.secondary actions.onGoGarage (RI18n.inspectCarButton lang)
            , Button.secondary actions.onGoMenu (RI18n.backToMenuLabel lang)
            , div [ Tw.classes [ Tw.text_xs, Tw.text_center, Tw.pt Th.s2 ], Attr.style "color" Theme.textMuted ]
                [ text (RI18n.controlsSummary lang) ]
            ]
        ]


viewCrashOverlay : Language -> { timer : Float, reason : CrashReason } -> HudActions msg -> Html msg
viewCrashOverlay lang rec actions =
    div
        [ Tw.classes [ Tw.pointer_events_auto, Tw.flex, Tw.items_center, Tw.justify_center, Tw.my_auto ]
        ]
        [ div
            [ Tw.classes [ Tw.rounded_n2xl, Tw.p Th.s6, Tw.border, Tw.w_full, Tw.text_center, Tw.flex, Tw.flex_col, Tw.gap Th.s3 ]
            , Attr.style "max-width" "28rem"
            , Attr.style "background" "rgba(15, 23, 42, 0.9)"
            , Attr.style "border-color" "#ef4444"
            ]
            [ div [ Tw.classes [ Tw.text_n2xl, Tw.font_extrabold ], Attr.style "color" "#f87171" ]
                [ text (RI18n.crashReasonText lang rec.reason) ]
            , div [ Tw.classes [ Tw.text_sm ], Attr.style "color" "#e2e8f0" ]
                [ text
                    ((if rec.reason == StructuralCollapse then
                        RI18n.wreckedHint lang

                      else
                        RI18n.recoveryHint lang
                     )
                        ++ " ("
                        ++ String.fromFloat (toFloat (round (max 0 rec.timer * 10)) / 10.0)
                        ++ "s)"
                    )
                ]
            , div [ Tw.classes [ Tw.flex, Tw.justify_center ] ]
                [ Button.primaryOnDark actions.onRespawnCheckpoint (RI18n.resetCheckpointLabel lang) ]
            ]
        ]


viewBottomArea : List (Html msg) -> HudViewData -> HudActions msg -> Html msg
viewBottomArea aboveFooter data actions =
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s2, Tw.w_full ] ]
        ((if touchPadsVisible data then
            viewTouchControls data actions

          else
            text ""
         )
            :: aboveFooter
            ++ [ viewTelemetryFooter data ]
        )


viewTelemetryFooter : HudViewData -> Html msg
viewTelemetryFooter data =
    let
        lang =
            data.language

        speedKmh =
            round (Vec3.length data.physics.velocity * 3.6)

        gradePct =
            round (data.physics.gradeRatio * 100.0)

        integrityPct =
            healthPercent data.physics.integrity

        integrityColor =
            healthColor integrityPct

        surfaceBadge =
            if data.physics.groundedWheels == 0 then
                "AIRBORNE!"

            else
                RI18n.surfaceName lang data.physics.surfaceUnderCar
    in
    div
        [ Tw.classes [ Tw.flex, Tw.items_end, Tw.justify_between, Tw.gap Th.s3, Tw.w_full ]
        ]
        [ -- Bottom-Left: Integrity + Hazards
          div
            [ Tw.classes [ Tw.rounded_xl, Tw.px Th.s3_dot_5, Tw.py Th.s2_dot_5, Tw.border, Tw.min_w Th.s44 ]
            , Attr.style "background" "rgba(15, 23, 42, 0.78)"
            , Attr.style "border-color" "rgba(148, 163, 184, 0.25)"
            ]
            [ div [ Tw.classes [ Tw.flex, Tw.justify_between, Tw.text_xs, Tw.mb Th.s1 ], Attr.style "color" "#e2e8f0" ]
                [ span [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s1, Tw.font_semibold ] ]
                    [ FeatherIcons.heart |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [ Attr.style "color" integrityColor ]
                    , text (RI18n.healthLabel lang)
                    ]
                , span [ Tw.classes [ Tw.tabular_nums, Tw.font_bold ], Attr.style "color" integrityColor ] [ text (String.fromInt integrityPct ++ "%") ]
                ]
            , div
                [ Tw.classes [ Tw.w_full, Tw.h Th.s2, Tw.rounded_full, Tw.overflow_hidden ]
                , Attr.style "background" "rgba(255,255,255,0.16)"
                ]
                [ div
                    [ Tw.classes [ Tw.h_full, Tw.rounded_full ]
                    , Attr.style "width" (String.fromInt integrityPct ++ "%")
                    , Attr.style "background" integrityColor
                    ]
                    []
                ]
            , div [ Tw.classes [ Tw.flex, Tw.gap Th.s1_dot_5, Tw.mt Th.s1_dot_5, Tw.flex_wrap ] ]
                [ if data.physics.chassisScraping then
                    hazardChip "#f59e0b" (RI18n.warningScraping lang)

                  else
                    text ""
                , if abs data.physics.roll > 0.72 then
                    hazardChip "#ef4444" (RI18n.warningTipping lang)

                  else
                    text ""
                , if data.physics.offTrack then
                    hazardChip "#fb923c" (RI18n.warningOffTrack lang)

                  else
                    text ""
                ]
            ]

        -- Bottom-Right: Digital Speedometer + Surface
        , div
            [ Tw.classes [ Tw.rounded_xl, Tw.px Th.s4, Tw.py Th.s2_dot_5, Tw.border, Tw.text_right ]
            , Attr.style "background" "rgba(15, 23, 42, 0.8)"
            , Attr.style "border-color" "rgba(148, 163, 184, 0.28)"
            ]
            [ div [ Tw.classes [ Tw.flex, Tw.items_baseline, Tw.justify_end, Tw.gap Th.s1 ] ]
                [ span
                    [ Tw.classes [ Tw.text_n3xl, Tw.font_extrabold, Tw.tabular_nums, Tw.text_simple Th.white ]
                    ]
                    [ text (String.fromInt speedKmh) ]
                , span [ Tw.classes [ Tw.text_xs, Tw.font_semibold ], Attr.style "color" "#94a3b8" ] [ text "km/h" ]
                ]
            , div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_end, Tw.gap Th.s2, Tw.text_xs, Tw.mt Th.s0_dot_5 ] ]
                [ span
                    [ Tw.classes [ Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded_sm, Tw.font_semibold ]
                    , Attr.style "background" "rgba(56, 189, 248, 0.2)"
                    , Attr.style "color" "#7dd3fc"
                    ]
                    [ text surfaceBadge ]
                , span [ Tw.classes [ Tw.tabular_nums ], Attr.style "color" "#cbd5e1" ]
                    [ text ("-" ++ String.fromInt gradePct ++ "%") ]
                ]
            ]
        ]


{-| Car health as a whole percentage, 0 .. 100.
-}
healthPercent : Float -> Int
healthPercent integrity =
    clamp 0 100 (round integrity)


{-| Health bar colour: green while healthy, amber when worn, red when one or
two more hard crashes will break the car apart.
-}
healthColor : Int -> String
healthColor pct =
    if pct > 65 then
        "#22c55e"

    else if pct > 30 then
        "#f59e0b"

    else
        "#ef4444"


hazardChip : String -> String -> Html msg
hazardChip col labelStr =
    span
        [ Tw.classes [ Tw.text_xs, Tw.font_bold, Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded_sm ]
        , Attr.style "background" col
        , Attr.style "color" "#0f172a"
        ]
        [ text labelStr ]


viewTouchControls : HudViewData -> HudActions msg -> Html msg
viewTouchControls data actions =
    let
        lang =
            data.language

        padStyle held =
            [ Tw.classes
                [ Tw.pointer_events_auto
                , Tw.px Th.s5
                , Tw.py Th.s4
                , Tw.rounded_n2xl
                , Tw.border
                , Tw.font_bold
                , Tw.text_base
                , Tw.text_simple Th.white
                ]
            , Attr.style "background"
                (if held then
                    "rgba(56, 189, 248, 0.72)"

                 else
                    "rgba(15, 23, 42, 0.74)"
                )
            , Attr.style "border-color" "rgba(148, 163, 184, 0.4)"
            , Attr.style "touch-action" "none"
            ]

        holdEvents setter =
            [ Html.Events.preventDefaultOn "pointerdown" (Decode.succeed ( setter True, True ))
            , Html.Events.on "pointerup" (Decode.succeed (setter False))
            , Html.Events.on "pointercancel" (Decode.succeed (setter False))
            , Html.Events.on "pointerleave" (Decode.succeed (setter False))
            ]
    in
    div [ Tw.classes [ Tw.flex, Tw.items_end, Tw.justify_between, Tw.w_full, Tw.mb Th.s1 ] ]
        [ div [ Tw.classes [ Tw.flex, Tw.gap Th.s2 ] ]
            [ button (Attr.type_ "button" :: holdEvents actions.onTouchSteerLeft ++ padStyle data.touchLeftHeld) [ text "◀" ]
            , button (Attr.type_ "button" :: holdEvents actions.onTouchSteerRight ++ padStyle data.touchRightHeld) [ text "▶" ]
            ]
        , div [ Tw.classes [ Tw.flex, Tw.gap Th.s2, Tw.items_center ] ]
            [ button (Attr.type_ "button" :: holdEvents actions.onTouchDrift ++ padStyle data.touchDriftHeld) [ text (RI18n.handbrakeHint lang) ]
            , button (Attr.type_ "button" :: holdEvents actions.onTouchTuck ++ padStyle data.touchTuckHeld) [ text "▲" ]
            , button (Attr.type_ "button" :: holdEvents actions.onTouchBrake ++ padStyle data.touchBrakeHeld) [ text "■" ]
            ]
        ]
