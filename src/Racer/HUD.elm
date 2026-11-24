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

{-| Gameplay HUD overlay: a toolbar with camera and audio, an analog speed dial
with the race counter and metrics under it, the damage skeleton, and the
countdown, crash, exit and fallback touch prompts.
-}

import Char
import DesignTokens.Colors as Brand
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
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Tailwind as Tw
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Th
import UI.Button as Button


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
    { onGoMenu : msg
    , onRespawnCheckpoint : msg
    , onCycleCamera : msg
    , onToggleAudio : msg
    , onTouchSteerLeft : Bool -> msg
    , onTouchSteerRight : Bool -> msg
    , onTouchBrake : Bool -> msg
    , onTouchTuck : Bool -> msg
    , onTouchDrift : Bool -> msg
    }


{-| Extra controls laid out in the HUD's own column instead of floating over
it: `belowTopBar` sits under the toolbar, `aboveFooter` above the damage and
speed panels. Portrait screens use them for room and player badges, which
would otherwise land on top of the HUD.
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
        ]
        [ viewTopArea slots.belowTopBar data actions
        , viewCenterOverlays data actions
        , viewBottomArea slots.aboveFooter data actions
        ]


viewTopArea : List (Html msg) -> HudViewData -> HudActions msg -> Html msg
viewTopArea belowTopBar data actions =
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_end, Tw.gap Th.s2 ] ]
        (viewToolbar data actions :: belowTopBar)


{-| Top-right toolbar: camera mode (labelled) and an icon-only audio toggle.
-}
viewToolbar : HudViewData -> HudActions msg -> Html msg
viewToolbar data actions =
    let
        lang =
            data.language

        audioLabel =
            if data.audioMuted then
                RI18n.audioMutedLabel lang

            else
                RI18n.audioUnmutedLabel lang
    in
    div [ Tw.classes [ Tw.pointer_events_auto, Tw.flex, Tw.items_center, Tw.gap Th.s1_dot_5 ] ]
        [ hudToolbarBtn actions.onCycleCamera (cameraIconName data.cameraMode) (cameraLabel lang data.cameraMode)
        , hudIconBtn actions.onToggleAudio
            (if data.audioMuted then
                FeatherIcons.volumeX

             else
                FeatherIcons.volume2
            )
            audioLabel
        ]


{-| Whether the on-screen steering and brake pads are shown: only while the
player wants touch controls and is not steering by tilt, so tilt play keeps the
screen clear.
-}
touchPadsVisible : { a | showTouchControls : Bool, tiltEnabled : Bool, tiltAvailable : Bool } -> Bool
touchPadsVisible data =
    data.showTouchControls && not (data.tiltEnabled && data.tiltAvailable)


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


{-| Icon-only toolbar button; the label is kept for screen readers and tooltips.
-}
hudIconBtn : msg -> FeatherIcons.Icon -> String -> Html msg
hudIconBtn clickMsg ic labelText =
    button
        [ Attr.type_ "button"
        , onClick clickMsg
        , Attr.title labelText
        , Attr.attribute "aria-label" labelText
        , Attr.style "cursor" "pointer"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.5rem"
        , Attr.style "height" "2.5rem"
        , Attr.style "border-radius" "0.75rem"
        , Attr.style "border" "1px solid rgba(148, 163, 184, 0.28)"
        , Attr.style "background" "rgba(15, 23, 42, 0.78)"
        , Attr.style "color" "#ffffff"
        , Attr.style "pointer-events" "auto"
        ]
        [ ic |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]


cameraIconName : CameraMode -> FeatherIcons.Icon
cameraIconName mode =
    case mode of
        ChaseCam ->
            FeatherIcons.video

        OrbitCam ->
            FeatherIcons.compass


cameraLabel : Language -> CameraMode -> String
cameraLabel lang mode =
    case mode of
        ChaseCam ->
            RI18n.camChaseLabel lang

        OrbitCam ->
            RI18n.camOrbitLabel lang


speedKmh : PhysicsState -> Float
speedKmh physics =
    Vec3.length physics.velocity * 3.6


{-| Whether the car has stopped, which is when the player may leave the race.
-}
carStopped : HudViewData -> Bool
carStopped data =
    speedKmh data.physics < 1.0


viewCenterOverlays : HudViewData -> HudActions msg -> Html msg
viewCenterOverlays data actions =
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
                    , Attr.style "background" "rgba(5, 19, 29, 0.88)"
                    , Attr.style "border-color" Brand.legoYellow
                    , Attr.style "color" Brand.legoYellow
                    ]
                    [ text bannerText ]
                ]

        Nothing ->
            div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.justify_center, Tw.my_auto, Tw.gap Th.s3 ] ]
                (viewCrashOrAlert data actions ++ [ viewExitPrompt data actions ])


viewCrashOrAlert : HudViewData -> HudActions msg -> List (Html msg)
viewCrashOrAlert data actions =
    case data.crashRecovery of
        Just rec ->
            [ viewCrashOverlay data.language rec actions ]

        Nothing ->
            case data.hudAlert of
                Just alert ->
                    [ div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.mt Th.s2 ] ]
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
                    ]

                Nothing ->
                    []


{-| Leave the race, offered once the car has come to a stop.
-}
viewExitPrompt : HudViewData -> HudActions msg -> Html msg
viewExitPrompt data actions =
    if carStopped data then
        div [ Tw.classes [ Tw.pointer_events_auto ] ]
            [ Button.primaryOnDark actions.onGoMenu (RI18n.exitRaceLabel data.language) ]

    else
        text ""


viewCrashOverlay : Language -> { timer : Float, reason : CrashReason } -> HudActions msg -> Html msg
viewCrashOverlay lang rec actions =
    div
        [ Tw.classes [ Tw.pointer_events_auto, Tw.flex, Tw.items_center, Tw.justify_center ]
        ]
        [ div
            [ Tw.classes [ Tw.rounded_n2xl, Tw.p Th.s6, Tw.border, Tw.w_full, Tw.text_center, Tw.flex, Tw.flex_col, Tw.gap Th.s3 ]
            , Attr.style "max-width" "28rem"
            , Attr.style "background" "rgba(5, 19, 29, 0.92)"
            , Attr.style "border-color" Brand.red
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
            ++ [ viewFooter data ]
        )


{-| Bottom row: damage skeleton on the left, speed dial and metrics on the right.
-}
viewFooter : HudViewData -> Html msg
viewFooter data =
    div [ Tw.classes [ Tw.flex, Tw.items_end, Tw.justify_between, Tw.gap Th.s3, Tw.w_full ] ]
        [ viewDamagePanel data
        , viewSpeedPanel data
        ]



-- DAMAGE


viewDamagePanel : HudViewData -> Html msg
viewDamagePanel data =
    let
        lang =
            data.language

        damage =
            100 - healthPercent data.physics.integrity

        damageRatio =
            toFloat damage / 100.0
    in
    div
        [ Tw.classes [ Tw.rounded_xl, Tw.px Th.s3, Tw.py Th.s2_dot_5, Tw.border, Tw.flex, Tw.flex_col, Tw.items_center ]
        , Attr.style "background" "rgba(15, 23, 42, 0.78)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.25)"
        , Attr.style "width" "min(40vw, 12rem)"
        ]
        [ div [ Tw.classes [ Tw.flex, Tw.justify_between, Tw.w_full, Tw.text_xs, Tw.font_semibold ], Attr.style "color" "#e2e8f0" ]
            [ span [] [ text (RI18n.damageLabel lang) ]
            , span [ Tw.classes [ Tw.tabular_nums, Tw.font_bold ], Attr.style "color" (skeletonColor damageRatio 0.0) ]
                [ text (String.fromInt damage ++ "%") ]
            ]
        , viewSkeleton damageRatio
        , div [ Tw.classes [ Tw.flex, Tw.gap Th.s1_dot_5, Tw.flex_wrap, Tw.justify_center ] ]
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


{-| Vertical white skeleton of the car, top view with the nose at the top.
Each part turns from white to red once the damage passes its threshold, so the
wheels show damage first and the steering last.
-}
viewSkeleton : Float -> Html msg
viewSkeleton damage =
    Svg.svg
        [ SvgA.viewBox "0 0 60 112"
        , Attr.style "width" "4rem"
        , Attr.style "height" "auto"
        , Attr.style "display" "block"
        , Attr.style "margin" "0.25rem auto"
        ]
        [ skeletonPart damage
            0.1
            [ Svg.rect [ SvgA.x "3", SvgA.y "18", SvgA.width "9", SvgA.height "16", SvgA.rx "3" ] []
            , Svg.rect [ SvgA.x "48", SvgA.y "18", SvgA.width "9", SvgA.height "16", SvgA.rx "3" ] []
            , Svg.rect [ SvgA.x "3", SvgA.y "78", SvgA.width "9", SvgA.height "16", SvgA.rx "3" ] []
            , Svg.rect [ SvgA.x "48", SvgA.y "78", SvgA.width "9", SvgA.height "16", SvgA.rx "3" ] []
            ]
        , skeletonPart damage
            0.25
            [ Svg.line [ SvgA.x1 "12", SvgA.y1 "26", SvgA.x2 "48", SvgA.y2 "26" ] []
            , Svg.line [ SvgA.x1 "12", SvgA.y1 "86", SvgA.x2 "48", SvgA.y2 "86" ] []
            ]
        , skeletonPart damage
            0.4
            [ Svg.path [ SvgA.d "M 20 8 L 40 8 L 43 22 L 43 90 L 40 104 L 20 104 L 17 90 L 17 22 Z" ] [] ]
        , skeletonPart damage
            0.6
            [ Svg.path [ SvgA.d "M 20 42 L 40 42 M 20 70 L 40 70 M 26 42 L 26 70 M 34 42 L 34 70" ] [] ]
        , skeletonPart damage
            0.7
            [ Svg.circle [ SvgA.cx "30", SvgA.cy "14", SvgA.r "3" ] [] ]
        ]


skeletonPart : Float -> Float -> List (Svg msg) -> Svg msg
skeletonPart damage threshold children =
    Svg.g
        [ SvgA.stroke (skeletonColor damage threshold)
        , SvgA.strokeWidth "2"
        , SvgA.strokeLinecap "round"
        , SvgA.strokeLinejoin "round"
        , SvgA.fill "none"
        , SvgA.style "transition: stroke 0.4s ease"
        ]
        children


{-| White at no damage, red (`#ef4444`) once the damage is 0.25 past the part's
threshold, with linear blending in between.
-}
skeletonColor : Float -> Float -> String
skeletonColor damage threshold =
    let
        t =
            clamp 0.0 1.0 ((damage - threshold) / 0.25)

        mix target =
            String.fromInt (round (255 + (target - 255) * t))
    in
    "rgb(" ++ mix 239 ++ "," ++ mix 68 ++ "," ++ mix 68 ++ ")"


{-| Car integrity as a whole percentage, 0 .. 100.
-}
healthPercent : Float -> Int
healthPercent integrity =
    clamp 0 100 (round integrity)


hazardChip : String -> String -> Html msg
hazardChip col labelStr =
    span
        [ Tw.classes [ Tw.text_xs, Tw.font_bold, Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded_sm ]
        , Attr.style "background" col
        , Attr.style "color" Brand.legoBlack
        ]
        [ text labelStr ]



-- SPEED


viewSpeedPanel : HudViewData -> Html msg
viewSpeedPanel data =
    div
        [ Tw.classes [ Tw.rounded_xl, Tw.px Th.s3, Tw.py Th.s2, Tw.border, Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s1 ]
        , Attr.style "background" "rgba(15, 23, 42, 0.8)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.28)"
        , Attr.style "width" "min(40vw, 9.5rem)"
        ]
        [ viewSpeedDial (speedKmh data.physics)
        , viewCounter (Leaderboard.formatRaceTime data.elapsedTime)
        , viewMetrics data
        ]


{-| Top speed of the dial in km/h; the needle sweeps 270 degrees up to it.
-}
dialMaxKmh : Float
dialMaxKmh =
    80.0


{-| Needle angle in degrees from 12 o'clock for a speed in km/h.
-}
dialAngle : Float -> Float
dialAngle kmh =
    -135.0 + 270.0 * clamp 0.0 1.0 (kmh / dialMaxKmh)


{-| Point on the dial at a radius and an angle from 12 o'clock.
-}
pointOn : Float -> Float -> ( Float, Float )
pointOn radius angleDeg =
    let
        a =
            degrees angleDeg
    in
    ( 100.0 + radius * sin a, 100.0 - radius * cos a )


viewSpeedDial : Float -> Html msg
viewSpeedDial kmh =
    Svg.svg
        [ SvgA.viewBox "0 0 200 200"
        , Attr.style "width" "100%"
        , Attr.style "height" "auto"
        , Attr.style "display" "block"
        ]
        (List.concat
            [ [ Svg.circle
                    [ SvgA.cx "100"
                    , SvgA.cy "100"
                    , SvgA.r "98"
                    , SvgA.fill "#0b1220"
                    , SvgA.stroke "rgba(148, 163, 184, 0.35)"
                    , SvgA.strokeWidth "2"
                    ]
                    []
              ]
            , List.map dialTick (List.range 0 16)
            , List.map dialLabel [ 0, 20, 40, 60, 80 ]
            , [ Svg.text_
                    [ SvgA.x "100"
                    , SvgA.y "136"
                    , SvgA.textAnchor "middle"
                    , SvgA.fontSize "26"
                    , SvgA.fontWeight "800"
                    , SvgA.fill "#ffffff"
                    ]
                    [ Svg.text (String.fromInt (round kmh)) ]
              , Svg.text_
                    [ SvgA.x "100"
                    , SvgA.y "156"
                    , SvgA.textAnchor "middle"
                    , SvgA.fontSize "12"
                    , SvgA.fill "#94a3b8"
                    ]
                    [ Svg.text "km/h" ]
              , Svg.g
                    [ SvgA.transform ("rotate(" ++ String.fromFloat (dialAngle kmh) ++ " 100 100)")
                    , SvgA.style "transition: transform 0.12s linear"
                    ]
                    [ Svg.line
                        [ SvgA.x1 "100"
                        , SvgA.y1 "114"
                        , SvgA.x2 "100"
                        , SvgA.y2 "28"
                        , SvgA.stroke "#f59e0b"
                        , SvgA.strokeWidth "3"
                        , SvgA.strokeLinecap "round"
                        ]
                        []
                    ]
              , Svg.circle [ SvgA.cx "100", SvgA.cy "100", SvgA.r "6", SvgA.fill "#f59e0b" ] []
              ]
            ]
        )


dialTick : Int -> Svg msg
dialTick i =
    let
        kmh =
            toFloat (i * 5)

        major =
            modBy 4 i == 0

        angle =
            dialAngle kmh

        ( x1, y1 ) =
            pointOn
                (if major then
                    78.0

                 else
                    86.0
                )
                angle

        ( x2, y2 ) =
            pointOn 92.0 angle
    in
    Svg.line
        [ SvgA.x1 (String.fromFloat x1)
        , SvgA.y1 (String.fromFloat y1)
        , SvgA.x2 (String.fromFloat x2)
        , SvgA.y2 (String.fromFloat y2)
        , SvgA.stroke
            (if major then
                "#e2e8f0"

             else
                "#64748b"
            )
        , SvgA.strokeWidth
            (if major then
                "3"

             else
                "1.5"
            )
        ]
        []


dialLabel : Float -> Svg msg
dialLabel kmh =
    let
        ( x, y ) =
            pointOn 62.0 (dialAngle kmh)
    in
    Svg.text_
        [ SvgA.x (String.fromFloat x)
        , SvgA.y (String.fromFloat y)
        , SvgA.textAnchor "middle"
        , SvgA.dominantBaseline "middle"
        , SvgA.fontSize "13"
        , SvgA.fontWeight "600"
        , SvgA.fill "#cbd5e1"
        ]
        [ Svg.text (String.fromInt (round kmh)) ]


{-| The race timer as a mechanical counter: one dark tile per digit.
-}
viewCounter : String -> Html msg
viewCounter timeText =
    div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s0_dot_5 ] ]
        (List.map counterChar (String.toList timeText))


counterChar : Char -> Html msg
counterChar c =
    if Char.isDigit c then
        span
            [ Attr.style "display" "inline-block"
            , Attr.style "min-width" "1.1rem"
            , Attr.style "padding" "0.1rem 0.25rem"
            , Attr.style "text-align" "center"
            , Attr.style "border-radius" "0.3rem"
            , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
            , Attr.style "background" "#020617"
            , Attr.style "box-shadow" "inset 0 2px 4px rgba(0, 0, 0, 0.6)"
            , Attr.style "color" "#f8fafc"
            , Attr.style "font-family" "ui-monospace, SFMono-Regular, Menlo, monospace"
            , Attr.style "font-size" "1.1rem"
            , Attr.style "font-weight" "700"
            ]
            [ text (String.fromChar c) ]

    else
        span
            [ Attr.style "color" "#94a3b8"
            , Attr.style "font-size" "1.2rem"
            , Attr.style "font-weight" "700"
            ]
            [ text (String.fromChar c) ]


{-| Race metrics under the dial: checkpoints and split, course progress and
elevation left, surface and grade.
-}
viewMetrics : HudViewData -> Html msg
viewMetrics data =
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

        gradePct =
            round (data.physics.gradeRatio * 100.0)

        surfaceBadge =
            if data.physics.groundedWheels == 0 then
                "AIRBORNE!"

            else
                RI18n.surfaceName lang data.physics.surfaceUnderCar
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1, Tw.w_full, Tw.text_xs ], Attr.style "color" "#cbd5e1" ]
        [ metricRow
            (span [ Tw.classes [ Tw.tabular_nums, Tw.font_semibold ] ]
                [ text (String.fromInt cpPassed ++ "/" ++ String.fromInt cpTotal) ]
            )
            (case data.splitDelta of
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
            )
        , metricRow
            (span [ Tw.classes [ Tw.tabular_nums, Tw.font_semibold ] ] [ text (String.fromInt progressPct ++ "%") ])
            (span [] [ text (RI18n.elevationRemainingLabel lang dropRemaining) ])
        , metricRow
            (span
                [ Tw.classes [ Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded_sm, Tw.font_semibold ]
                , Attr.style "background" "rgba(56, 189, 248, 0.2)"
                , Attr.style "color" "#7dd3fc"
                ]
                [ text surfaceBadge ]
            )
            (span [ Tw.classes [ Tw.tabular_nums ] ] [ text ("-" ++ String.fromInt gradePct ++ "%") ])
        ]


metricRow : Html msg -> Html msg -> Html msg
metricRow left right =
    div [ Tw.classes [ Tw.flex, Tw.justify_between, Tw.items_center, Tw.w_full ] ]
        [ left, right ]



-- TOUCH FALLBACK


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
