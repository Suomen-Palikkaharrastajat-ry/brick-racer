module Pages.Garage exposing
    ( GarageActions
    , GarageViewData
    , PlayerModeTab(..)
    , PlayerSlot(..)
    , view
    )

{-| Unified setup screen for Brick Racer (`SetupPage`).

Combines the 3D turntable vehicle inspector, course selector with square preview cards,
builder entry CTA, hot seat & network multiplayer options, and host waiting room.

-}

import Component.Navbar as Navbar
import DesignTokens.Colors as Brand
import FeatherIcons
import Game.Multiplayer exposing (Role(..), RoomState, SessionMode(..))
import Html exposing (Html, button, div, h2, input, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import I18n exposing (Language(..))
import Json.Decode as Decode
import QRCode
import Racer.Analyze exposing (CarMeshBundle)
import Racer.I18n as RI18n
import Racer.Leaderboard as Leaderboard exposing (Leaderboard)
import Racer.Render as Render
import Racer.TerrainShader exposing (TerrainVertex)
import Racer.Types
    exposing
        ( RaceMode(..)
        , TrackId(..)
        , VehicleSpec
        , allTrackIds
        )
import Render.Camera exposing (Camera)
import Svg
import Svg.Attributes as SvgAttr
import Tailwind as Tw
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Th
import WebGL


{-| Player mode tab choices: local hot seat vs network multiplayer.
-}
type PlayerModeTab
    = HotSeatTab
    | NetworkTab


{-| Active car preview slot in two-player mode.
-}
type PlayerSlot
    = SlotPlayer1
    | SlotPlayer2


{-| View-model snapshot for rendering the unified setup screen.
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
    , leaderboard : Leaderboard
    , viewportWidth : Int
    , viewportHeight : Int
    , raceMode : RaceMode
    , audioMuted : Bool
    , roomCode : String
    , playerModeTab : PlayerModeTab
    , multiplayer : Maybe RoomState
    , shareCopiedNotice : Bool
    , baseUrl : String
    , activeSlot : PlayerSlot
    , playerOneTitle : String
    , playerTwoTitle : String
    }


{-| Actions dispatched by the unified setup screen controls.
-}
type alias GarageActions msg =
    { onOpenBuilder : msg
    , onStartRace : msg
    , onSelectTrack : TrackId -> msg
    , onChangeSeed : String -> msg
    , onRandomSeed : msg
    , onSetPlayerModeTab : PlayerModeTab -> msg
    , onSetRaceMode : RaceMode -> msg
    , onSelectPlayerSlot : PlayerSlot -> msg
    , onToggleAudio : msg
    , onStartHostMultiplayer : SessionMode -> msg
    , onJoinMultiplayer : String -> msg
    , onRoomCodeChanged : String -> msg
    , onOpenQrScanner : msg
    , onCopyMultiplayerLink : msg
    , onStartMultiplayerCountdown : msg
    , onLeaveMultiplayer : msg
    , onMouseDown : Float -> Float -> msg
    , onMouseMove : Float -> Float -> msg
    , onMouseUp : msg
    , onWheel : Float -> msg
    }


{-| Render the unified Setup screen.
-}
view : GarageViewData -> GarageActions msg -> Html msg
view data actions =
    let
        lang =
            data.language

        canvasWidth =
            max 320 (data.viewportWidth - 32)

        canvasHeight =
            clamp 240 500 (round (toFloat data.viewportHeight * 0.46))

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
        [ Tw.classes
            [ Tw.min_h_screen
            , Tw.w_full
            , Tw.flex
            , Tw.flex_col
            ]
        , Attr.style "background" ("linear-gradient(170deg, " ++ Brand.legoBlack ++ " 0%, #081b2a 50%, " ++ Brand.legoBlack ++ " 100%)")
        , Attr.style "color" Brand.legoWhite
        ]
        [ -- Header using master-builder Component.Navbar
          viewHeader data actions
        , -- Main Scrollable Content Area
          div
            [ Tw.classes
                [ Tw.flex_1
                , Tw.flex
                , Tw.flex_col
                , Tw.items_center
                , Tw.w_full
                , Tw.px Th.s4
                , Tw.py Th.s4
                , Tw.gap Th.s5
                ]
            ]
            [ div
                [ Tw.classes
                    [ Tw.w_full
                    , Tw.flex
                    , Tw.flex_col
                    , Tw.gap Th.s5
                    ]
                , Attr.style "max-width" "64rem"
                ]
                [ -- Dual Player Car Switcher (when in 2-Player mode)
                  viewPlayerSlotSelector data actions
                , -- 3D Car Turntable Container (~46vh)
                  viewTurntableHero data canvasWidth canvasHeight canvasEvents
                , -- Action Buttons: prominent builder entry + start race CTA
                  viewActionButtons data actions
                , -- Square Track Selection with Previews
                  viewCourseSelection data actions
                , -- Unified Player Mode & Multiplayer (Hot seat vs Network, Host Waiting Room)
                  viewPlayerOptions data actions
                ]
            ]
        ]


{-| Header using master-builder Navbar with brand and audio toggle.
-}
viewHeader : GarageViewData -> GarageActions msg -> Html msg
viewHeader data actions =
    Navbar.viewAppHeader
        { brand =
            Navbar.viewBrand
                { href = "#"
                , iconSrc = "/favicon.svg"
                , title = RI18n.menuTitle data.language
                , onClick = Nothing
                }
        , navLabel = "Päävalikko"
        , links = []
        , trailing =
            [ button
                [ Attr.type_ "button"
                , onClick actions.onToggleAudio
                , Tw.classes
                    [ Tw.p Th.s2
                    , Tw.rounded_lg
                    , Tw.text_simple Th.white
                    , Tw.cursor_pointer
                    , Bp.hover [ Tw.bg_color (Th.yellow Th.s400), Tw.text_simple Th.black ]
                    ]
                , Attr.attribute "aria-label"
                    (if data.audioMuted then
                        RI18n.audioMutedLabel data.language

                     else
                        RI18n.audioUnmutedLabel data.language
                    )
                , Attr.title
                    (if data.audioMuted then
                        RI18n.audioMutedLabel data.language

                     else
                        RI18n.audioUnmutedLabel data.language
                    )
                ]
                [ (if data.audioMuted then
                    FeatherIcons.volumeX

                   else
                    FeatherIcons.volume2
                  )
                    |> FeatherIcons.withSize 20
                    |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                ]
            ]
        , menuToggle = Nothing
        , sticky = False
        }


{-| Dual Player Car Switcher (when in 2-Player mode).
-}
viewPlayerSlotSelector : GarageViewData -> GarageActions msg -> Html msg
viewPlayerSlotSelector data actions =
    let
        p1Active =
            data.activeSlot == SlotPlayer1

        p2Active =
            data.activeSlot == SlotPlayer2

        slotButton isSelected icon title slot =
            button
                [ Attr.type_ "button"
                , onClick (actions.onSelectPlayerSlot slot)
                , Tw.classes
                    [ Tw.flex_1
                    , Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.gap Th.s2
                    , Tw.py Th.s2_dot_5
                    , Tw.px Th.s4
                    , Tw.rounded_xl
                    , Tw.font_bold
                    , Tw.text_sm
                    , Tw.cursor_pointer
                    , Tw.transition_all
                    ]
                , Attr.style "background"
                    (if isSelected then
                        Brand.legoYellow

                     else
                        "rgba(255, 255, 255, 0.08)"
                    )
                , Attr.style "color"
                    (if isSelected then
                        Brand.legoBlack

                     else
                        Brand.legoWhite
                    )
                , Attr.style "border"
                    (if isSelected then
                        "1px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.15)"
                    )
                ]
                [ span [] [ text icon ]
                , span [ Tw.classes [ Tw.truncate ] ] [ text title ]
                ]
    in
    if data.raceMode == SplitScreenTwoPlayer then
        div [ Tw.classes [ Tw.flex, Tw.gap Th.s3, Tw.w_full ] ]
            [ slotButton p1Active "🏎️ P1:" data.playerOneTitle SlotPlayer1
            , slotButton p2Active "🚙 P2:" data.playerTwoTitle SlotPlayer2
            ]

    else
        text ""


{-| 3D Turntable Hero container taking roughly half screen height.
-}
viewTurntableHero : GarageViewData -> Int -> Int -> List (Html.Attribute msg) -> Html msg
viewTurntableHero data canvasWidth canvasHeight canvasEvents =
    let
        lang =
            data.language
    in
    div
        [ Tw.classes
            [ Tw.relative
            , Tw.w_full
            , Tw.rounded_2xl
            , Tw.overflow_hidden
            , Tw.border
            , Tw.shrink_0
            ]
        , Attr.style "height" "46vh"
        , Attr.style "min-height" "240px"
        , Attr.style "max-height" "50vh"
        , Attr.style "touch-action" "pan-y"
        , Attr.style "background" Brand.legoBlack
        , Attr.style "border-color" "rgba(255, 255, 255, 0.12)"
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
            , Attr.style "top" "12px"
            , Attr.style "left" "12px"
            ]
            [ span
                [ Tw.classes [ Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_xl, Tw.text_xs, Tw.font_bold, Tw.border ]
                , Attr.style "background" "rgba(5, 19, 29, 0.88)"
                , Attr.style "border-color" "rgba(255, 255, 255, 0.2)"
                ]
                [ text data.vehicle.name ]
            , if data.partsTotal > 0 && data.partsLoaded < data.partsTotal then
                span
                    [ Tw.classes [ Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_xl, Tw.text_xs, Tw.font_semibold ]
                    , Attr.style "background" "rgba(2, 132, 199, 0.9)"
                    ]
                    [ text (RI18n.garageResolveProgress lang data.partsLoaded data.partsTotal) ]

              else
                text ""
            ]
        , span
            [ Tw.classes [ Tw.absolute, Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.text_xs, Tw.pointer_events_none ]
            , Attr.style "bottom" "8px"
            , Attr.style "right" "8px"
            , Attr.style "background" "rgba(5, 19, 29, 0.7)"
            , Attr.style "color" "#94a3b8"
            ]
            [ text "🖱️ Drag to rotate" ]
        ]


{-| Prominent action buttons directly below the turntable: open builder & start race CTA.
-}
viewActionButtons : GarageViewData -> GarageActions msg -> Html msg
viewActionButtons data actions =
    let
        lang =
            data.language

        tooFewWheels =
            List.length data.vehicle.wheels < 3

        inMultiplayerLobby =
            data.multiplayer /= Nothing
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Bp.sm [ Tw.flex_row ], Tw.gap Th.s3, Tw.w_full ] ]
        [ -- Secondary Builder CTA: "Rakenna oma kärry!"
          button
            [ Attr.type_ "button"
            , onClick actions.onOpenBuilder
            , Tw.classes
                [ Tw.flex_1
                , Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.gap Th.s2_dot_5
                , Tw.py Th.s3_dot_5
                , Tw.px Th.s6
                , Tw.rounded_xl
                , Tw.font_extrabold
                , Tw.text_base
                , Tw.cursor_pointer
                , Tw.transition_all
                ]
            , Attr.style "background" "rgba(255, 255, 255, 0.1)"
            , Attr.style "border" "1px solid rgba(255, 255, 255, 0.25)"
            , Attr.style "color" Brand.legoWhite
            ]
            [ FeatherIcons.tool |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml []
            , span []
                [ text
                    (if data.raceMode == SplitScreenTwoPlayer then
                        if data.activeSlot == SlotPlayer1 then
                            RI18n.openBuilderButton lang ++ " (P1)"

                        else
                            RI18n.openBuilderButton lang ++ " (P2)"

                     else
                        RI18n.openBuilderButton lang
                    )
                ]
            ]
        , -- Primary Start Race CTA: "Laske mäkeä alas!" or Multiplayer Lobby hint
          if inMultiplayerLobby then
            button
                [ Attr.type_ "button"
                , onClick actions.onStartMultiplayerCountdown
                , Tw.classes
                    [ Tw.flex_1
                    , Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.gap Th.s2_dot_5
                    , Tw.py Th.s3_dot_5
                    , Tw.px Th.s6
                    , Tw.rounded_xl
                    , Tw.font_extrabold
                    , Tw.text_base
                    , Tw.cursor_pointer
                    , Tw.transition_all
                    , Tw.shadow_lg
                    ]
                , Attr.style "background" Brand.legoYellow
                , Attr.style "color" Brand.legoBlack
                ]
                [ FeatherIcons.users |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml []
                , span [] [ text "Moninpelitila (katso alla)" ]
                ]

          else
            button
                [ Attr.type_ "button"
                , onClick actions.onStartRace
                , Tw.classes
                    [ Tw.flex_1
                    , Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.gap Th.s2_dot_5
                    , Tw.py Th.s3_dot_5
                    , Tw.px Th.s6
                    , Tw.rounded_xl
                    , Tw.font_extrabold
                    , Tw.text_base
                    , Tw.cursor_pointer
                    , Tw.transition_all
                    , Tw.shadow_lg
                    ]
                , Attr.style "background"
                    (if tooFewWheels then
                        "#475569"

                     else
                        Brand.legoYellow
                    )
                , Attr.style "color"
                    (if tooFewWheels then
                        "#94a3b8"

                     else
                        Brand.legoBlack
                    )
                ]
                [ FeatherIcons.play |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml []
                , span []
                    [ text
                        (if tooFewWheels then
                            RI18n.raceAnywayButton lang

                         else
                            RI18n.startRaceButton lang
                        )
                    ]
                ]
        ]


{-| Square track selection tiles with SVG course previews, PB badge, and seed controls.
-}
viewCourseSelection : GarageViewData -> GarageActions msg -> Html msg
viewCourseSelection data actions =
    let
        lang =
            data.language

        viewTrackCard trackId =
            let
                isSelected =
                    data.selectedTrack == trackId

                title =
                    RI18n.trackTitle lang trackId

                desc =
                    RI18n.trackDescription lang trackId

                personalBest =
                    Leaderboard.bestForTrack trackId data.leaderboard
                        |> Maybe.map (\entry -> String.fromFloat (roundFloat 2 entry.timeSeconds) ++ " s")
                        |> Maybe.withDefault "—"
            in
            button
                [ Attr.type_ "button"
                , onClick (actions.onSelectTrack trackId)
                , Attr.title desc
                , Tw.classes
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.justify_between
                    , Tw.p Th.s3
                    , Tw.rounded_xl
                    , Tw.text_left
                    , Tw.transition_all
                    , Tw.cursor_pointer
                    , Tw.aspect_square
                    , Tw.relative
                    , Tw.overflow_hidden
                    ]
                , Attr.style "background"
                    (if isSelected then
                        "rgba(13, 34, 53, 0.95)"

                     else
                        "rgba(7, 25, 38, 0.75)"
                    )
                , Attr.style "border"
                    (if isSelected then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.12)"
                    )
                , Attr.style "box-shadow"
                    (if isSelected then
                        "0 0 0 2px rgba(250, 200, 10, 0.3)"

                     else
                        "none"
                    )
                ]
                [ -- Card Top: Title & PB
                  div [ Tw.classes [ Tw.flex, Tw.items_start, Tw.justify_between, Tw.w_full, Tw.gap Th.s1 ] ]
                    [ div
                        [ Tw.classes [ Tw.font_bold, Tw.text_sm ]
                        , Attr.style "color"
                            (if isSelected then
                                Brand.legoYellow

                             else
                                Brand.legoWhite
                            )
                        ]
                        [ text title ]
                    , span
                        [ Tw.classes [ Tw.text_xs, Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded, Tw.font_mono ]
                        , Attr.style "background" "rgba(255, 255, 255, 0.08)"
                        , Attr.style "color"
                            (if personalBest == "—" then
                                "#94a3b8"

                             else
                                Brand.legoYellow
                            )
                        ]
                        [ text ("⏱ " ++ personalBest) ]
                    ]

                -- Card Middle: SVG Minimap Preview
                , div [ Tw.classes [ Tw.w_full, Tw.flex_1, Tw.my Th.s1, Tw.flex, Tw.items_center, Tw.justify_center ] ]
                    [ viewTrackPreviewSvg trackId ]
                ]
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s3 ] ]
        [ div [ Tw.classes [ Tw.flex, Tw.items_baseline, Tw.justify_between ] ]
            [ h2 [ Tw.classes [ Tw.text_sm, Tw.font_bold, Tw.uppercase, Tw.tracking_wider ], Attr.style "color" Brand.legoYellow ]
                [ text (RI18n.coursesHeading lang) ]
            , div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
                [ span [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ]
                    [ text (RI18n.seedLabel lang ++ ":") ]
                , input
                    [ Attr.type_ "number"
                    , Attr.value (String.fromInt data.seed)
                    , onInput actions.onChangeSeed
                    , Tw.classes [ Tw.w Th.s20, Tw.px Th.s2, Tw.py Th.s1, Tw.text_xs, Tw.rounded_lg, Tw.font_mono, Tw.text_center ]
                    , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                    , Attr.style "color" Brand.legoWhite
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onRandomSeed
                    , Tw.classes [ Tw.px Th.s2, Tw.py Th.s1, Tw.text_xs, Tw.rounded_lg, Tw.cursor_pointer ]
                    , Attr.style "background" "rgba(255, 255, 255, 0.1)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.title "Arvo uusi siemen"
                    ]
                    [ text "🎲" ]
                ]
            ]
        , div [ Tw.classes [ Tw.grid, Tw.grid_cols_1, Bp.sm [ Tw.grid_cols_3 ], Tw.gap Th.s3 ] ]
            (List.map viewTrackCard allTrackIds)
        ]


{-| Visual SVG course minimap / path / contour preview for a track.
-}
viewTrackPreviewSvg : TrackId -> Html msg
viewTrackPreviewSvg id =
    case id of
        FarmLane ->
            Svg.svg
                [ SvgAttr.viewBox "0 0 120 75"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                , Attr.style "display" "block"
                ]
                [ -- subtle ground contour
                  Svg.path
                    [ SvgAttr.d "M 0 50 Q 60 70 120 40 L 120 75 L 0 75 Z"
                    , SvgAttr.fill "#091c2b"
                    ]
                    []

                -- winding dirt road with S-bends
                , Svg.path
                    [ SvgAttr.d "M 18 14 C 40 14, 52 28, 52 42 C 52 56, 78 56, 78 40 C 78 24, 98 24, 102 62"
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "#D97706"
                    , SvgAttr.strokeWidth "7"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M 18 14 C 40 14, 52 28, 52 42 C 52 56, 78 56, 78 40 C 78 24, 98 24, 102 62"
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "#FDE68A"
                    , SvgAttr.strokeWidth "1.2"
                    , SvgAttr.strokeDasharray "3 3"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []

                -- Start green dot
                , Svg.circle [ SvgAttr.cx "18", SvgAttr.cy "14", SvgAttr.r "3.5", SvgAttr.fill "#22C55E" ] []

                -- Finish marker
                , Svg.circle [ SvgAttr.cx "102", SvgAttr.cy "62", SvgAttr.r "3.5", SvgAttr.fill "#FAC80A" ] []
                ]

        ForestTrail ->
            Svg.svg
                [ SvgAttr.viewBox "0 0 120 75"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                , Attr.style "display" "block"
                ]
                [ -- subtle pine tree silhouettes
                  Svg.polygon [ SvgAttr.points "12,42 16,30 20,42", SvgAttr.fill "#0e3a24" ] []
                , Svg.polygon [ SvgAttr.points "105,32 109,20 113,32", SvgAttr.fill "#0e3a24" ] []
                , Svg.polygon [ SvgAttr.points "58,22 62,10 66,22", SvgAttr.fill "#0e3a24" ] []

                -- rooty trail curve
                , Svg.path
                    [ SvgAttr.d "M 15 20 C 45 10, 65 30, 48 50 C 35 64, 75 68, 105 54"
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "#15803D"
                    , SvgAttr.strokeWidth "6"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []

                -- log jump indicator
                , Svg.line
                    [ SvgAttr.x1 "46"
                    , SvgAttr.y1 "47"
                    , SvgAttr.x2 "54"
                    , SvgAttr.y2 "53"
                    , SvgAttr.stroke "#B45309"
                    , SvgAttr.strokeWidth "3"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []

                -- Start green dot
                , Svg.circle [ SvgAttr.cx "15", SvgAttr.cy "20", SvgAttr.r "3.5", SvgAttr.fill "#22C55E" ] []

                -- Finish marker
                , Svg.circle [ SvgAttr.cx "105", SvgAttr.cy "54", SvgAttr.r "3.5", SvgAttr.fill "#FAC80A" ] []
                ]

        MeadowPath ->
            Svg.svg
                [ SvgAttr.viewBox "0 0 120 75"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                , Attr.style "display" "block"
                ]
                [ -- elevation contour lines
                  Svg.path [ SvgAttr.d "M 0 25 Q 60 10 120 25", SvgAttr.fill "none", SvgAttr.stroke "#163a24", SvgAttr.strokeWidth "1" ] []
                , Svg.path [ SvgAttr.d "M 0 50 Q 60 35 120 50", SvgAttr.fill "none", SvgAttr.stroke "#163a24", SvgAttr.strokeWidth "1" ] []

                -- switchback trail
                , Svg.path
                    [ SvgAttr.d "M 20 16 Q 95 18 88 32 Q 80 44 26 46 Q 18 58 98 64"
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "#84CC16"
                    , SvgAttr.strokeWidth "4"
                    , SvgAttr.strokeLinecap "round"
                    ]
                    []

                -- Start green dot
                , Svg.circle [ SvgAttr.cx "20", SvgAttr.cy "16", SvgAttr.r "3.5", SvgAttr.fill "#22C55E" ] []

                -- Finish marker
                , Svg.circle [ SvgAttr.cx "98", SvgAttr.cy "64", SvgAttr.r "3.5", SvgAttr.fill "#FAC80A" ] []
                ]


{-| Unified player mode & multiplayer: hot seat vs network play, and host waiting room.
-}
viewPlayerOptions : GarageViewData -> GarageActions msg -> Html msg
viewPlayerOptions data actions =
    case data.multiplayer of
        Just room ->
            viewMultiplayerLobby data actions room

        Nothing ->
            viewNormalPlayerOptions data actions


{-| Host / client waiting room dialog showing QR code, room code, and joined player roster.
-}
viewMultiplayerLobby : GarageViewData -> GarageActions msg -> RoomState -> Html msg
viewMultiplayerLobby data actions room =
    let
        lang =
            data.language

        isHost =
            room.role == HostCommentator

        isTwoPlayerHost =
            isHost && room.maxPlayers == 2

        playerCount =
            List.length room.players

        roomUrl =
            makeRoomUrl data.baseUrl room.roomId

        readyToStart =
            if isTwoPlayerHost then
                playerCount >= 2

            else
                playerCount >= 1
    in
    div
        [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s4, Tw.p Th.s5, Tw.rounded_2xl, Tw.border ]
        , Attr.style "background" "rgba(8, 27, 42, 0.95)"
        , Attr.style "border-color" Brand.legoYellow
        , Attr.style "box-shadow" "0 0 24px rgba(250, 200, 10, 0.15)"
        ]
        [ -- Title & Role Badge
          div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s1, Tw.text_center ] ]
            [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
                [ span [ Tw.classes [ Tw.px Th.s2, Tw.py Th.s0_dot_5, Tw.rounded, Tw.text_xs, Tw.font_bold, Tw.uppercase ], Attr.style "background" Brand.legoYellow, Attr.style "color" Brand.legoBlack ]
                    [ text
                        (if isHost then
                            "HOST"

                         else
                            "PLAYER"
                        )
                    ]
                , h2 [ Tw.classes [ Tw.text_base, Bp.sm [ Tw.text_lg ], Tw.font_extrabold, Tw.tracking_wide ] ]
                    [ text
                        (if isTwoPlayerHost then
                            RI18n.networkTwoPlayersTitle lang

                         else if isHost then
                            RI18n.networkHostGridTitle lang

                         else
                            "Huone: " ++ room.roomId
                        )
                    ]
                ]
            , p [ Tw.classes [ Tw.text_xs ], Attr.style "max-width" "28rem", Attr.style "color" "#94a3b8" ]
                [ text
                    (if isTwoPlayerHost then
                        "Skannaa QR-koodi toisella laitteella liittyäksesi kisaan."

                     else if isHost then
                        RI18n.networkHostGridDesc lang

                     else
                        RI18n.waitingForHostToStart lang
                    )
                ]
            ]
        , -- QR Code & Room Code Banner (Prominent for Hosts!)
          if isHost then
            div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s3 ] ]
                [ viewQrCode roomUrl
                , div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
                    [ span [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ] [ text "Huonekoodi:" ]
                    , span [ Tw.classes [ Tw.text_lg, Tw.font_mono, Tw.font_black, Tw.tracking_widest, Tw.px Th.s2, Tw.py Th.s0_dot_5, Tw.rounded_lg, Tw.border ], Attr.style "color" Brand.legoYellow, Attr.style "background" "rgba(15, 23, 42, 0.8)", Attr.style "border-color" "rgba(250, 200, 10, 0.3)" ]
                        [ text room.roomId ]
                    , button
                        [ Attr.type_ "button"
                        , onClick actions.onCopyMultiplayerLink
                        , Tw.classes [ Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_lg, Tw.text_xs, Tw.font_bold, Tw.cursor_pointer, Tw.transition_all ]
                        , Attr.style "background" "rgba(255, 255, 255, 0.1)"
                        , Attr.style "color" Brand.legoWhite
                        ]
                        [ text
                            (if data.shareCopiedNotice then
                                "✓ Kopioitu!"

                             else
                                RI18n.copyRoomLinkButton lang
                            )
                        ]
                    ]
                ]

          else
            text ""
        , -- Player Roster & Readiness Status
          div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s2, Tw.w_full ], Attr.style "max-width" "28rem" ]
            [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.w_full, Tw.px Th.s2 ] ]
                [ span [ Tw.classes [ Tw.text_xs, Tw.font_bold, Tw.uppercase, Tw.tracking_wider ], Attr.style "color" Brand.legoYellow ]
                    [ text ("Pelaajat (" ++ String.fromInt playerCount ++ " / " ++ String.fromInt room.maxPlayers ++ ")") ]
                , span
                    [ Tw.classes [ Tw.text_xs, Tw.font_semibold, Tw.px Th.s2, Tw.py Th.s0_dot_5, Tw.rounded_md ]
                    , Attr.style "background"
                        (if readyToStart then
                            "rgba(34, 197, 94, 0.2)"

                         else
                            "rgba(245, 158, 11, 0.2)"
                        )
                    , Attr.style "color"
                        (if readyToStart then
                            "#22c55e"

                         else
                            "#f59e0b"
                        )
                    ]
                    [ text
                        (if isTwoPlayerHost then
                            if playerCount >= 2 then
                                RI18n.bothPlayersReady lang

                            else
                                RI18n.waitingForPlayerTwo lang

                         else if isHost then
                            if playerCount >= 1 then
                                "Valmis aloittamaan!"

                            else
                                "Odotetaan pelaajia..."

                         else
                            RI18n.waitingForHostToStart lang
                        )
                    ]
                ]
            , div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5, Tw.w_full ] ]
                (List.map
                    (\p ->
                        div
                            [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.px Th.s3, Tw.py Th.s2, Tw.rounded_lg, Tw.border ]
                            , Attr.style "background" "rgba(15, 23, 42, 0.7)"
                            , Attr.style "border-color" "rgba(255, 255, 255, 0.1)"
                            ]
                            [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
                                [ span [ Tw.classes [ Tw.text_xs, Tw.font_mono, Tw.font_bold ], Attr.style "color" Brand.legoYellow ]
                                    [ text ("#" ++ String.fromInt p.slot) ]
                                , span [ Tw.classes [ Tw.text_xs, Tw.font_semibold ] ] [ text p.name ]
                                ]
                            , span
                                [ Tw.classes [ Tw.text_xs ]
                                , Attr.style "color"
                                    (if p.vehicleReady then
                                        "#22c55e"

                                     else
                                        "#94a3b8"
                                    )
                                ]
                                [ text
                                    (if p.vehicleReady then
                                        "Auto valmis"

                                     else
                                        "Ladataan..."
                                    )
                                ]
                            ]
                    )
                    room.players
                )
            ]
        , -- Action Buttons: Start Race (Host only) & Leave Room
          div [ Tw.classes [ Tw.flex, Tw.gap Th.s3, Tw.w_full, Tw.pt Th.s2 ], Attr.style "max-width" "28rem" ]
            [ button
                [ Attr.type_ "button"
                , onClick actions.onLeaveMultiplayer
                , Tw.classes [ Tw.flex_1, Tw.py Th.s2_dot_5, Tw.px Th.s4, Tw.rounded_xl, Tw.text_xs, Tw.font_bold, Tw.cursor_pointer, Tw.transition_all ]
                , Attr.style "background" "rgba(255, 255, 255, 0.08)"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
                , Attr.style "color" "#cbd5e1"
                ]
                [ text (RI18n.leaveRoomButton lang) ]
            , if isHost then
                button
                    [ Attr.type_ "button"
                    , onClick actions.onStartMultiplayerCountdown
                    , Attr.disabled (not readyToStart)
                    , Tw.classes
                        [ Tw.flex_1
                        , Tw.py Th.s2_dot_5
                        , Tw.px Th.s4
                        , Tw.rounded_xl
                        , Tw.text_xs
                        , Tw.font_extrabold
                        , Tw.transition_all
                        , if readyToStart then
                            Tw.cursor_pointer

                          else
                            Tw.cursor_not_allowed
                        ]
                    , Attr.style "background"
                        (if readyToStart then
                            Brand.legoYellow

                         else
                            "#475569"
                        )
                    , Attr.style "color"
                        (if readyToStart then
                            Brand.legoBlack

                         else
                            "#94a3b8"
                        )
                    ]
                    [ text
                        (if isTwoPlayerHost && not readyToStart then
                            "Odotetaan 2. pelaajaa..."

                         else
                            RI18n.startMultiplayerRaceButton lang
                        )
                    ]

              else
                text ""
            ]
        ]


{-| Normal player mode selection when not in an active multiplayer room.
-}
viewNormalPlayerOptions : GarageViewData -> GarageActions msg -> Html msg
viewNormalPlayerOptions data actions =
    let
        lang =
            data.language
    in
    div
        [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s3 ] ]
        [ -- Header with Mode Tabs (Hot Seat vs Network)
          div [ Tw.classes [ Tw.flex, Tw.flex_col, Bp.sm [ Tw.flex_row ], Tw.items_start, Bp.sm [ Tw.items_center ], Tw.justify_between, Tw.gap Th.s2 ] ]
            [ h2 [ Tw.classes [ Tw.text_sm, Tw.font_bold, Tw.uppercase, Tw.tracking_wider ], Attr.style "color" Brand.legoYellow ]
                [ text (RI18n.playerOptionsHeading lang) ]
            , div [ Tw.classes [ Tw.flex, Tw.rounded_xl, Tw.p Th.s1, Tw.gap Th.s1 ], Attr.style "background" "rgba(15, 23, 42, 0.85)" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick (actions.onSetPlayerModeTab HotSeatTab)
                    , Tw.classes [ Tw.py Th.s1_dot_5, Tw.px Th.s3, Tw.rounded_lg, Tw.text_xs, Tw.font_bold, Tw.transition_all, Tw.cursor_pointer ]
                    , Attr.style "background"
                        (if data.playerModeTab == HotSeatTab then
                            Brand.legoYellow

                         else
                            "transparent"
                        )
                    , Attr.style "color"
                        (if data.playerModeTab == HotSeatTab then
                            Brand.legoBlack

                         else
                            "#94a3b8"
                        )
                    ]
                    [ text ("🛋️ " ++ RI18n.hotSeatTabLabel lang) ]
                , button
                    [ Attr.type_ "button"
                    , onClick (actions.onSetPlayerModeTab NetworkTab)
                    , Tw.classes [ Tw.py Th.s1_dot_5, Tw.px Th.s3, Tw.rounded_lg, Tw.text_xs, Tw.font_bold, Tw.transition_all, Tw.cursor_pointer ]
                    , Attr.style "background"
                        (if data.playerModeTab == NetworkTab then
                            Brand.legoYellow

                         else
                            "transparent"
                        )
                    , Attr.style "color"
                        (if data.playerModeTab == NetworkTab then
                            Brand.legoBlack

                         else
                            "#94a3b8"
                        )
                    ]
                    [ text ("🌐 " ++ RI18n.networkTabLabel lang) ]
                ]
            ]
        , case data.playerModeTab of
            HotSeatTab ->
                viewHotSeatOptions data actions

            NetworkTab ->
                viewNetworkOptions data actions
        ]


{-| Hot seat 1-2 players options on local device.
-}
viewHotSeatOptions : GarageViewData -> GarageActions msg -> Html msg
viewHotSeatOptions data actions =
    let
        lang =
            data.language

        renderCard mode title desc icon =
            let
                isSelected =
                    data.raceMode == mode
            in
            button
                [ Attr.type_ "button"
                , onClick (actions.onSetRaceMode mode)
                , Attr.title desc
                , Tw.classes
                    [ Tw.flex_1
                    , Tw.flex
                    , Tw.flex_col
                    , Tw.p Th.s3_dot_5
                    , Tw.rounded_xl
                    , Tw.text_left
                    , Tw.cursor_pointer
                    , Tw.transition_all
                    ]
                , Attr.style "background"
                    (if isSelected then
                        "rgba(13, 34, 53, 0.95)"

                     else
                        "rgba(15, 23, 42, 0.65)"
                    )
                , Attr.style "border"
                    (if isSelected then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.1)"
                    )
                , Attr.style "box-shadow"
                    (if isSelected then
                        "0 0 0 2px rgba(250, 200, 10, 0.25)"

                     else
                        "none"
                    )
                ]
                [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.w_full ] ]
                    [ div
                        [ Tw.classes [ Tw.font_bold, Tw.text_sm ]
                        , Attr.style "color"
                            (if isSelected then
                                Brand.legoYellow

                             else
                                Brand.legoWhite
                            )
                        ]
                        [ text (icon ++ " " ++ title) ]
                    , if isSelected then
                        span [ Tw.classes [ Tw.text_xs, Tw.font_bold, Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded ], Attr.style "background" Brand.legoYellow, Attr.style "color" Brand.legoBlack ]
                            [ text "VALITTU" ]

                      else
                        text ""
                    ]
                ]
    in
    div [ Tw.classes [ Tw.grid, Tw.grid_cols_1, Bp.sm [ Tw.grid_cols_2 ], Tw.gap Th.s3 ] ]
        [ renderCard SinglePlayer (RI18n.singlePlayerModeLabel lang) (RI18n.hotSeatSingleDesc lang) "🏎️"
        , renderCard SplitScreenTwoPlayer (RI18n.splitScreenModeLabel lang) (RI18n.hotSeatSplitDesc lang) "🏁"
        ]


{-| Network play options: 2P network race, host multiplayer grid, or join room.
-}
viewNetworkOptions : GarageViewData -> GarageActions msg -> Html msg
viewNetworkOptions data actions =
    let
        lang =
            data.language
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s3, Tw.w_full ] ]
        [ -- 1. Top row: 2P | Host
          div [ Tw.classes [ Tw.grid, Tw.grid_cols_1, Bp.sm [ Tw.grid_cols_2 ], Tw.gap Th.s3 ] ]
            [ -- Card 1: 2P
              div
                [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.justify_between, Tw.p Th.s3_dot_5, Tw.rounded_xl, Tw.border ]
                , Attr.style "background" "rgba(15, 23, 42, 0.65)"
                , Attr.style "border-color" "rgba(255, 255, 255, 0.1)"
                ]
                [ div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5 ] ]
                    [ div [ Tw.classes [ Tw.font_bold, Tw.text_sm, Tw.text_simple Th.white ] ]
                        [ text ("👥 " ++ RI18n.networkTwoPlayersTitle lang) ]
                    , p [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ]
                        [ text (RI18n.networkTwoPlayersDesc lang) ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick (actions.onStartHostMultiplayer TwoPlayers)
                    , Tw.classes [ Tw.w_full, Tw.mt Th.s3, Tw.py Th.s2, Tw.px Th.s3, Tw.rounded_lg, Tw.text_xs, Tw.font_extrabold, Tw.cursor_pointer, Tw.transition_all ]
                    , Attr.style "background" "rgba(250, 200, 10, 0.2)"
                    , Attr.style "border" "1px solid rgba(250, 200, 10, 0.4)"
                    , Attr.style "color" Brand.legoYellow
                    ]
                    [ text (RI18n.createTwoPlayerRoomButton lang) ]
                ]
            , -- Card 2: Host
              div
                [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.justify_between, Tw.p Th.s3_dot_5, Tw.rounded_xl, Tw.border ]
                , Attr.style "background" "rgba(15, 23, 42, 0.65)"
                , Attr.style "border-color" "rgba(255, 255, 255, 0.1)"
                ]
                [ div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5 ] ]
                    [ div [ Tw.classes [ Tw.font_bold, Tw.text_sm, Tw.text_simple Th.white ] ]
                        [ text ("📺 " ++ RI18n.networkHostGridTitle lang) ]
                    , p [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ]
                        [ text (RI18n.networkHostGridDesc lang) ]
                    ]
                , button
                    [ Attr.type_ "button"
                    , onClick (actions.onStartHostMultiplayer GridSession)
                    , Tw.classes [ Tw.w_full, Tw.mt Th.s3, Tw.py Th.s2, Tw.px Th.s3, Tw.rounded_lg, Tw.text_xs, Tw.font_extrabold, Tw.cursor_pointer, Tw.transition_all ]
                    , Attr.style "background" "rgba(250, 200, 10, 0.2)"
                    , Attr.style "border" "1px solid rgba(250, 200, 10, 0.4)"
                    , Attr.style "color" Brand.legoYellow
                    ]
                    [ text (RI18n.createGridRoomButton lang) ]
                ]
            ]
        , -- 2. Bottom row: Join (spanning full width)
          div
            [ Tw.classes [ Tw.flex, Tw.flex_col, Bp.sm [ Tw.flex_row, Tw.items_center ], Tw.justify_between, Tw.gap Th.s3, Tw.p Th.s3_dot_5, Tw.rounded_xl, Tw.border ]
            , Attr.style "background" "rgba(15, 23, 42, 0.65)"
            , Attr.style "border-color" "rgba(255, 255, 255, 0.1)"
            ]
            [ div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s0_dot_5, Tw.min_w Th.s0 ] ]
                [ div [ Tw.classes [ Tw.font_bold, Tw.text_sm, Tw.text_simple Th.white ] ]
                    [ text ("📥 " ++ RI18n.networkJoinTitle lang) ]
                , p [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ]
                    [ text
                        (case lang of
                            Fi ->
                                "Syötä kaverin huonekoodi tai lue QR-koodi kameralla."

                            En ->
                                "Enter a friend's room code or scan their QR code."
                        )
                    ]
                ]
            , div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2, Tw.w_full, Bp.sm [ Tw.w_auto ] ] ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.placeholder
                        (case lang of
                            Fi ->
                                "Syötä huonekoodi..."

                            En ->
                                "Room code..."
                        )
                    , Attr.value data.roomCode
                    , onInput actions.onRoomCodeChanged
                    , Tw.classes [ Tw.flex_1, Tw.px Th.s3, Tw.py Th.s2, Tw.rounded_lg, Tw.text_xs, Tw.font_mono ]
                    , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "min-width" "160px"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick (actions.onJoinMultiplayer data.roomCode)
                    , Tw.classes [ Tw.py Th.s2, Tw.px Th.s4, Tw.rounded_lg, Tw.text_xs, Tw.font_bold, Tw.cursor_pointer, Tw.whitespace_nowrap, Tw.transition_all ]
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    ]
                    [ text (RI18n.joinRoomButton lang) ]
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onOpenQrScanner
                    , Tw.classes [ Tw.py Th.s2, Tw.px Th.s3, Tw.rounded_lg, Tw.text_xs, Tw.font_bold, Tw.cursor_pointer, Tw.whitespace_nowrap, Tw.transition_all ]
                    , Attr.style "background" "rgba(255, 255, 255, 0.1)"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.title
                        (case lang of
                            Fi ->
                                "Lue QR-koodi kameralla"

                            En ->
                                "Scan QR code with camera"
                        )
                    ]
                    [ text "📷 QR" ]
                ]
            ]
        ]


{-| Render a scannable QR Code element.
-}
viewQrCode : String -> Html msg
viewQrCode url =
    case QRCode.fromString url of
        Ok qr ->
            div
                [ Attr.style "background" "#ffffff"
                , Attr.style "padding" "12px"
                , Attr.style "border-radius" "16px"
                , Attr.style "box-shadow" "0 4px 20px rgba(0, 0, 0, 0.4)"
                , Attr.style "display" "inline-block"
                ]
                [ QRCode.toSvg
                    [ SvgAttr.width "180px"
                    , SvgAttr.height "180px"
                    ]
                    qr
                ]

        Err _ ->
            text ""


{-| Construct full or fragment room URL for QR codes.
-}
makeRoomUrl : String -> String -> String
makeRoomUrl baseUrl roomId =
    let
        cleanBase =
            if String.endsWith "/" baseUrl then
                String.dropRight 1 baseUrl

            else
                baseUrl
    in
    if String.isEmpty cleanBase then
        "#room=" ++ roomId

    else
        cleanBase ++ "/#room=" ++ roomId


{-| Helper to round floats to N decimal places.
-}
roundFloat : Int -> Float -> Float
roundFloat decimals val =
    let
        factor =
            10.0 ^ toFloat decimals
    in
    toFloat (round (val * factor)) / factor
