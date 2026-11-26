module Pages.Garage exposing
    ( GarageActions
    , GarageViewData
    , MenuPanel(..)
    , PlayerModeTab(..)
    , PlayerSlot(..)
    , seedInputId
    , view
    )

{-| Unified setup screen (main menu) for Brick Racer (`SetupPage`).

From top to bottom: the toolbar, the 3D turntable with the car, the two main
buttons (build a car, race), a "Course | World" switch, and the player mode
row. The world panel is a row of icon buttons: a die that rolls a new seed,
and the biome, season, weather and slope of the generated world, each of
which steps that property on. Under them sits the small seed, which opens an
editor when clicked, and a copy button. The course panel shows the plan of the
course. Player modes (1P / 2P, local / network, joining a room) are icon-only
buttons with labels for screen readers and tooltips. The host waiting room
replaces the player mode row while a room is open.

-}

import Component.Navbar as Navbar
import DesignTokens.Colors as Brand
import FeatherIcons exposing (Icon)
import Game.MapParams as Map exposing (Biome, MapParams, Pick(..), Season, Selection, WeatherKind)
import Game.Multiplayer exposing (Role(..), RoomState, SessionMode(..))
import Html exposing (Html, button, div, h2, input, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Lazy
import I18n exposing (Language(..))
import Json.Decode as Decode
import QRCode
import Racer.Analyze exposing (CarMeshBundle)
import Racer.Course as Course
import Racer.CoursePreview as CoursePreview
import Racer.I18n as RI18n
import Racer.Leaderboard as Leaderboard exposing (Leaderboard)
import Racer.MenuIcons as MenuIcons
import Racer.Render as Render
import Racer.TerrainShader exposing (TerrainVertex)
import Racer.Track as Track
import Racer.Types
    exposing
        ( RaceMode(..)
        , VehicleSpec
        )
import Render.Camera exposing (Camera)
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


{-| The two panels under the main buttons: the course plan or the world
buttons.
-}
type MenuPanel
    = CoursePanel
    | WorldPanel


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
    , selection : Selection
    , map : MapParams
    , seedText : String
    , editingSeed : Bool
    , menuPanel : MenuPanel
    , hasPreviousMap : Bool
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

`onChangeSeed` sends each keystroke of the seed editor; the seed is applied
with `onCommitSeed` (Enter or leaving the field) and dropped with
`onCancelSeedEdit` (Escape).

-}
type alias GarageActions msg =
    { onOpenBuilder : msg
    , onStartRace : msg
    , onPickBiome : Pick Biome -> msg
    , onPickSeason : Pick Season -> msg
    , onPickWeather : Pick WeatherKind -> msg
    , onToggleSteepness : msg
    , onChangeSeed : String -> msg
    , onStartSeedEdit : msg
    , onCommitSeed : msg
    , onCancelSeedEdit : msg
    , onCopySeed : msg
    , onRandomSeed : msg
    , onUsePreviousMap : msg
    , onSetMenuPanel : MenuPanel -> msg
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


{-| The DOM id of the seed editor, which `Main` focuses when it opens.
-}
seedInputId : String
seedInputId =
    "racer-seed-input"


{-| Render the unified Setup screen.
-}
view : GarageViewData -> GarageActions msg -> Html msg
view data actions =
    let
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
        [ viewHeader data actions
        , div
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
                    , Tw.gap Th.s4
                    ]
                , Attr.style "max-width" "64rem"
                ]
                [ viewPlayerSlotSelector data actions
                , viewTurntableHero data canvasWidth canvasHeight canvasEvents
                , viewActionButtons data actions
                , viewMenuPanels data actions
                , viewPlayerOptions data actions
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
            data.map
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
                    [ text (RI18n.garageResolveProgress data.language data.partsLoaded data.partsTotal) ]

              else
                text ""
            ]
        ]


{-| The two main buttons under the turntable: build a car, and race.
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

        mainButtonClasses =
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

        iconOf icon =
            icon |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Bp.sm [ Tw.flex_row ], Tw.gap Th.s3, Tw.w_full ] ]
        [ button
            [ Attr.type_ "button"
            , onClick actions.onOpenBuilder
            , Tw.classes mainButtonClasses
            , Attr.style "background" "rgba(255, 255, 255, 0.1)"
            , Attr.style "border" "1px solid rgba(255, 255, 255, 0.25)"
            , Attr.style "color" Brand.legoWhite
            ]
            [ iconOf FeatherIcons.tool
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
        , if inMultiplayerLobby then
            button
                [ Attr.type_ "button"
                , onClick actions.onStartMultiplayerCountdown
                , Tw.classes (Tw.shadow_lg :: mainButtonClasses)
                , Attr.style "background" Brand.legoYellow
                , Attr.style "color" Brand.legoBlack
                ]
                [ iconOf FeatherIcons.users
                , span [] [ text (RI18n.startRaceButton lang) ]
                ]

          else
            button
                [ Attr.type_ "button"
                , onClick actions.onStartRace
                , Tw.classes (Tw.shadow_lg :: mainButtonClasses)
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
                [ iconOf FeatherIcons.play
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


{-| The "Course | World" switch and the panel it shows.
-}
viewMenuPanels : GarageViewData -> GarageActions msg -> Html msg
viewMenuPanels data actions =
    let
        lang =
            data.language

        tab panel icon label =
            let
                isSelected =
                    data.menuPanel == panel
            in
            button
                [ Attr.type_ "button"
                , onClick (actions.onSetMenuPanel panel)
                , Attr.attribute "aria-pressed" (boolText isSelected)
                , Tw.classes [ Tw.flex_1, Tw.flex, Tw.items_center, Tw.justify_center, Tw.gap Th.s1_dot_5, Tw.py Th.s1_dot_5, Tw.px Th.s3, Tw.rounded_lg, Tw.text_xs, Tw.font_bold, Tw.transition_all, Tw.cursor_pointer ]
                , Attr.style "background"
                    (if isSelected then
                        Brand.legoYellow

                     else
                        "transparent"
                    )
                , Attr.style "color"
                    (if isSelected then
                        Brand.legoBlack

                     else
                        "#94a3b8"
                    )
                ]
                [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                , span [] [ text label ]
                ]
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s3, Tw.w_full ] ]
        [ div
            [ Tw.classes [ Tw.flex, Tw.rounded_xl, Tw.p Th.s1, Tw.gap Th.s1, Tw.w_full ]
            , Attr.style "max-width" "20rem"
            , Attr.style "background" "rgba(15, 23, 42, 0.85)"
            ]
            [ tab CoursePanel FeatherIcons.map (RI18n.menuCourseTab lang)
            , tab WorldPanel FeatherIcons.globe (RI18n.menuWorldTab lang)
            ]
        , case data.menuPanel of
            CoursePanel ->
                viewCoursePanel data

            WorldPanel ->
                viewWorldPanel data actions
        ]


{-| The course plan, and the personal best on this map when there is one.
-}
viewCoursePanel : GarageViewData -> Html msg
viewCoursePanel data =
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s2, Tw.w_full ] ]
        [ Html.Lazy.lazy2 viewCoursePreview data.language data.map
        , case Leaderboard.bestForMap data.map data.leaderboard of
            Just entry ->
                span
                    [ Tw.classes [ Tw.self_end, Tw.text_xs, Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded, Tw.font_mono ]
                    , Attr.style "background" "rgba(255, 255, 255, 0.08)"
                    , Attr.style "color" Brand.legoYellow
                    ]
                    [ text ("⏱ " ++ String.fromFloat (roundFloat 2 entry.timeSeconds) ++ " s") ]

            Nothing ->
                text ""
        ]


{-| The plan view of the selected map's course. Lazy on the map, so the
course is generated again only when the map changes.
-}
viewCoursePreview : Language -> MapParams -> Html msg
viewCoursePreview lang map =
    div
        [ Tw.classes [ Tw.px Th.s2, Tw.py Th.s1_dot_5, Tw.rounded_xl ]
        , Attr.style "background" "rgba(7, 25, 38, 0.75)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.12)"
        ]
        [ Track.trackConfigFor map
            |> Track.generate
            |> CoursePreview.fromTrack
            |> CoursePreview.view (RI18n.mapTitle lang map)
        ]


{-| The world buttons: a die for a new world, then the biome, season,
weather and slope of the generated world. Each of the four steps its property
on to the next value; the slope button looks for the nearest seed whose
course has the other steepness. Under them, on the right, the small seed and
its copy button.
-}
viewWorldPanel : GarageViewData -> GarageActions msg -> Html msg
viewWorldPanel data actions =
    let
        lang =
            data.language

        labels =
            RI18n.mapPickerLabels lang

        map =
            data.map

        nextWeather =
            nextIn (Map.allowedWeathers map.biome map.season) map.weather

        steepness =
            Course.steepness map
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s1, Tw.w_full ] ]
        [ div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.justify_center, Tw.items_center, Tw.gap Th.s2 ] ]
            ([ iconButton
                { label = RI18n.randomWorldLabel lang
                , pressed = Nothing
                , onPress = actions.onRandomSeed
                , icon = MenuIcons.dice
                , accent = True
                }
             , iconButton
                { label = labels.biome ++ ": " ++ labels.biomeName map.biome
                , pressed = Nothing
                , onPress = actions.onPickBiome (Exactly (nextIn Map.allBiomes map.biome))
                , icon = MenuIcons.biome map.biome
                , accent = False
                }
             , iconButton
                { label = labels.season ++ ": " ++ labels.seasonName map.season
                , pressed = Nothing
                , onPress = actions.onPickSeason (Exactly (nextIn Map.allSeasons map.season))
                , icon = MenuIcons.season map.season
                , accent = False
                }
             , iconButton
                { label = labels.weather ++ ": " ++ labels.weatherName map.weather
                , pressed = Nothing
                , onPress = actions.onPickWeather (Exactly nextWeather)
                , icon = MenuIcons.weather map.weather
                , accent = False
                }
             , iconButton
                { label = RI18n.steepnessLabel lang steepness (Course.gradientPercent map)
                , pressed = Nothing
                , onPress = actions.onToggleSteepness
                , icon = MenuIcons.steepness steepness
                , accent = False
                }
             ]
                ++ (if data.hasPreviousMap then
                        [ iconButton
                            { label = labels.previous
                            , pressed = Nothing
                            , onPress = actions.onUsePreviousMap
                            , icon = FeatherIcons.rotateCcw
                            , accent = False
                            }
                        ]

                    else
                        []
                   )
            )
        , viewSeedRow data actions
        ]


{-| The small seed under the world buttons. Clicked, it turns into a field
that applies the seed on Enter or when it loses focus (Escape drops the
edit). The copy button stays in the same place in both states, so a click on
it is not lost when the field closes on blur.
-}
viewSeedRow : GarageViewData -> GarageActions msg -> Html msg
viewSeedRow data actions =
    let
        lang =
            data.language

        keyDecoder =
            Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "Enter" ->
                                Decode.succeed actions.onCommitSeed

                            "Escape" ->
                                Decode.succeed actions.onCancelSeedEdit

                            _ ->
                                Decode.fail "other key"
                    )

        seedView =
            if data.editingSeed then
                input
                    [ Attr.id seedInputId
                    , Attr.type_ "text"
                    , Attr.attribute "inputmode" "numeric"
                    , Attr.attribute "aria-label" (RI18n.mapPickerLabels lang).seed
                    , Attr.value data.seedText
                    , onInput actions.onChangeSeed
                    , onBlur actions.onCommitSeed
                    , Html.Events.on "keydown" keyDecoder
                    , Tw.classes [ Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded, Tw.text_base, Tw.font_mono, Tw.text_right ]
                    , Attr.style "width" "7rem"
                    , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                    , Attr.style "color" Brand.legoWhite
                    ]
                    []

            else
                button
                    [ Attr.type_ "button"
                    , onClick actions.onStartSeedEdit
                    , Attr.attribute "aria-label" (RI18n.editSeedLabel lang data.map.seed)
                    , Attr.title (RI18n.editSeedLabel lang data.map.seed)
                    , Tw.classes [ Tw.px Th.s1_dot_5, Tw.py Th.s0_dot_5, Tw.rounded, Tw.text_xs, Tw.font_mono, Tw.cursor_pointer ]
                    , Attr.style "background" "transparent"
                    , Attr.style "color" "#94a3b8"
                    ]
                    [ text ("#" ++ String.fromInt data.map.seed) ]
    in
    div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_end, Tw.gap Th.s1, Tw.w_full ] ]
        [ seedView
        , button
            [ Attr.type_ "button"
            , onClick actions.onCopySeed
            , Attr.attribute "aria-label" (RI18n.copySeedLabel lang)
            , Attr.title (RI18n.copySeedLabel lang)
            , Tw.classes [ Tw.p Th.s1, Tw.rounded, Tw.cursor_pointer ]
            , Attr.style "background" "transparent"
            , Attr.style "color"
                (if data.shareCopiedNotice then
                    Brand.legoYellow

                 else
                    "#94a3b8"
                )
            ]
            [ (if data.shareCopiedNotice then
                FeatherIcons.check

               else
                FeatherIcons.copy
              )
                |> FeatherIcons.withSize 14
                |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
            ]
        ]


{-| A square icon-only button with its label as `aria-label` and tooltip.
`pressed` adds `aria-pressed` (and the selected look) to toggle buttons;
`accent` gives a yellow outline to the button that stands out in its row.
-}
iconButton :
    { label : String
    , pressed : Maybe Bool
    , onPress : msg
    , icon : Icon
    , accent : Bool
    }
    -> Html msg
iconButton cfg =
    let
        isSelected =
            cfg.pressed == Just True
    in
    button
        ([ Attr.type_ "button"
         , onClick cfg.onPress
         , Attr.attribute "aria-label" cfg.label
         , Attr.title cfg.label
         , Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_center, Tw.rounded_xl, Tw.cursor_pointer, Tw.transition_all, Tw.shrink_0 ]
         , Attr.style "width" "44px"
         , Attr.style "height" "44px"
         , Attr.style "background"
            (if isSelected then
                Brand.legoYellow

             else
                "rgba(15, 23, 42, 0.65)"
            )
         , Attr.style "color"
            (if isSelected then
                Brand.legoBlack

             else if cfg.accent then
                Brand.legoYellow

             else
                Brand.legoWhite
            )
         , Attr.style "border"
            (if isSelected || cfg.accent then
                "1px solid " ++ Brand.legoYellow

             else
                "1px solid rgba(255, 255, 255, 0.15)"
            )
         ]
            ++ (case cfg.pressed of
                    Just on ->
                        [ Attr.attribute "aria-pressed" (boolText on) ]

                    Nothing ->
                        []
               )
        )
        [ cfg.icon |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]


{-| The value after `current` in `options`, wrapping to the first; the first
when `current` is not among them.
-}
nextIn : List a -> a -> a
nextIn options current =
    case options of
        first :: _ ->
            nextAfter first current options

        [] ->
            current


{-| Walk `options` to the one after `current`; `first` when `current` is the
last one or missing.
-}
nextAfter : a -> a -> List a -> a
nextAfter first current options =
    case options of
        x :: rest ->
            if x == current then
                List.head rest |> Maybe.withDefault first

            else
                nextAfter first current rest

        [] ->
            first


{-| `"true"` or `"false"` for ARIA attributes.
-}
boolText : Bool -> String
boolText b =
    if b then
        "true"

    else
        "false"


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


{-| Player modes when no room is open: a local / network pair of icon
buttons, then the 1P / 2P pair (local) or the room buttons and the join field
(network).
-}
viewNormalPlayerOptions : GarageViewData -> GarageActions msg -> Html msg
viewNormalPlayerOptions data actions =
    let
        lang =
            data.language

        tabButton tab icon label =
            iconButton
                { label = label
                , pressed = Just (data.playerModeTab == tab)
                , onPress = actions.onSetPlayerModeTab tab
                , icon = icon
                , accent = False
                }
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.justify_center, Tw.items_center, Tw.gap Th.s3, Tw.w_full ] ]
        [ div
            [ Tw.classes [ Tw.flex, Tw.rounded_xl, Tw.p Th.s1, Tw.gap Th.s1 ]
            , Attr.attribute "role" "group"
            , Attr.style "background" "rgba(15, 23, 42, 0.85)"
            ]
            [ tabButton HotSeatTab FeatherIcons.monitor (RI18n.hotSeatTabLabel lang)
            , tabButton NetworkTab FeatherIcons.wifi (RI18n.networkTabLabel lang)
            ]
        , case data.playerModeTab of
            HotSeatTab ->
                viewHotSeatOptions data actions

            NetworkTab ->
                viewNetworkOptions data actions
        ]


{-| One or two players on this device, as a pair of icon buttons.
-}
viewHotSeatOptions : GarageViewData -> GarageActions msg -> Html msg
viewHotSeatOptions data actions =
    let
        lang =
            data.language

        modeButton mode icon label =
            iconButton
                { label = label
                , pressed = Just (data.raceMode == mode)
                , onPress = actions.onSetRaceMode mode
                , icon = icon
                , accent = False
                }
    in
    div [ Tw.classes [ Tw.flex, Tw.rounded_xl, Tw.p Th.s1, Tw.gap Th.s1 ], Attr.attribute "role" "group", Attr.style "background" "rgba(15, 23, 42, 0.85)" ]
        [ modeButton SinglePlayer FeatherIcons.user (RI18n.singlePlayerModeLabel lang)
        , modeButton SplitScreenTwoPlayer FeatherIcons.users (RI18n.splitScreenModeLabel lang)
        ]


{-| Network play: create a two-player room or a grid room, or join a room by
its code or its QR code.
-}
viewNetworkOptions : GarageViewData -> GarageActions msg -> Html msg
viewNetworkOptions data actions =
    let
        lang =
            data.language
    in
    div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.justify_center, Tw.items_center, Tw.gap Th.s2 ] ]
        [ iconButton
            { label = RI18n.createTwoPlayerRoomButton lang
            , pressed = Nothing
            , onPress = actions.onStartHostMultiplayer TwoPlayers
            , icon = FeatherIcons.userPlus
            , accent = True
            }
        , iconButton
            { label = RI18n.createGridRoomButton lang
            , pressed = Nothing
            , onPress = actions.onStartHostMultiplayer GridSession
            , icon = FeatherIcons.tv
            , accent = True
            }
        , div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s1 ] ]
            [ input
                [ Attr.type_ "text"
                , Attr.placeholder (RI18n.roomCodePlaceholder lang)
                , Attr.attribute "aria-label" (RI18n.roomCodePlaceholder lang)
                , Attr.value data.roomCode
                , onInput actions.onRoomCodeChanged
                , Tw.classes [ Tw.px Th.s3, Tw.rounded_lg, Tw.text_base, Tw.font_mono ]
                , Attr.style "height" "44px"
                , Attr.style "width" "8rem"
                , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                , Attr.style "color" Brand.legoWhite
                ]
                []
            , iconButton
                { label = RI18n.networkJoinTitle lang
                , pressed = Nothing
                , onPress = actions.onJoinMultiplayer data.roomCode
                , icon = FeatherIcons.logIn
                , accent = False
                }
            , iconButton
                { label = RI18n.scanQrLabel lang
                , pressed = Nothing
                , onPress = actions.onOpenQrScanner
                , icon = FeatherIcons.camera
                , accent = False
                }
            ]
        ]


{-| The menu card: a two-way switch, then the course panel or the world
panel.
-}
viewMenu :
    { a
        | course : Course
        , personalBest : Maybe LeaderboardEntry
        , language : Language
        , selection : Selection
        , seedText : String
        , seedEditing : Bool
        , menuTab : MenuTab
        , actions : ShipyardActions msg
    }
    -> Html msg
viewMenu cfg =
    div [ Attr.class "flex flex-col gap-3 p-3 rounded-xl bg-[#0d2235]/95 border-2 border-brand-yellow" ]
        [ div [ Attr.attribute "role" "group", Attr.class "flex p-1 rounded-lg bg-slate-900/80 border border-white/10" ]
            [ tabButton cfg.actions cfg.menuTab CourseTab (T.courseTab cfg.language)
            , tabButton cfg.actions cfg.menuTab WorldTab (T.worldTab cfg.language)
            ]
        , case cfg.menuTab of
            CourseTab ->
                viewCoursePanel cfg

            WorldTab ->
                viewWorldPanel cfg
        ]


tabButton : ShipyardActions msg -> MenuTab -> MenuTab -> String -> Html msg
tabButton actions current tab label =
    let
        selected =
            current == tab
    in
    button
        [ Attr.type_ "button"
        , onClick (actions.onSelectTab tab)
        , Attr.attribute "aria-pressed"
            (if selected then
                "true"

             else
                "false"
            )
        , Attr.class
            ("flex-1 min-h-[40px] rounded-md text-sm font-bold cursor-pointer transition-all "
                ++ (if selected then
                        "bg-brand-yellow text-brand"

                    else
                        "text-slate-300"
                   )
            )
        ]
        [ text label ]


{-| The course: the map's name, the best time on it with its stars (nothing
while there is none), and the previous map's button.
-}
viewCoursePanel :
    { a
        | course : Course
        , personalBest : Maybe LeaderboardEntry
        , language : Language
        , actions : ShipyardActions msg
    }
    -> Html msg
viewCoursePanel { course, personalBest, language, actions } =
    let
        labels =
            T.mapPickerLabels language
    in
    div [ Attr.class "flex flex-wrap items-center justify-between gap-2 min-h-[44px]" ]
        [ span [ Attr.class "flex items-center gap-2 text-sm font-bold text-brand-yellow" ]
            [ FeatherIcons.map |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
            , text (T.mapTitle language course.map)
            ]
        , div [ Attr.class "flex items-center gap-2" ]
            [ case personalBest of
                Just pb ->
                    span [ Attr.class "text-xs px-1.5 py-0.5 rounded font-mono bg-white/10 text-brand-yellow" ]
                        [ text ("⏱ " ++ T.personalBestLine language (Leaderboard.formatRaceTime pb.timeSeconds) (Physics.starRating course.targetSeconds pb.timeSeconds)) ]

                Nothing ->
                    text ""
            , case actions.onUsePreviousMap of
                Just msg ->
                    iconButton msg labels.previous FeatherIcons.rotateCcw

                Nothing ->
                    text ""
            ]
        ]


{-| The world: the dice, the biome, season and weather buttons and the wind,
then the seed number (or its field) with the copy-link button at the right.
-}
viewWorldPanel :
    { a
        | course : Course
        , language : Language
        , selection : Selection
        , seedText : String
        , seedEditing : Bool
        , actions : ShipyardActions msg
    }
    -> Html msg
viewWorldPanel { course, language, selection, seedText, seedEditing, actions } =
    let
        labels =
            T.mapPickerLabels language

        world =
            MapParams.resolve selection
    in
    div [ Attr.class "flex flex-col gap-1.5" ]
        [ div [ Attr.class "flex flex-wrap items-center gap-2" ]
            [ button
                [ Attr.type_ "button"
                , onClick actions.onRollWorld
                , Attr.title (T.rollWorld language)
                , Attr.attribute "aria-label" (T.rollWorld language)
                , Attr.class "w-12 h-12 flex items-center justify-center rounded-xl bg-brand-yellow text-brand cursor-pointer active:brightness-110"
                ]
                [ MenuIcons.dice |> FeatherIcons.withSize 26 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
            , span [ Attr.class "w-px h-8 bg-white/15 mx-1", Attr.attribute "aria-hidden" "true" ] []
            , worldButton language
                { label = labels.biome
                , valueName = labels.biomeName world.biome
                , pick = selection.biome
                , icon = MenuIcons.biome world.biome
                , onPick = actions.onPickBiome (cycle MapParams.allBiomes selection.biome world.biome)
                }
            , worldButton language
                { label = labels.season
                , valueName = labels.seasonName world.season
                , pick = selection.season
                , icon = MenuIcons.season world.season
                , onPick = actions.onPickSeason (cycle MapParams.allSeasons selection.season world.season)
                }
            , worldButton language
                { label = labels.weather
                , valueName = labels.weatherName world.weather
                , pick = selection.weather
                , icon = MenuIcons.weather world.weather
                , onPick = actions.onPickWeather (cycle (MapParams.allowedWeathers world.biome world.season) selection.weather world.weather)
                }
            , span
                [ Attr.attribute "role" "img"
                , Attr.attribute "aria-label" (T.windLabel language course.wind.speed)
                , Attr.title (T.windLabel language course.wind.speed)
                , Attr.class "w-12 h-12 flex flex-col items-center justify-center gap-0.5 rounded-xl bg-white/5 border border-white/10 text-sky-200"
                ]
                [ FeatherIcons.wind |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                , span [ Attr.class "text-[10px] leading-none font-mono", Attr.attribute "aria-hidden" "true" ]
                    [ text (String.fromInt (round course.wind.speed)) ]
                ]
            ]
        , div [ Attr.class "flex items-center justify-end gap-1" ]
            [ if seedEditing then
                input
                    [ Attr.type_ "text"
                    , Attr.id seedInputId
                    , Attr.value seedText
                    , onInput actions.onSeedInput
                    , onBlur actions.onCommitSeed
                    , Html.Events.on "keydown" (enterKey actions.onCommitSeed)
                    , Attr.attribute "aria-label" labels.seed
                    , Attr.attribute "autocomplete" "off"
                    , Attr.attribute "inputmode" "text"
                    , Attr.class "w-40 min-h-[32px] px-2 rounded-md bg-slate-900/80 border border-slate-400/30 text-xs font-mono text-white"
                    ]
                    []

              else
                button
                    [ Attr.type_ "button"
                    , onClick actions.onEditSeed
                    , Attr.title (T.editSeed language)
                    , Attr.attribute "aria-label" (labels.seed ++ " " ++ String.fromInt selection.seed ++ ". " ++ T.editSeed language)
                    , Attr.class "min-h-[32px] px-2 rounded-md text-[10px] font-mono text-slate-400 cursor-pointer hover:text-white"
                    ]
                    [ text ("#" ++ String.fromInt selection.seed) ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onCopyMapLink

                -- Keep the seed field's focus, so a click copies instead of
                -- first closing the field under the pointer.
                , Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( actions.onKeepFocus, True ))
                , Attr.title (T.copyLink language)
                , Attr.attribute "aria-label" (T.copyLink language)
                , Attr.class "w-8 h-8 flex items-center justify-center rounded-md text-slate-400 cursor-pointer hover:text-white"
                ]
                [ FeatherIcons.copy |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
            ]
        ]


{-| A world setting's button: the icon of the value the map has, marked when
it is chosen rather than left to chance. A press steps to the next value.
-}
worldButton :
    Language
    ->
        { label : String
        , valueName : String
        , pick : Pick a
        , icon : FeatherIcons.Icon
        , onPick : msg
        }
    -> Html msg
worldButton language { label, valueName, pick, icon, onPick } =
    let
        ( description, look ) =
            case pick of
                Any ->
                    ( label ++ ": " ++ valueName ++ ", " ++ T.byChance language
                    , "bg-white/5 border-white/15 border-dashed text-white"
                    )

                Exactly _ ->
                    ( label ++ ": " ++ valueName
                    , "bg-brand-yellow/20 border-brand-yellow/60 text-brand-yellow"
                    )
    in
    button
        [ Attr.type_ "button"
        , onClick onPick
        , Attr.title description
        , Attr.attribute "aria-label" description
        , Attr.class ("w-12 h-12 flex items-center justify-center rounded-xl border cursor-pointer transition-all " ++ look)
        ]
        [ icon |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]


{-| The next pick of a world setting: from chance to the first value, from a
value (the one the map has) to the next, and from the last back to chance.
-}
cycle : List a -> Pick a -> a -> Pick a
cycle values current shown =
    case current of
        Any ->
            List.head values |> Maybe.map Exactly |> Maybe.withDefault Any

        Exactly _ ->
            case dropUntil shown values of
                _ :: next :: _ ->
                    Exactly next

                _ ->
                    Any


dropUntil : a -> List a -> List a
dropUntil value values =
    case values of
        [] ->
            []

        first :: rest ->
            if first == value then
                values

            else
                dropUntil value rest


enterKey : msg -> Decode.Decoder msg
enterKey msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "not Enter"
            )


{-| Online play as icon buttons: host a room of two or of eight, or type a
room code (or paste a link) and join it.
-}
viewCrew : Language -> String -> ShipyardActions msg -> Html msg
viewCrew language roomCode actions =
    section
        [ Attr.attribute "aria-label" (T.crewHeading language)
        , Attr.class "flex flex-wrap items-center gap-2 p-3 rounded-xl bg-slate-900/65 border border-white/10"
        ]
        [ hostButton language actions TwoPlayers FeatherIcons.users
        , hostButton language actions GridSession FeatherIcons.tv
        , span [ Attr.class "w-px h-8 bg-white/15 mx-1", Attr.attribute "aria-hidden" "true" ] []
        , div [ Attr.class "flex flex-1 items-center gap-2 min-w-[180px]" ]
            [ input
                [ Attr.type_ "text"
                , Attr.value roomCode
                , onInput actions.onRoomCodeChanged
                , Attr.placeholder (T.roomCodePlaceholder language)
                , Attr.attribute "aria-label" (T.roomCodeLabel language)
                , Attr.attribute "autocomplete" "off"
                , Attr.class "flex-1 min-w-0 min-h-[44px] px-3 rounded-lg bg-slate-900/80 border border-slate-400/30 text-xs font-mono text-white placeholder:text-slate-500"
                ]
                []
            , iconButton (actions.onJoinMultiplayer roomCode) (T.joinRace language) FeatherIcons.logIn
            ]
        ]


{-| A host button for one session mode (a 2-player room or a grid session).
-}
hostButton : Language -> ShipyardActions msg -> SessionMode -> FeatherIcons.Icon -> Html msg
hostButton language actions mode icon =
    let
        label =
            T.hostTitle language (Multiplayer.sessionCapacity mode)
    in
    button
        [ Attr.type_ "button"
        , onClick (actions.onStartHostMultiplayer mode)
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.class "w-12 h-12 flex items-center justify-center rounded-xl bg-brand-yellow/20 border border-brand-yellow/40 text-brand-yellow cursor-pointer transition-all active:bg-brand-yellow/30"
        ]
        [ icon |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]


{-| A 44 px yellow icon-only button with its label for screen readers and as
its tooltip.
-}
iconButton : msg -> String -> FeatherIcons.Icon -> Html msg
iconButton msg label icon =
    button
        [ Attr.type_ "button"
        , onClick msg
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.class "w-11 h-11 shrink-0 flex items-center justify-center rounded-lg bg-brand-yellow text-brand cursor-pointer active:brightness-110"
        ]
        [ icon |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]


{-| A small bar of a boat's stat, with its icon; the stat's name is its
tooltip.
-}
statBar : FeatherIcons.Icon -> String -> Float -> Html msg
statBar icon label v =
    div
        [ Attr.attribute "role" "img"
        , Attr.attribute "aria-label" label
        , Attr.title label
        , Attr.class "flex items-center gap-1 w-16 shrink-0 text-slate-300"
        ]
        [ icon |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
        , div [ Attr.class "flex-1 h-1.5 rounded-full bg-slate-700 overflow-hidden" ]
            [ div [ Attr.class "h-full bg-sky-400 rounded-full", Attr.style "width" (String.fromInt (round (clamp 0 1 v * 100)) ++ "%") ] [] ]
        ]
