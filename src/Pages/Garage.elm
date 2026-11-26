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
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onBlur, onClick, onInput)
import I18n exposing (Language(..))
import Json.Decode as Decode
import QRCode
import Racer.Analyze exposing (CarMeshBundle)
import Racer.I18n as RI18n
import Racer.Leaderboard exposing (Leaderboard)
import Racer.MenuIcons as MenuIcons
import Racer.Render as Render
import Racer.TerrainShader exposing (TerrainVertex)
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
                , viewTrackSection data actions
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


{-| Track randomization: dice icon to randomize track, then three icons
visualizing the result (biome, season, weather) that cycle their feature axes on
click; below them, right-aligned map code + seed without explanation and small
copy icon button. Clicking the code + seed opens inline editing.
-}
viewTrackSection : GarageViewData -> GarageActions msg -> Html msg
viewTrackSection data actions =
    let
        lang =
            data.language

        labels =
            RI18n.mapPickerLabels lang

        map =
            data.map

        nextWeather =
            nextIn (Map.allowedWeathers map.biome map.season) map.weather
    in
    div
        [ Tw.classes
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap Th.s2
            , Tw.p Th.s3
            , Tw.rounded_2xl
            , Tw.border
            , Tw.w_full
            ]
        , Attr.style "background" "rgba(13, 34, 53, 0.95)"
        , Attr.style "border-color" "rgba(255, 255, 255, 0.1)"
        ]
        [ -- Icon row: Dice to randomize, then Biome, Season, Weather
          div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.items_center, Tw.justify_center, Bp.sm [ Tw.justify_start ], Tw.gap Th.s3 ] ]
            [ iconButton
                { label = RI18n.randomWorldLabel lang
                , pressed = Nothing
                , onPress = actions.onRandomSeed
                , icon = MenuIcons.dice
                , accent = True
                }
            , span [ Tw.classes [ Tw.w_px, Tw.h Th.s8, Tw.mx Th.s1 ], Attr.style "background" "rgba(255, 255, 255, 0.15)", Attr.attribute "aria-hidden" "true" ] []
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
            ]
        , -- Right-aligned code + seed row without explanation
          viewSeedRow data actions
        ]


{-| The right-aligned map code + seed without explanation under the icon row.
Clicking the code + seed opens inline editing. Next to it is a small icon button
to copy the map code to clipboard.
-}
viewSeedRow : GarageViewData -> GarageActions msg -> Html msg
viewSeedRow data actions =
    let
        lang =
            data.language

        codeText =
            Map.toCode data.map

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
                    , Attr.attribute "inputmode" "text"
                    , Attr.attribute "aria-label" (RI18n.mapPickerLabels lang).seed
                    , Attr.value data.seedText
                    , onInput actions.onChangeSeed
                    , onBlur actions.onCommitSeed
                    , Html.Events.on "keydown" keyDecoder
                    , Tw.classes [ Tw.px Th.s2_dot_5, Tw.py Th.s1, Tw.rounded_md, Tw.text_xs, Tw.font_mono, Tw.text_right ]
                    , Attr.style "min-width" "14rem"
                    , Attr.style "max-width" "100%"
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
                    , Tw.classes [ Tw.px Th.s2, Tw.py Th.s1, Tw.rounded_md, Tw.text_xs, Tw.font_mono, Tw.cursor_pointer, Tw.transition_colors ]
                    , Attr.style "background" "transparent"
                    , Attr.style "color" "#94a3b8"
                    ]
                    [ text codeText ]
    in
    div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_end, Tw.gap Th.s1_dot_5, Tw.w_full, Tw.pt Th.s1 ] ]
        [ seedView
        , button
            [ Attr.type_ "button"
            , onClick actions.onCopySeed
            , Attr.attribute "aria-label" (RI18n.copySeedLabel lang)
            , Attr.title (RI18n.copySeedLabel lang)
            , Tw.classes [ Tw.p Th.s1, Tw.rounded, Tw.cursor_pointer, Tw.transition_colors ]
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
        [ -- Room header: icon + room code + copy & leave/start icon buttons
          div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.items_center, Tw.justify_center, Tw.gap Th.s2, Tw.w_full ] ]
            ([ span
                [ Tw.classes [ Tw.text_lg, Tw.font_mono, Tw.font_black, Tw.tracking_widest, Tw.px Th.s3, Tw.py Th.s1, Tw.rounded_lg, Tw.border ]
                , Attr.style "color" Brand.legoYellow
                , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                , Attr.style "border-color" "rgba(250, 200, 10, 0.3)"
                ]
                [ text room.roomId ]
             , iconButton
                { label = RI18n.copyRoomLinkButton lang
                , pressed = Just data.shareCopiedNotice
                , onPress = actions.onCopyMultiplayerLink
                , icon =
                    if data.shareCopiedNotice then
                        FeatherIcons.check

                    else
                        FeatherIcons.copy
                , accent = False
                }
             , iconButton
                { label = RI18n.leaveRoomButton lang
                , pressed = Nothing
                , onPress = actions.onLeaveMultiplayer
                , icon = FeatherIcons.logOut
                , accent = False
                }
             ]
                ++ (if isHost && readyToStart then
                        [ iconButton
                            { label = RI18n.startMultiplayerRaceButton lang
                            , pressed = Nothing
                            , onPress = actions.onStartMultiplayerCountdown
                            , icon = FeatherIcons.play
                            , accent = True
                            }
                        ]

                    else
                        []
                   )
            )
        , if isHost then
            viewQrCode roomUrl

          else
            text ""
        , -- Player Roster
          div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap Th.s1_dot_5, Tw.w_full ], Attr.style "max-width" "24rem" ]
            (List.map
                (\p ->
                    div
                        [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_lg, Tw.border, Tw.w_full ]
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
                            [ (if p.vehicleReady then
                                FeatherIcons.check

                               else
                                FeatherIcons.clock
                              )
                                |> FeatherIcons.withSize 14
                                |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                            ]
                        ]
                )
                room.players
            )
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
