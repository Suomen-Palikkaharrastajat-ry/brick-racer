module Pages.Menu exposing
    ( MenuActions
    , MenuViewData
    , view
    )

{-| Title & vehicle import screen for Brick Racer (`MenuPage`).

Lets players paste any `#b/...` or `#model=...` URL exported from `brick-play` or
`brick-studio`, open or drop `.ldr`/`.mpd`/`.io` files, pick from starter soapbox
builds, select one of the three mountain courses + seed, and view local PBs.

-}

import FeatherIcons
import Html exposing (Html, button, div, h1, h2, input, p, span, text, textarea)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import I18n exposing (Language(..))
import Racer.I18n as RI18n
import Racer.Ingest as Ingest
import Racer.Leaderboard as Leaderboard exposing (Leaderboard)
import Racer.Types exposing (PresetBuild, RaceMode(..), TrackId(..))
import Tailwind as Tw
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Th
import UI.Button as Button
import UI.Theme as Theme


{-| View-model snapshot for rendering the main menu.
-}
type alias MenuViewData =
    { language : Language
    , pasteText : String
    , pasteError : Maybe String
    , selectedTrack : TrackId
    , seed : Int
    , leaderboard : Leaderboard
    , audioMuted : Bool
    , raceMode : RaceMode
    }


{-| Actions emitted by the Menu screen back to `Main.update`.
-}
type alias MenuActions msg =
    { onPasteChanged : String -> msg
    , onSubmitPaste : msg
    , onRequestFileUpload : msg
    , onSelectPreset : PresetBuild -> msg
    , onSelectTrack : TrackId -> msg
    , onChangeSeed : String -> msg
    , onRandomSeed : msg
    , onToggleLanguage : msg
    , onToggleAudio : msg
    , onToggleRaceMode : msg
    }


{-| Render the responsive Menu screen.
-}
view : MenuViewData -> MenuActions msg -> Html msg
view data actions =
    let
        lang =
            data.language

        otherLangLabel =
            case lang of
                En ->
                    "FI"

                Fi ->
                    "EN"
    in
    div
        [ Tw.classes [ Tw.min_h_screen, Tw.w_full, Tw.p Th.s4, Bp.md [ Tw.p Th.s8 ], Tw.overflow_y_auto ]
        , Attr.style "background" "linear-gradient(155deg, #0f172a 0%, #1e293b 50%, #1e3a5f 100%)"
        , Attr.style "color" "#f8fafc"
        ]
        [ div
            [ Tw.classes [ Tw.w_full, Tw.mx_auto, Tw.flex, Tw.flex_col, Tw.gap Th.s6 ]
            , Attr.style "max-width" "72rem"
            ]
            [ -- Header
              div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.items_center, Tw.justify_between, Tw.gap Th.s4 ] ]
                [ div []
                    [ h1 [ Tw.classes [ Tw.text_n3xl, Bp.md [ Tw.text_n4xl ], Tw.font_extrabold ] ]
                        [ text (RI18n.menuTitle lang) ]
                    , p [ Tw.classes [ Tw.text_sm, Bp.md [ Tw.text_base ], Tw.mt Th.s1 ], Attr.style "color" "#cbd5e1" ]
                        [ text (RI18n.menuSubtitle lang) ]
                    ]
                , div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
                    [ Button.secondary actions.onToggleRaceMode
                        (if data.raceMode == SplitScreenTwoPlayer then
                            "🏁 2P SPLIT [2]"

                         else
                            "🏎️ 1 PLAYER [2]"
                        )
                    , Button.secondary actions.onToggleLanguage otherLangLabel
                    , Button.secondary actions.onToggleAudio
                        (if data.audioMuted then
                            RI18n.audioMutedLabel lang

                         else
                            RI18n.audioUnmutedLabel lang
                        )
                    ]
                ]

            -- Two-column content
            , div [ Tw.classes [ Tw.grid, Tw.grid_cols_1, Bp.lg [ Tw.grid_cols_12 ], Tw.gap Th.s6 ] ]
                [ -- Left: Vehicle Import & Starter Racers (7 cols)
                  div [ Tw.classes [ Bp.lg [ Tw.col_span_7 ], Tw.flex, Tw.flex_col, Tw.gap Th.s5 ] ]
                    [ viewImportCard data actions
                    , viewPresetsCard lang actions
                    ]

                -- Right: Course Picker & Personal Bests (5 cols)
                , div [ Tw.classes [ Bp.lg [ Tw.col_span_5 ], Tw.flex, Tw.flex_col, Tw.gap Th.s5 ] ]
                    [ viewCourseCard data actions
                    , viewLeaderboardCard data
                    ]
                ]
            , div [ Tw.classes [ Tw.text_xs, Tw.text_center, Tw.pt Th.s2 ], Attr.style "color" "#94a3b8" ]
                [ text (RI18n.controlsSummary lang) ]
            ]
        ]


viewImportCard : MenuViewData -> MenuActions msg -> Html msg
viewImportCard data actions =
    let
        lang =
            data.language
    in
    div
        [ Tw.classes [ Tw.rounded_n2xl, Tw.p Th.s5, Tw.border, Tw.flex, Tw.flex_col, Tw.gap Th.s3 ]
        , Attr.style "background" "rgba(15, 23, 42, 0.72)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.25)"
        ]
        [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
            [ FeatherIcons.link |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
            , h2 [ Tw.classes [ Tw.text_lg, Tw.font_bold ] ] [ text (RI18n.pasteButtonLabel lang) ]
            ]
        , textarea
            [ Attr.value data.pasteText
            , onInput actions.onPasteChanged
            , Attr.placeholder (RI18n.pastePlaceholder lang)
            , Attr.rows 3
            , Tw.classes [ Tw.w_full, Tw.rounded_xl, Tw.p Th.s3, Tw.text_sm, Tw.border ]
            , Attr.style "background" "rgba(2, 6, 23, 0.65)"
            , Attr.style "border-color" "rgba(148, 163, 184, 0.32)"
            , Attr.style "color" "#f8fafc"
            , Attr.style "resize" "vertical"
            ]
            []
        , case data.pasteError of
            Just err ->
                div
                    [ Tw.classes [ Tw.text_xs, Tw.px Th.s3, Tw.py Th.s2, Tw.rounded_lg ]
                    , Attr.style "background" "rgba(239, 68, 68, 0.2)"
                    , Attr.style "color" "#fca5a5"
                    ]
                    [ text err ]

            Nothing ->
                text ""
        , div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.items_center, Tw.gap Th.s3 ] ]
            [ Button.primaryOnDark actions.onSubmitPaste (RI18n.pasteButtonLabel lang)
            , Button.secondary actions.onRequestFileUpload (RI18n.dropZoneButton lang)
            , span [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ]
                [ text (RI18n.dropHintLabel lang) ]
            ]
        ]


viewPresetsCard : Language -> MenuActions msg -> Html msg
viewPresetsCard lang actions =
    div
        [ Tw.classes [ Tw.rounded_n2xl, Tw.p Th.s5, Tw.border, Tw.flex, Tw.flex_col, Tw.gap Th.s3 ]
        , Attr.style "background" "rgba(15, 23, 42, 0.72)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.25)"
        ]
        [ h2 [ Tw.classes [ Tw.text_lg, Tw.font_bold ] ] [ text (RI18n.defaultPresetSectionTitle lang) ]
        , div [ Tw.classes [ Tw.grid, Tw.grid_cols_1, Bp.sm [ Tw.grid_cols_2 ], Tw.gap Th.s3 ] ]
            (List.map (presetCard lang actions.onSelectPreset) Ingest.presets)
        ]


presetCard : Language -> (PresetBuild -> msg) -> PresetBuild -> Html msg
presetCard lang onSelect preset =
    let
        title =
            case lang of
                Fi ->
                    preset.nameFi

                En ->
                    preset.nameEn

        summary =
            case lang of
                Fi ->
                    preset.summaryFi

                En ->
                    preset.summaryEn
    in
    button
        [ Attr.type_ "button"
        , onClick (onSelect preset)
        , Tw.classes [ Tw.p Th.s3_dot_5, Tw.rounded_xl, Tw.border, Tw.flex, Tw.flex_col, Tw.gap Th.s1 ]
        , Attr.style "text-align" "left"
        , Attr.style "cursor" "pointer"
        , Attr.style "background" "rgba(30, 41, 59, 0.78)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.3)"
        , Attr.style "color" "#f8fafc"
        ]
        [ div [ Tw.classes [ Tw.font_bold, Tw.text_sm ] ] [ text title ]
        , div [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#cbd5e1" ] [ text summary ]
        ]


viewCourseCard : MenuViewData -> MenuActions msg -> Html msg
viewCourseCard data actions =
    let
        lang =
            data.language

        allTracks =
            [ PineRidge, QuarrySwitchbacks, AlpineGlacier ]
    in
    div
        [ Tw.classes [ Tw.rounded_n2xl, Tw.p Th.s5, Tw.border, Tw.flex, Tw.flex_col, Tw.gap Th.s3 ]
        , Attr.style "background" "rgba(15, 23, 42, 0.72)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.25)"
        ]
        [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.gap Th.s2 ] ]
            [ h2 [ Tw.classes [ Tw.text_lg, Tw.font_bold ] ] [ text "Mountain Courses" ]
            , div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
                [ span [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ] [ text (RI18n.seedLabel lang) ]
                , input
                    [ Attr.type_ "number"
                    , Attr.value (String.fromInt data.seed)
                    , onInput actions.onChangeSeed
                    , Tw.classes [ Tw.w Th.s20, Tw.px Th.s2, Tw.py Th.s1, Tw.rounded_lg, Tw.border, Tw.text_xs ]
                    , Attr.style "background" "rgba(2, 6, 23, 0.75)"
                    , Attr.style "border-color" "rgba(148, 163, 184, 0.3)"
                    , Attr.style "color" "#f8fafc"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onRandomSeed
                    , Tw.classes [ Tw.px Th.s2, Tw.py Th.s1, Tw.rounded_lg, Tw.border, Tw.text_xs ]
                    , Attr.style "cursor" "pointer"
                    , Attr.style "background" Theme.panelSubtleBackground
                    , Attr.style "color" Theme.textPrimary
                    ]
                    [ FeatherIcons.shuffle |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml [] ]
                ]
            ]
        , div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s2_dot_5 ] ]
            (List.map (trackOptionBtn lang data.selectedTrack actions.onSelectTrack) allTracks)
        ]


trackOptionBtn : Language -> TrackId -> (TrackId -> msg) -> TrackId -> Html msg
trackOptionBtn lang activeId onSelect id =
    let
        isSelected =
            id == activeId
    in
    button
        [ Attr.type_ "button"
        , onClick (onSelect id)
        , Tw.classes [ Tw.p Th.s3_dot_5, Tw.rounded_xl, Tw.border, Tw.flex, Tw.flex_col, Tw.gap Th.s1 ]
        , Attr.style "text-align" "left"
        , Attr.style "cursor" "pointer"
        , Attr.style "background"
            (if isSelected then
                "rgba(14, 165, 233, 0.22)"

             else
                "rgba(30, 41, 59, 0.68)"
            )
        , Attr.style "border-color"
            (if isSelected then
                "#38bdf8"

             else
                "rgba(148, 163, 184, 0.28)"
            )
        , Attr.style "color" "#f8fafc"
        ]
        [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between ] ]
            [ span [ Tw.classes [ Tw.font_bold, Tw.text_sm ] ] [ text (RI18n.trackTitle lang id) ]
            , if isSelected then
                FeatherIcons.checkCircle |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []

              else
                text ""
            ]
        , div [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#cbd5e1" ]
            [ text (RI18n.trackDescription lang id) ]
        ]


viewLeaderboardCard : MenuViewData -> Html msg
viewLeaderboardCard data =
    let
        entries =
            Leaderboard.entriesForTrack data.selectedTrack data.leaderboard
    in
    div
        [ Tw.classes [ Tw.rounded_n2xl, Tw.p Th.s5, Tw.border, Tw.flex, Tw.flex_col, Tw.gap Th.s2_dot_5 ]
        , Attr.style "background" "rgba(15, 23, 42, 0.72)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.25)"
        ]
        [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
            [ FeatherIcons.award |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
            , h2 [ Tw.classes [ Tw.text_base, Tw.font_bold ] ]
                [ text (RI18n.trackTitle data.language data.selectedTrack ++ " — Local Top Times") ]
            ]
        , if List.isEmpty entries then
            p [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ]
                [ text "No finishes recorded on this course yet." ]

          else
            div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5 ] ]
                (List.indexedMap
                    (\idx e ->
                        div
                            [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.text_xs, Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_lg ]
                            , Attr.style "background" "rgba(30, 41, 59, 0.6)"
                            ]
                            [ span [ Tw.classes [ Tw.font_semibold ] ]
                                [ text ("#" ++ String.fromInt (idx + 1) ++ " " ++ e.vehicleName) ]
                            , span [ Tw.classes [ Tw.tabular_nums, Tw.font_bold ], Attr.style "color" "#4ade80" ]
                                [ text (Leaderboard.formatRaceTime e.timeSeconds) ]
                            ]
                    )
                    entries
                )
        ]
