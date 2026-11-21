module Pages.Results exposing
    ( ResultsActions
    , ResultsViewData
    , view
    )

{-| Post-race podium, checkpoint split table, personal-best badge, and leaderboard (`FinishLeaderboardPage`).
-}

import FeatherIcons
import Html exposing (Html, div, h1, h2, span, text)
import Html.Attributes as Attr
import I18n exposing (Language)
import Racer.I18n as RI18n
import Racer.Leaderboard as Leaderboard exposing (Leaderboard)
import Racer.Types exposing (TrackId, VehicleSpec)
import Tailwind as Tw
import Tailwind.Theme as Th
import UI.Button as Button


{-| View-model snapshot for rendering race finish results.
-}
type alias ResultsViewData =
    { language : Language
    , trackId : TrackId
    , seed : Int
    , vehicle : VehicleSpec
    , finishTimeS : Float
    , topSpeedKmh : Float
    , finalIntegrityPct : Int
    , splitTimes : List Float
    , isNewBest : Bool
    , leaderboard : Leaderboard
    , shareCopiedNotice : Bool
    }


{-| Actions dispatched from the Finish & Results screen.
-}
type alias ResultsActions msg =
    { onRaceAgain : msg
    , onCopyShareLink : msg
    , onInspectGarage : msg
    , onBackToMenu : msg
    }


{-| Render the Finish / Leaderboard summary view.
-}
view : ResultsViewData -> ResultsActions msg -> Html msg
view data actions =
    let
        lang =
            data.language

        entries =
            Leaderboard.entriesForTrack data.trackId data.leaderboard
    in
    div
        [ Tw.classes [ Tw.min_h_screen, Tw.w_full, Tw.flex, Tw.items_center, Tw.justify_center, Tw.p Th.s4, Tw.overflow_y_auto ]
        , Attr.style "background" "linear-gradient(155deg, #0f172a 0%, #1e293b 55%, #0c4a6e 100%)"
        , Attr.style "color" "#f8fafc"
        ]
        [ div
            [ Tw.classes [ Tw.w_full, Tw.rounded_n2xl, Tw.p Th.s6, Tw.border, Tw.flex, Tw.flex_col, Tw.gap Th.s5 ]
            , Attr.style "max-width" "46rem"
            , Attr.style "background" "rgba(15, 23, 42, 0.86)"
            , Attr.style "border-color" "rgba(148, 163, 184, 0.28)"
            ]
            [ -- Header + PB badge
              div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.items_center, Tw.justify_between, Tw.gap Th.s3 ] ]
                [ div []
                    [ div [ Tw.classes [ Tw.text_xs, Tw.font_semibold ], Attr.style "color" "#38bdf8" ]
                        [ text (RI18n.trackTitle lang data.trackId ++ " • #" ++ String.fromInt data.seed) ]
                    , h1 [ Tw.classes [ Tw.text_n3xl, Tw.font_extrabold ] ] [ text (RI18n.finishedHeadline lang) ]
                    , div [ Tw.classes [ Tw.text_sm ], Attr.style "color" "#cbd5e1" ]
                        [ text (data.vehicle.name ++ " (" ++ String.fromInt (List.length data.vehicle.wheels) ++ " wheels)") ]
                    ]
                , if data.isNewBest then
                    span
                        [ Tw.classes [ Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_xl, Tw.text_xs, Tw.font_extrabold ]
                        , Attr.style "background" "#facc15"
                        , Attr.style "color" "#0f172a"
                        ]
                        [ text (RI18n.newRecordBadge lang) ]

                  else
                    text ""
                ]

            -- Big Time + Key Stats
            , div [ Tw.classes [ Tw.grid, Tw.grid_cols_2, Tw.gap Th.s3 ] ]
                [ metricBox "Finish Time" (Leaderboard.formatRaceTime data.finishTimeS) "#4ade80"
                , metricBox "Top Speed" (String.fromInt (round data.topSpeedKmh) ++ " km/h") "#38bdf8"
                , metricBox "Integrity Left" (String.fromInt data.finalIntegrityPct ++ "%") "#f8fafc"
                , metricBox (RI18n.garageSteeringLabel lang) (RI18n.steeringMechName lang data.vehicle.steering) "#facc15"
                ]

            -- Checkpoint Splits
            , if List.isEmpty data.splitTimes then
                text ""

              else
                div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5 ] ]
                    [ h2 [ Tw.classes [ Tw.text_sm, Tw.font_bold ], Attr.style "color" "#cbd5e1" ]
                        [ text "Checkpoint Splits" ]
                    , div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.gap Th.s2 ] ]
                        (List.indexedMap
                            (\idx t ->
                                div
                                    [ Tw.classes [ Tw.px Th.s3, Tw.py Th.s1_dot_5, Tw.rounded_lg, Tw.text_xs, Tw.tabular_nums ]
                                    , Attr.style "background" "rgba(30, 41, 59, 0.8)"
                                    ]
                                    [ text ("CP " ++ String.fromInt (idx + 1) ++ ": " ++ Leaderboard.formatRaceTime t) ]
                            )
                            data.splitTimes
                        )
                    ]

            -- Course Leaderboard
            , div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s2 ] ]
                [ div [ Tw.classes [ Tw.flex, Tw.items_center, Tw.gap Th.s2 ] ]
                    [ FeatherIcons.award |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                    , h2 [ Tw.classes [ Tw.text_sm, Tw.font_bold ] ] [ text "Local Leaderboard" ]
                    ]
                , div [ Tw.classes [ Tw.flex, Tw.flex_col, Tw.gap Th.s1_dot_5 ] ]
                    (List.indexedMap
                        (\idx e ->
                            div
                                [ Tw.classes [ Tw.flex, Tw.items_center, Tw.justify_between, Tw.px Th.s3, Tw.py Th.s2, Tw.rounded_lg, Tw.text_xs ]
                                , Attr.style "background" "rgba(30, 41, 59, 0.65)"
                                ]
                                [ span [ Tw.classes [ Tw.font_semibold ] ]
                                    [ text ("#" ++ String.fromInt (idx + 1) ++ " " ++ e.vehicleName ++ " • " ++ e.steeringLabel) ]
                                , span [ Tw.classes [ Tw.tabular_nums, Tw.font_bold ], Attr.style "color" "#4ade80" ]
                                    [ text (Leaderboard.formatRaceTime e.timeSeconds) ]
                                ]
                        )
                        entries
                    )
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

            -- Action Buttons
            , div [ Tw.classes [ Tw.flex, Tw.flex_wrap, Tw.gap Th.s3, Tw.pt Th.s2 ] ]
                [ Button.primaryOnDark actions.onRaceAgain (RI18n.raceAgainButton lang)
                , Button.secondary actions.onCopyShareLink (RI18n.shareRaceUrlButton lang)
                , Button.secondary actions.onInspectGarage (RI18n.inspectCarButton lang)
                , Button.secondary actions.onBackToMenu (RI18n.changeCarLabel lang)
                ]
            ]
        ]


metricBox : String -> String -> String -> Html msg
metricBox labelStr valueStr valueColor =
    div
        [ Tw.classes [ Tw.p Th.s3_dot_5, Tw.rounded_xl, Tw.border ]
        , Attr.style "background" "rgba(30, 41, 59, 0.75)"
        , Attr.style "border-color" "rgba(148, 163, 184, 0.2)"
        ]
        [ div [ Tw.classes [ Tw.text_xs ], Attr.style "color" "#94a3b8" ] [ text labelStr ]
        , div [ Tw.classes [ Tw.text_xl, Tw.font_extrabold, Tw.tabular_nums, Tw.mt Th.s0_dot_5 ], Attr.style "color" valueColor ]
            [ text valueStr ]
        ]
