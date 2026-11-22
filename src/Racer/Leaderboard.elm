module Racer.Leaderboard exposing
    ( Leaderboard
    , bestForTrack
    , decodeLeaderboard
    , empty
    , encodeLeaderboard
    , entriesForTrack
    , formatRaceTime
    , formatSplitDelta
    , recordRun
    )

{-| Local per-track personal best and leaderboard persistence codecs.
-}

import Game.Leaderboard as GameLB
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Racer.Types exposing (LeaderboardEntry, TrackId(..), trackIdFromString, trackIdToString)


{-| In-memory leaderboard collection persisted to `localStorage`.
-}
type alias Leaderboard =
    { entries : List LeaderboardEntry
    }


{-| Empty leaderboard.
-}
empty : Leaderboard
empty =
    { entries = [] }


{-| Decode persisted leaderboard JSON array from flags, ignoring malformed records.
-}
decodeLeaderboard : String -> Leaderboard
decodeLeaderboard rawJson =
    case Decode.decodeString (Decode.list entryDecoder) rawJson of
        Ok items ->
            { entries = List.sortBy .timeSeconds items |> List.take 40 }

        Err _ ->
            empty


{-| Serialize leaderboard to JSON string for `Game.Ports.saveLeaderboard`.
-}
encodeLeaderboard : Leaderboard -> String
encodeLeaderboard lb =
    Encode.encode 0 (Encode.list encodeEntry lb.entries)


{-| Record a finished run, returning the updated leaderboard and whether a new
track personal best was achieved.
-}
recordRun : LeaderboardEntry -> Leaderboard -> ( Leaderboard, Bool )
recordRun entry lb =
    let
        prevBest =
            bestForTrack entry.trackId lb
                |> Maybe.map .timeSeconds

        isNewBest =
            case prevBest of
                Nothing ->
                    True

                Just t ->
                    entry.timeSeconds < t

        nextEntries =
            (entry :: lb.entries)
                |> List.sortBy .timeSeconds
                |> keepTopPerTrack 6
    in
    ( { entries = nextEntries }, isNewBest )


{-| Return fastest recorded entry for a `TrackId`.
-}
bestForTrack : TrackId -> Leaderboard -> Maybe LeaderboardEntry
bestForTrack id lb =
    entriesForTrack id lb |> List.head


{-| Return top sorted leaderboard entries for a given track.
-}
entriesForTrack : TrackId -> Leaderboard -> List LeaderboardEntry
entriesForTrack id lb =
    lb.entries
        |> List.filter (\e -> e.trackId == id)
        |> List.sortBy .timeSeconds


{-| Format elapsed seconds as `MM:SS.cc` (e.g., `01:14.32`).
-}
formatRaceTime : Float -> String
formatRaceTime seconds =
    GameLB.formatRaceTime seconds


{-| Format checkpoint/PB delta seconds with sign (`-0.62 s` or `+0.41 s`).
-}
formatSplitDelta : Float -> String
formatSplitDelta delta =
    GameLB.formatDeltaSeconds delta


keepTopPerTrack : Int -> List LeaderboardEntry -> List LeaderboardEntry
keepTopPerTrack maxPerTrack entries =
    GameLB.keepTopPer .trackId maxPerTrack entries


encodeEntry : LeaderboardEntry -> Encode.Value
encodeEntry e =
    Encode.object
        [ ( "trackId", Encode.string (trackIdToString e.trackId) )
        , ( "vehicleName", Encode.string e.vehicleName )
        , ( "timeSeconds", Encode.float e.timeSeconds )
        , ( "topSpeedKmh", Encode.float e.topSpeedKmh )
        , ( "wheelCount", Encode.int e.wheelCount )
        , ( "steeringLabel", Encode.string e.steeringLabel )
        , ( "seed", Encode.int e.seed )
        ]


entryDecoder : Decoder LeaderboardEntry
entryDecoder =
    Decode.map7 LeaderboardEntry
        (Decode.field "trackId" Decode.string
            |> Decode.map (\s -> Maybe.withDefault FarmLane (trackIdFromString s))
        )
        (Decode.field "vehicleName" Decode.string)
        (Decode.field "timeSeconds" Decode.float)
        (Decode.field "topSpeedKmh" Decode.float)
        (Decode.field "wheelCount" Decode.int)
        (Decode.field "steeringLabel" Decode.string)
        (Decode.field "seed" Decode.int)
