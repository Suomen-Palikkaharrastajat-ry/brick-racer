module Racer.Leaderboard exposing
    ( Leaderboard
    , bestForMap
    , decodeLeaderboard
    , empty
    , encodeLeaderboard
    , entriesForMap
    , formatRaceTime
    , formatSplitDelta
    , recordRun
    )

{-| Local per-map personal best and leaderboard persistence codecs.

Entries are keyed by `Game.MapParams.leaderboardKey`, one board per exact map
code. Entries without a key of the current generator version (such as the
old per-track entries) are dropped when the board is read.

-}

import Game.Leaderboard as GameLB
import Game.MapParams as Map exposing (MapParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Racer.Types exposing (LeaderboardEntry)


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


{-| Decode persisted leaderboard JSON array from flags. Each record is
decoded on its own and malformed or outdated ones are dropped, so one bad
record cannot wipe the board.
-}
decodeLeaderboard : String -> Leaderboard
decodeLeaderboard rawJson =
    case Decode.decodeString (Decode.list Decode.value) rawJson of
        Ok values ->
            { entries =
                values
                    |> List.filterMap (Decode.decodeValue entryDecoder >> Result.toMaybe)
                    |> keepTopPerMap maxPerMap
                    |> capMaps maxMaps
            }

        Err _ ->
            empty


{-| Serialize leaderboard to JSON string for `Game.Ports.saveLeaderboard`.
-}
encodeLeaderboard : Leaderboard -> String
encodeLeaderboard lb =
    Encode.encode 0 (Encode.list encodeEntry lb.entries)


{-| Record a finished run, returning the updated leaderboard and whether a new
personal best on the map was achieved.
-}
recordRun : LeaderboardEntry -> Leaderboard -> ( Leaderboard, Bool )
recordRun entry lb =
    let
        prevBest =
            bestForKey entry.mapCode lb
                |> Maybe.map .timeSeconds

        isNewBest =
            case prevBest of
                Nothing ->
                    True

                Just t ->
                    entry.timeSeconds < t

        nextEntries =
            -- Newest map first, so the cap drops the maps raced longest ago.
            List.sortBy .timeSeconds (entry :: List.filter (\e -> e.mapCode == entry.mapCode) lb.entries)
                ++ List.filter (\e -> e.mapCode /= entry.mapCode) lb.entries
                |> keepTopPerMap maxPerMap
                |> capMaps maxMaps
    in
    ( { entries = nextEntries }, isNewBest )


{-| Return fastest recorded entry for a map.
-}
bestForMap : MapParams -> Leaderboard -> Maybe LeaderboardEntry
bestForMap map lb =
    bestForKey (Map.leaderboardKey map) lb


bestForKey : String -> Leaderboard -> Maybe LeaderboardEntry
bestForKey key lb =
    entriesForKey key lb |> List.head


{-| Return top sorted leaderboard entries for a map.
-}
entriesForMap : MapParams -> Leaderboard -> List LeaderboardEntry
entriesForMap map lb =
    entriesForKey (Map.leaderboardKey map) lb


entriesForKey : String -> Leaderboard -> List LeaderboardEntry
entriesForKey key lb =
    lb.entries
        |> List.filter (\e -> e.mapCode == key)
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


{-| Runs kept per map.
-}
maxPerMap : Int
maxPerMap =
    6


{-| Distinct maps kept on the board.
-}
maxMaps : Int
maxMaps =
    60


keepTopPerMap : Int -> List LeaderboardEntry -> List LeaderboardEntry
keepTopPerMap maxPer entries =
    GameLB.keepTopPer .mapCode maxPer entries


{-| Keep the entries of the first `maxCount` distinct maps, in list order.
-}
capMaps : Int -> List LeaderboardEntry -> List LeaderboardEntry
capMaps maxCount entries =
    let
        step entry ( keys, kept ) =
            if List.member entry.mapCode keys then
                ( keys, entry :: kept )

            else if List.length keys < maxCount then
                ( entry.mapCode :: keys, entry :: kept )

            else
                ( keys, kept )
    in
    List.foldl step ( [], [] ) entries
        |> Tuple.second
        |> List.reverse


encodeEntry : LeaderboardEntry -> Encode.Value
encodeEntry e =
    Encode.object
        [ ( "mapCode", Encode.string e.mapCode )
        , ( "vehicleName", Encode.string e.vehicleName )
        , ( "timeSeconds", Encode.float e.timeSeconds )
        , ( "topSpeedKmh", Encode.float e.topSpeedKmh )
        , ( "wheelCount", Encode.int e.wheelCount )
        , ( "steeringLabel", Encode.string e.steeringLabel )
        ]


entryDecoder : Decoder LeaderboardEntry
entryDecoder =
    Decode.map6 LeaderboardEntry
        (Decode.field "mapCode" Decode.string
            |> Decode.andThen
                (\key ->
                    if String.startsWith (Map.generatorVersion ++ ":") key then
                        Decode.succeed key

                    else
                        Decode.fail "old map code"
                )
        )
        (Decode.field "vehicleName" Decode.string)
        (Decode.field "timeSeconds" Decode.float)
        (Decode.field "topSpeedKmh" Decode.float)
        (Decode.field "wheelCount" Decode.int)
        (Decode.field "steeringLabel" Decode.string)
