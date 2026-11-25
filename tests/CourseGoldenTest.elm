module CourseGoldenTest exposing (suite)

{-| Golden fingerprints of generated courses (plan §7.2).

Each fixed map code pins a small fingerprint of its course: the knot count,
the total length rounded to a metre, the `s` of the first and last
checkpoint (rounded to a metre), and the number of obstacles of each kind.

A failure here means a generator change altered what a map code produces.
That can be intentional, but then old leaderboard times no longer match the
course: bump `Game.MapParams.generatorVersion` (`g1`), update the expected
fingerprints below, and note the change in the changelog at the end of this
comment.

Changelog of the expected values:

  - g1: first pinned values.

-}

import Dict
import Expect
import Game.MapParams as Map
import Racer.Track as Track
import Test exposing (Test, describe, test)


type alias Fingerprint =
    { knots : Int
    , lengthM : Int
    , firstCheckpointS : Int
    , lastCheckpointS : Int
    , obstacles : List ( String, Int )
    }


fingerprint : String -> Maybe Fingerprint
fingerprint code =
    Map.fromCode code
        |> Maybe.map
            (\map ->
                let
                    config =
                        Track.trackConfigFor map

                    track =
                        Track.generate config

                    checkpointS =
                        List.map .s track.checkpoints
                in
                { knots = List.length config.knots
                , lengthM = round track.totalLengthM
                , firstCheckpointS = List.head checkpointS |> Maybe.withDefault -1 |> round
                , lastCheckpointS = List.reverse checkpointS |> List.head |> Maybe.withDefault -1 |> round
                , obstacles =
                    track.obstacles
                        |> List.foldl
                            (\o counts -> Dict.update (Debug.toString o.kind) (\n -> Just (Maybe.withDefault 0 n + 1)) counts)
                            Dict.empty
                        |> Dict.toList
                }
            )


golden : List ( String, Fingerprint )
golden =
    [ ( "farmland-summer-clear-1"
      , { knots = 8
        , lengthM = 974
        , firstCheckpointS = 234
        , lastCheckpointS = 938
        , obstacles =
            [ ( "Barn", 5 )
            , ( "Barrel", 3 )
            , ( "BrickGate", 2 )
            , ( "BrickPallet", 1 )
            , ( "BrickWall", 3 )
            , ( "Bush", 5 )
            , ( "Chalet", 1 )
            , ( "Cottage", 4 )
            , ( "CountdownBoard", 4 )
            , ( "Crate", 5 )
            , ( "FenceSection", 122 )
            , ( "GardenGnome", 3 )
            , ( "HayBale", 12 )
            , ( "LeafyTree", 13 )
            , ( "PineTree", 1 )
            , ( "PitGarage", 3 )
            , ( "Rock", 2 )
            , ( "Signpost", 2 )
            , ( "TileStack", 3 )
            ]
        }
      )
    , ( "boreal-winter-snow-48213"
      , { knots = 8
        , lengthM = 1059
        , firstCheckpointS = 256
        , lastCheckpointS = 1022
        , obstacles =
            [ ( "Barrel", 7 )
            , ( "BrickGate", 1 )
            , ( "BrickPallet", 5 )
            , ( "BrickWall", 4 )
            , ( "Bush", 15 )
            , ( "Chalet", 4 )
            , ( "Cottage", 6 )
            , ( "CountdownBoard", 1 )
            , ( "Crate", 10 )
            , ( "FenceSection", 15 )
            , ( "GardenGnome", 8 )
            , ( "HayBale", 1 )
            , ( "LeafyTree", 21 )
            , ( "PineTree", 60 )
            , ( "Rock", 23 )
            , ( "Signpost", 6 )
            , ( "SlopeBlock", 1 )
            , ( "TileStack", 3 )
            ]
        }
      )
    , ( "fell-autumn-rain-7"
      , { knots = 8
        , lengthM = 942
        , firstCheckpointS = 226
        , lastCheckpointS = 906
        , obstacles =
            [ ( "Barn", 4 )
            , ( "Barrel", 1 )
            , ( "BrickGate", 1 )
            , ( "BrickPallet", 2 )
            , ( "BrickWall", 3 )
            , ( "Bush", 15 )
            , ( "Cottage", 3 )
            , ( "CountdownBoard", 2 )
            , ( "Crate", 2 )
            , ( "FenceSection", 88 )
            , ( "GardenGnome", 4 )
            , ( "HayBale", 7 )
            , ( "LeafyTree", 4 )
            , ( "PineTree", 3 )
            , ( "PitGarage", 1 )
            , ( "Rock", 13 )
            , ( "Signpost", 2 )
            , ( "SlopeBlock", 1 )
            ]
        }
      )
    , ( "desert-spring-storm-99"
      , { knots = 8
        , lengthM = 933
        , firstCheckpointS = 224
        , lastCheckpointS = 898
        , obstacles =
            [ ( "Barrel", 19 )
            , ( "BrickGate", 2 )
            , ( "BrickWall", 3 )
            , ( "Bush", 5 )
            , ( "Chalet", 1 )
            , ( "Cottage", 5 )
            , ( "CountdownBoard", 2 )
            , ( "Crate", 12 )
            , ( "FenceSection", 24 )
            , ( "GardenGnome", 2 )
            , ( "PalmTree", 19 )
            , ( "PitGarage", 2 )
            , ( "Rock", 13 )
            , ( "Signpost", 1 )
            , ( "SlopeBlock", 2 )
            , ( "TileStack", 1 )
            ]
        }
      )
    ]


suite : Test
suite =
    describe "Racer course golden fingerprints"
        (List.map
            (\( code, expected ) ->
                test code <|
                    \_ ->
                        -- The code must survive `fromCode` unchanged, so no
                        -- weather coercion hides behind a pinned value.
                        Expect.equal ( Just code, Just expected )
                            ( Map.fromCode code |> Maybe.map Map.toCode, fingerprint code )
            )
            golden
        )
