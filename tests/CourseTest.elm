module CourseTest exposing (suite)

{-| Invariants of the seeded course blueprints (`Racer.Course`), checked over
every biome and a sweep of seeds.
-}

import Expect
import Game.MapParams as Map exposing (Biome(..), MapParams, Season(..), WeatherKind(..))
import Racer.Course as Course exposing (CourseParams)
import Racer.Track as Track
import Racer.Types exposing (BlueprintKnot, SegmentFeature(..))
import Test exposing (Test, describe, test)


seeds : List Int
seeds =
    List.range 1 40


maps : List MapParams
maps =
    List.concatMap
        (\biome ->
            List.map (\seed -> { biome = biome, season = Spring, weather = Clear, seed = seed }) seeds
        )
        [ Farmland, Boreal, Fell, Desert ]


violations : (MapParams -> CourseParams -> List BlueprintKnot -> List String) -> List String
violations check =
    List.concatMap
        (\map ->
            let
                cp =
                    Course.courseParams map
            in
            check map cp (Course.generateKnots 0 map cp)
                |> List.map (\message -> Map.toCode map ++ ": " ++ message)
        )
        maps


suite : Test
suite =
    describe "Racer.Course"
        [ test "knot count is 8 or 9 and matches the course" <|
            \_ ->
                Expect.equal []
                    (violations
                        (\_ cp knots ->
                            if cp.knotCount >= 8 && cp.knotCount <= 9 && List.length knots == cp.knotCount then
                                []

                            else
                                [ "knot count " ++ String.fromInt (List.length knots) ]
                        )
                    )
        , test "the run-in starts at 0 and the finish ends at 1" <|
            \_ ->
                Expect.equal []
                    (violations
                        (\_ _ knots ->
                            case ( List.head knots, List.reverse knots |> List.head ) of
                                ( Just first, Just last ) ->
                                    List.filterMap identity
                                        [ if first.sFrac == 0 && first.xOffset == 0 then
                                            Nothing

                                          else
                                            Just "run-in is not pinned"
                                        , if last.sFrac == 1 then
                                            Nothing

                                          else
                                            Just "finish is not at sFrac 1"
                                        ]

                                _ ->
                                    [ "no knots" ]
                        )
                    )
        , test "forward progress strictly increases" <|
            \_ ->
                Expect.equal []
                    (violations
                        (\_ _ knots ->
                            if pairs knots |> List.all (\( a, b ) -> b.zProgress > a.zProgress) then
                                []

                            else
                                [ "zProgress does not increase" ]
                        )
                    )
        , test "the drop never goes up and ends at 1" <|
            \_ ->
                Expect.equal []
                    (violations
                        (\_ _ knots ->
                            let
                                monotonic =
                                    pairs knots |> List.all (\( a, b ) -> b.yDropFrac >= a.yDropFrac)

                                ends =
                                    List.reverse knots |> List.head |> Maybe.map (\k -> abs (k.yDropFrac - 1) < 1.0e-6) |> Maybe.withDefault False
                            in
                            if monotonic && ends then
                                []

                            else
                                [ "drop is not monotonic or does not end at 1" ]
                        )
                    )
        , test "sideways steps stay within 0.55 m per metre forward" <|
            \_ ->
                Expect.equal []
                    (violations
                        (\_ cp knots ->
                            let
                                tooSteep ( a, b ) =
                                    abs (b.xOffset - a.xOffset) > 0.55 * (b.zProgress - a.zProgress) * cp.lengthM + 1.0e-6
                            in
                            if List.any tooSteep (pairs knots) then
                                [ "sideways step too steep" ]

                            else
                                []
                        )
                    )
        , test "washes and launch ramps match the biome and never touch" <|
            \_ ->
                Expect.equal []
                    (violations
                        (\_ cp knots ->
                            let
                                indexed =
                                    List.indexedMap Tuple.pair knots

                                featureIndices pick =
                                    indexed
                                        |> List.filterMap (\( i, k ) -> Maybe.andThen pick k.feature |> Maybe.map (\_ -> i))

                                washes =
                                    featureIndices
                                        (\f ->
                                            case f of
                                                RoughWash _ ->
                                                    Just ()

                                                _ ->
                                                    Nothing
                                        )

                                ramps =
                                    featureIndices
                                        (\f ->
                                            case f of
                                                LaunchRamp _ ->
                                                    Just ()

                                                _ ->
                                                    Nothing
                                        )

                                allIndices =
                                    List.sort (washes ++ ramps)

                                touching =
                                    pairs allIndices |> List.any (\( a, b ) -> b - a <= 1)
                            in
                            List.filterMap identity
                                [ if List.length washes == cp.washes then
                                    Nothing

                                  else
                                    Just "wash count"
                                , if List.length ramps == cp.ramps then
                                    Nothing

                                  else
                                    Just "ramp count"
                                , if touching then
                                    Just "features touch"

                                  else
                                    Nothing
                                ]
                        )
                    )
        , test "halfWidth stays near the course's width, and the finish is wider" <|
            \_ ->
                Expect.equal []
                    (violations
                        (\_ cp knots ->
                            let
                                widths =
                                    List.map .halfWidth knots

                                inRange w =
                                    w >= cp.halfWidthM - 0.25 && w <= cp.halfWidthM + 0.45
                            in
                            if List.all inRange widths then
                                []

                            else
                                [ "halfWidth out of range" ]
                        )
                    )
        , test "every seed of every biome passes validation" <|
            \_ ->
                Expect.equal []
                    (List.concatMap
                        (\id ->
                            List.filterMap
                                (\seed ->
                                    case Track.validationIssues (Track.generate (Track.trackConfigFor { biome = id, season = Summer, weather = Clear, seed = seed })) of
                                        [] ->
                                            Nothing

                                        issues ->
                                            Just (Debug.toString id ++ " seed " ++ String.fromInt seed ++ ": " ++ String.join ", " issues)
                                )
                                (List.range 1 40 ++ [ 42, 77, 99, 109, 1234 ])
                        )
                        Map.allBiomes
                    )
        , test "the same map always gives the same course" <|
            \_ ->
                Expect.equal []
                    (List.filter
                        (\map ->
                            let
                                cp =
                                    Course.courseParams map
                            in
                            Course.generateKnots 0 map cp /= Course.generateKnots 0 map cp
                        )
                        maps
                    )
        ]


pairs : List a -> List ( a, a )
pairs items =
    case items of
        a :: ((b :: _) as rest) ->
            ( a, b ) :: pairs rest

        _ ->
            []
