module TrackTest exposing (suite)

{-| Unit tests for `Racer.Track` procedural spline generation, heightmap continuity,
deterministic seeding, course length/steepness, and roadside scenery clearance.
-}

import Array
import Expect
import Math.Vector3 as Vec3
import Racer.Track as Track
import Racer.Types exposing (SurfaceType(..), TrackId(..), allTrackIds, trackIdFromString, trackIdToString)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Track generation and spatial queries"
        [ describe "All three track presets"
            [ verifyCourseMonotonicAndFinite "FarmLane" FarmLane 42
            , verifyCourseMonotonicAndFinite "ForestTrail" ForestTrail 77
            , verifyCourseMonotonicAndFinite "MeadowPath" MeadowPath 109
            ]
        , describe "Courses are short and steep"
            (List.map verifyShortAndSteep allTrackIds)
        , test "Track ids round-trip and retired mountain ids map to countryside courses" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (List.map Just allTrackIds) (List.map (trackIdToString >> trackIdFromString) allTrackIds)
                    , \_ -> Expect.equal (Just FarmLane) (trackIdFromString "pine-ridge")
                    , \_ -> Expect.equal (Just ForestTrail) (trackIdFromString "quarry-switchbacks")
                    , \_ -> Expect.equal (Just MeadowPath) (trackIdFromString "alpine-glacier")
                    ]
                    ()
        , test "Deterministic generation produces identical samples and obstacles for same seed" <|
            \_ ->
                let
                    cfg =
                        Track.trackConfigFor FarmLane 1234

                    t1 =
                        Track.generate cfg

                    t2 =
                        Track.generate cfg
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 1.0e-6) t1.totalLengthM t2.totalLengthM
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) t1.totalDropM t2.totalDropM
                    , \_ -> Expect.equal (List.length t1.obstacles) (List.length t2.obstacles)
                    , \_ -> Expect.equal (Array.length t1.samples) (Array.length t2.samples)
                    ]
                    ()
        , test "Heightfield transitions continuously across the unpaved roadbed and grass verge" <|
            \_ ->
                let
                    track =
                        Track.generate (Track.trackConfigFor FarmLane 7)

                    midSample =
                        Track.splineSampleAtS track (track.totalLengthM * 0.45)

                    insideEdge =
                        Vec3.add midSample.center (Vec3.scale (midSample.halfWidth - 0.05) midSample.right)

                    justOutsideEdge =
                        Vec3.add midSample.center (Vec3.scale (midSample.halfWidth + 0.05) midSample.right)

                    qIn =
                        Track.sampleAtHinted midSample.s track (Vec3.getX insideEdge) (Vec3.getZ insideEdge)

                    qOut =
                        Track.sampleAtHinted midSample.s track (Vec3.getX justOutsideEdge) (Vec3.getZ justOutsideEdge)
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.25) qIn.height qOut.height
                    , \_ -> Expect.notEqual MeadowGrass qIn.surface
                    , \_ -> Expect.notEqual RoughVerge qIn.surface
                    , \_ -> Expect.equal RoughVerge qOut.surface
                    ]
                    ()
        , test "sampleAtHinted agrees closely with full-scan sampleAt" <|
            \_ ->
                let
                    track =
                        Track.generate (Track.trackConfigFor MeadowPath 5)

                    smp =
                        Track.splineSampleAtS track 220.0

                    p =
                        Vec3.add smp.center (Vec3.scale 2.1 smp.right)

                    x =
                        Vec3.getX p

                    z =
                        Vec3.getZ p

                    fullQ =
                        Track.sampleAt track x z

                    hintQ =
                        Track.sampleAtHinted 215.0 track x z
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.01) fullQ.height hintQ.height
                    , \_ -> Expect.within (Expect.Absolute 0.05) fullQ.nearestS hintQ.nearestS
                    , \_ -> Expect.within (Expect.Absolute 0.01) (Vec3.length hintQ.normal) 1.0
                    ]
                    ()
        , describe "Roadside scenery stays off the roadbed and clear of the start"
            (List.map verifySceneryClearance allTrackIds)
        , test "removeObstacles drops smashed pieces from the list and the collision buckets" <|
            \_ ->
                let
                    track =
                        Track.generate (Track.trackConfigFor FarmLane 3)

                    victims =
                        List.take 3 track.obstacles |> List.map .id

                    after =
                        Track.removeObstacles victims track

                    bucketIds =
                        Array.toList after.obstacleBuckets |> List.concat |> List.map .id
                in
                Expect.all
                    [ \_ -> Expect.equal (List.length track.obstacles - 3) (List.length after.obstacles)
                    , \_ -> Expect.equal False (List.any (\i -> List.member i victims) bucketIds)
                    , \_ -> Expect.equal (List.length after.obstacles) (List.length bucketIds)
                    ]
                    ()
        ]


verifyShortAndSteep : TrackId -> Test
verifyShortAndSteep id =
    test (trackIdToString id ++ " is 0.35-0.65 km long with at least 25% average grade") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id 11)
            in
            Expect.all
                [ \_ -> Expect.atLeast 350.0 track.totalLengthM
                , \_ -> Expect.atMost 650.0 track.totalLengthM
                , \_ -> Expect.atLeast 0.25 (track.totalDropM / track.totalLengthM)
                ]
                ()


verifySceneryClearance : TrackId -> Test
verifySceneryClearance id =
    test (trackIdToString id ++ " scenery is varied, off the road and clear of the start pad") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id 99)

                badStart =
                    List.filter (\o -> o.s < 20.0) track.obstacles

                badRoadbed =
                    List.filter
                        (\o ->
                            let
                                q =
                                    Track.sampleAt track (Vec3.getX o.position) (Vec3.getZ o.position)
                            in
                            abs q.roadOffsetM - o.radiusM < q.halfWidthM + 0.25
                        )
                        track.obstacles

                kinds =
                    List.foldl
                        (\o acc ->
                            if List.member o.kind acc then
                                acc

                            else
                                o.kind :: acc
                        )
                        []
                        track.obstacles
            in
            Expect.all
                [ \_ -> Expect.equal [] badStart
                , \_ -> Expect.equal [] badRoadbed
                , \_ -> Expect.greaterThan 25 (List.length track.obstacles)
                , \_ -> Expect.atLeast 5 (List.length kinds)
                ]
                ()


verifyCourseMonotonicAndFinite : String -> TrackId -> Int -> Test
verifyCourseMonotonicAndFinite label id seed =
    test (label ++ " descends monotonically, stays finite, and has unit normals") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id seed)

                sampleList =
                    Array.toList track.samples

                startY =
                    Vec3.getY track.startPosition

                endY =
                    List.reverse sampleList
                        |> List.head
                        |> Maybe.map (.center >> Vec3.getY)
                        |> Maybe.withDefault startY

                allFinite =
                    List.all
                        (\s ->
                            not (isNaN (Vec3.getX s.center))
                                && not (isNaN (Vec3.getY s.center))
                                && not (isNaN (Vec3.getZ s.center))
                                && abs (Vec3.length s.normal - 1.0)
                                < 0.05
                                && abs (Vec3.length s.tangent - 1.0)
                                < 0.05
                        )
                        sampleList

                dropsEveryStep =
                    pairwiseDecreasing (List.map (.center >> Vec3.getY) sampleList)
            in
            Expect.all
                [ \_ -> Expect.equal True allFinite
                , \_ -> Expect.equal True dropsEveryStep
                , \_ -> Expect.greaterThan 80.0 (startY - endY)
                , \_ -> Expect.equal 4 (List.length track.checkpoints)
                ]
                ()


pairwiseDecreasing : List Float -> Bool
pairwiseDecreasing ys =
    case ys of
        a :: b :: rest ->
            b <= a + 0.02 && pairwiseDecreasing (b :: rest)

        _ ->
            True
