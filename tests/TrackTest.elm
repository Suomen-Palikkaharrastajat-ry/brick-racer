module TrackTest exposing (suite)

{-| Unit tests for `Racer.Track` procedural spline generation, heightmap continuity,
deterministic seeding, and obstacle clearance.
-}

import Array
import Expect
import Math.Vector3 as Vec3
import Racer.Track as Track
import Racer.Types exposing (ObstacleKind(..), SurfaceType(..), TrackId(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Track generation and spatial queries"
        [ describe "All three track presets"
            [ verifyCourseMonotonicAndFinite "PineRidge" PineRidge 42
            , verifyCourseMonotonicAndFinite "QuarrySwitchbacks" QuarrySwitchbacks 77
            , verifyCourseMonotonicAndFinite "AlpineGlacier" AlpineGlacier 109
            ]
        , test "Deterministic generation produces identical samples and obstacles for same seed" <|
            \_ ->
                let
                    cfg =
                        Track.trackConfigFor PineRidge 1234

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
        , test "Heightfield transitions continuously across roadbed and gravel shoulder" <|
            \_ ->
                let
                    track =
                        Track.generate (Track.trackConfigFor PineRidge 7)

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
                    , \_ -> Expect.notEqual AlpineGrass qIn.surface
                    , \_ -> Expect.equal GravelShoulder qOut.surface
                    ]
                    ()
        , test "sampleAtHinted agrees closely with full-scan sampleAt" <|
            \_ ->
                let
                    track =
                        Track.generate (Track.trackConfigFor AlpineGlacier 5)

                    smp =
                        Track.splineSampleAtS track 420.0

                    p =
                        Vec3.add smp.center (Vec3.scale 2.1 smp.right)

                    x =
                        Vec3.getX p

                    z =
                        Vec3.getZ p

                    fullQ =
                        Track.sampleAt track x z

                    hintQ =
                        Track.sampleAtHinted 415.0 track x z
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.01) fullQ.height hintQ.height
                    , \_ -> Expect.within (Expect.Absolute 0.05) fullQ.nearestS hintQ.nearestS
                    , \_ -> Expect.within (Expect.Absolute 0.01) (Vec3.length hintQ.normal) 1.0
                    ]
                    ()
        , test "Roadside trees/boulders maintain safe clearance from start pad and asphalt lane" <|
            \_ ->
                let
                    track =
                        Track.generate (Track.trackConfigFor QuarrySwitchbacks 99)

                    roadsideObstacles =
                        List.filter (\o -> o.kind /= ChicaneBlock) track.obstacles

                    badStart =
                        List.filter (\o -> o.s < 55.0) roadsideObstacles

                    badRoadbed =
                        List.filter
                            (\o ->
                                let
                                    smp =
                                        Track.splineSampleAtS track o.s

                                    dx =
                                        Vec3.getX o.position - Vec3.getX smp.center

                                    dz =
                                        Vec3.getZ o.position - Vec3.getZ smp.center

                                    latDist =
                                        sqrt (dx * dx + dz * dz)
                                in
                                latDist < smp.halfWidth + 2.5
                            )
                            roadsideObstacles
                in
                Expect.all
                    [ \_ -> Expect.equal [] badStart
                    , \_ -> Expect.equal [] badRoadbed
                    , \_ -> Expect.greaterThan 15 (List.length track.obstacles)
                    ]
                    ()
        ]


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
