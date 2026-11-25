module TrackTest exposing (suite)

{-| Unit tests for `Racer.Track` procedural spline generation, heightmap continuity,
deterministic seeding, course length/steepness, and roadside scenery clearance.

The courses are generated from maps (`Racer.Course`), so these tests check
invariants every generated course must keep rather than the values of one
hand-written layout.

-}

import Array
import Expect
import Game.MapParams as Map exposing (Season(..))
import Math.Vector3 as Vec3
import Racer.TerrainMesh as TerrainMesh
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
        , describe "Start grid boxes and starting line detail"
            [ test "segIdx -1 paints bright white starting line across road bands (bands 5, 6, 7)" <|
                \_ ->
                    let
                        c5 =
                            TerrainMesh.bandColor FarmLane Summer DirtRoad -1 5

                        c6 =
                            TerrainMesh.bandColor FarmLane Summer DirtRoad -1 6

                        c7 =
                            TerrainMesh.bandColor FarmLane Summer DirtRoad -1 7
                    in
                    Expect.all
                        [ \_ -> Expect.equal c5 c6
                        , \_ -> Expect.equal c6 c7
                        ]
                        ()
            , test "segIdx -2 staging paddock box paints alternating yellow and white grid boxes with dark road center" <|
                \_ ->
                    let
                        c5 =
                            TerrainMesh.bandColor FarmLane Summer DirtRoad -2 5

                        c6 =
                            TerrainMesh.bandColor FarmLane Summer DirtRoad -2 6

                        c7 =
                            TerrainMesh.bandColor FarmLane Summer DirtRoad -2 7

                        whiteStartLine =
                            TerrainMesh.bandColor FarmLane Summer DirtRoad -1 5
                    in
                    Expect.all
                        [ \_ -> Expect.equal c7 whiteStartLine
                        , \_ -> Expect.notEqual c5 whiteStartLine
                        , \_ -> Expect.notEqual c6 c5
                        , \_ -> Expect.notEqual c6 c7
                        ]
                        ()
            ]
        , test "The ground takes the season's colours: snow in winter, a new green in spring, tan in autumn" <|
            \_ ->
                let
                    field season =
                        TerrainMesh.bandColor ForestTrail season DirtRoad 5 1

                    snowy c =
                        Vec3.getX c > 0.85 && Vec3.getY c > 0.85 && Vec3.getZ c > 0.85
                in
                Expect.all
                    [ \_ -> Expect.equal ( True, False ) ( snowy (field Winter), snowy (field Summer) )
                    , \_ -> Expect.notEqual (field Spring) (field Summer)
                    , \_ -> Expect.notEqual (field Autumn) (field Summer)
                    , \_ -> Expect.notEqual (field Autumn) (field Spring)
                    ]
                    ()
        , test "Track ids round-trip and retired mountain ids map to countryside courses" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (List.map Just allTrackIds) (List.map (trackIdToString >> trackIdFromString) allTrackIds)
                    , \_ -> Expect.equal (Just FarmLane) (trackIdFromString "pine-ridge")
                    , \_ -> Expect.equal (Just ForestTrail) (trackIdFromString "quarry-switchbacks")
                    , \_ -> Expect.equal (Just MeadowPath) (trackIdFromString "alpine-glacier")
                    ]
                    ()
        , test "Each track id is its biome's summer, clear map with the same seed" <|
            \_ ->
                Expect.equal
                    [ ( Map.Farmland, Map.Summer, ( Map.Clear, 42 ) )
                    , ( Map.Boreal, Map.Summer, ( Map.Clear, 42 ) )
                    , ( Map.Fell, Map.Summer, ( Map.Clear, 42 ) )
                    ]
                    (List.map
                        (\id ->
                            let
                                m =
                                    (Track.trackConfigFor id 42).map
                            in
                            ( m.biome, m.season, ( m.weather, m.seed ) )
                        )
                        [ FarmLane, ForestTrail, MeadowPath ]
                    )
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
        , describe "Brick launch ramps stand on one lane and leave a free lane"
            (List.concatMap (\id -> List.map (verifyBrickRamps id) [ 1, 42, 99, 1234 ]) allTrackIds)
        , describe "Rounded side ramps stand on the road edge, leave the road free and are clear of the other features"
            (List.concatMap (\id -> List.map (verifySideRamps id) [ 1, 42, 99, 1234 ]) allTrackIds)
        , describe "Each course has one brick loop, placed clear of the gates, ramps and scenery"
            (List.concatMap (\id -> List.map (verifyLoopPlacement id) [ 1, 42, 99, 1234 ]) allTrackIds)
        , describe "The loop's lane is continuous, goes upside down and meets the road at both ends"
            (List.concatMap (\id -> List.map (verifyLoopGeometry id) [ 1, 42, 99, 1234 ]) allTrackIds)
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
    test (trackIdToString id ++ " is 0.7-1.3 km long with at least 25% average grade") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id 11)
            in
            Expect.all
                [ \_ -> Expect.atLeast 700.0 track.totalLengthM
                , \_ -> Expect.atMost 1300.0 track.totalLengthM
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


verifyBrickRamps : TrackId -> Int -> Test
verifyBrickRamps id seed =
    test (trackIdToString id ++ " seed " ++ String.fromInt seed ++ " has ramps on the road with a free lane, clear of the gates") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id seed)

                again =
                    Track.generate (Track.trackConfigFor id seed)

                badRamps =
                    List.filter
                        (\rp ->
                            let
                                smp =
                                    Track.splineSampleAtS track ((rp.startS + rp.endS) * 0.5)

                                hw =
                                    smp.halfWidth

                                -- Width of road left free on the wider side of the ramp.
                                freeLane =
                                    max (hw - (rp.centerOffsetM + rp.halfWidthM)) (rp.centerOffsetM - rp.halfWidthM + hw)

                                nearGate =
                                    List.any (\cp -> cp.s > rp.startS - 8.0 && cp.s < rp.endS + 26.0) track.checkpoints

                                -- A car must reach a loop on its wheels.
                                nearLoop =
                                    List.any (\lp -> rp.endS + 22.0 > lp.entryS - 30.0 && rp.startS < lp.exitS + 16.0) track.loops
                            in
                            abs rp.centerOffsetM + rp.halfWidthM > hw || freeLane < 2.5 || nearGate || nearLoop || rp.startS < 40.0
                        )
                        track.brickRamps

                starts =
                    List.map .startS track.brickRamps

                spacedApart =
                    List.map2 (\a b -> b - a >= 50.0) starts (List.drop 1 starts)
                        |> List.all identity

                lipLift =
                    List.map
                        (\rp -> (Track.brickRampLift track (rp.endS - 0.01) rp.centerOffsetM).lift)
                        track.brickRamps
            in
            Expect.all
                [ -- Validation asks for at least one; how many more fit depends on
                  -- how much of the generated course is straight and not too steep.
                  \_ -> Expect.atLeast 1 (List.length track.brickRamps)
                , \_ -> Expect.atMost 8 (List.length track.brickRamps)
                , \_ -> Expect.equal True spacedApart
                , \_ -> Expect.equal [] badRamps
                , \_ -> Expect.equal track.brickRamps again.brickRamps
                , \_ -> Expect.equal True (List.all (\h -> h > 0.6) lipLift)
                , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0.0 (Track.brickRampLift track 10.0 0.0).lift
                ]
                ()


verifyLoopPlacement : TrackId -> Int -> Test
verifyLoopPlacement id seed =
    test (trackIdToString id ++ " seed " ++ String.fromInt seed ++ " has a loop on straight road, away from the arches, ramps and scenery") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id seed)

                again =
                    Track.generate (Track.trackConfigFor id seed)

                badLoops =
                    List.filter
                        (\lp ->
                            let
                                zoneStart =
                                    lp.entryS - 30.0

                                zoneEnd =
                                    lp.exitS + 16.0

                                inZone sVal =
                                    sVal > zoneStart - 2.0 && sVal < zoneEnd + 2.0

                                bendy =
                                    List.range 0 (round ((zoneEnd - zoneStart) / 2.0))
                                        |> List.any (\k -> abs (Track.splineSampleAtS track (zoneStart + toFloat k * 2.0)).bankingRad >= 0.05)
                            in
                            List.any (\cp -> inZone cp.s || inZone (cp.s - 55.0)) track.checkpoints
                                || List.any (\rp -> rp.endS > zoneStart && rp.startS < zoneEnd) track.brickRamps
                                || List.any (\rp -> rp.endS > zoneStart && rp.startS < zoneEnd) track.ramps
                                || List.any (\w -> w.endS > zoneStart && w.startS < zoneEnd) track.washes
                                || bendy
                                || lp.entryS
                                < 160.0
                                || zoneEnd
                                > track.finishS
                                - 40.0
                        )
                        track.loops

                sceneryAtLoops =
                    List.filter
                        (\o -> List.any (\lp -> o.s > lp.entryS - 7.0 && o.s < lp.exitS + 7.0) track.loops)
                        track.obstacles
            in
            Expect.all
                [ \_ -> Expect.equal 1 (List.length track.loops)
                , \_ -> Expect.equal [] badLoops
                , \_ -> Expect.equal [] sceneryAtLoops
                , \_ -> Expect.equal track.loops again.loops
                ]
                ()


verifyLoopGeometry : TrackId -> Int -> Test
verifyLoopGeometry id seed =
    test (trackIdToString id ++ " seed " ++ String.fromInt seed ++ " loop lane is continuous, upside down at the top and lands on the road") <|
        \_ ->
            case List.head (Track.generate (Track.trackConfigFor id seed)).loops of
                Nothing ->
                    Expect.fail "no loop"

                Just lp ->
                    let
                        track =
                            Track.generate (Track.trackConfigFor id seed)

                        steps =
                            240

                        thetaAt k =
                            2.0 * pi * toFloat k / toFloat steps

                        frames =
                            List.map (\k -> Track.loopFrame track lp (thetaAt k) 0.0) (List.range 0 steps)

                        gaps =
                            List.map2 (\a b -> Vec3.distance a.position b.position) frames (List.drop 1 frames)

                        turns =
                            List.map2 (\a b -> Vec3.dot a.tangent b.tangent) frames (List.drop 1 frames)

                        orthonormal f =
                            abs (Vec3.length f.tangent - 1.0)
                                < 1.0e-6
                                && abs (Vec3.length f.up - 1.0)
                                < 1.0e-6
                                && abs (Vec3.length f.right - 1.0)
                                < 1.0e-6
                                && abs (Vec3.dot f.tangent f.up)
                                < 1.0e-6
                                && abs (Vec3.dot f.tangent f.right)
                                < 1.0e-6
                                && abs (Vec3.dot f.up f.right)
                                < 1.0e-6

                        -- The lane meets the road surface, along the road, at both ends.
                        meetsRoad sVal theta lat =
                            let
                                f =
                                    Track.loopFrame track lp theta lat

                                smp =
                                    Track.splineSampleAtS track sVal

                                ground =
                                    Track.groundHeightAt track sVal (Vec3.getX f.position) (Vec3.getZ f.position)

                                q =
                                    Track.sampleAtHinted sVal track (Vec3.getX f.position) (Vec3.getZ f.position)
                            in
                            abs (Vec3.getY f.position - ground)
                                < 0.08
                                && abs (q.nearestS - sVal)
                                < 0.3
                                && abs (q.roadOffsetM - lat)
                                < 0.1
                                && Vec3.dot f.tangent (Vec3.normalize smp.tangent)
                                > 0.99
                                && Vec3.getY f.up
                                > 0.9

                        ends =
                            List.concatMap (\lat -> [ meetsRoad lp.entryS 0.0 lat, meetsRoad lp.exitS (2.0 * pi) lat ]) [ -1.0, 0.0, 1.0 ]

                        entryY =
                            Vec3.getY (Track.splineSampleAtS track lp.entryS).center

                        topY =
                            List.map (.position >> Vec3.getY) frames |> List.maximum |> Maybe.withDefault entryY

                        -- Seen from the side the way up and the way down cross:
                        -- there they must be a full lane apart sideways.
                        upHalf =
                            List.filter (\k -> thetaAt k > 0.5 && thetaAt k < pi - 0.9) (List.range 0 steps)

                        downHalf =
                            List.filter (\k -> thetaAt k > pi + 0.9 && thetaAt k < 2.0 * pi - 0.5) (List.range 0 steps)

                        sideways =
                            normalizeFlat (Track.splineSampleAtS track lp.entryS).right

                        clashes =
                            List.concatMap
                                (\a ->
                                    List.filterMap
                                        (\b ->
                                            let
                                                pa =
                                                    (Track.loopFrame track lp (thetaAt a) 0.0).position

                                                pb =
                                                    (Track.loopFrame track lp (thetaAt b) 0.0).position

                                                d =
                                                    Vec3.sub pa pb

                                                lateral =
                                                    abs (Vec3.dot d sideways)

                                                inPlane =
                                                    Vec3.length (Vec3.sub d (Vec3.scale (Vec3.dot d sideways) sideways))
                                            in
                                            if lateral < 2.0 * (lp.laneHalfWidthM + 0.25) && inPlane < 1.5 then
                                                Just ( a, b )

                                            else
                                                Nothing
                                        )
                                        downHalf
                                )
                                upHalf
                    in
                    Expect.all
                        [ \_ -> Expect.atMost 0.25 (List.maximum gaps |> Maybe.withDefault 1.0)
                        , \_ -> Expect.atLeast 0.95 (List.minimum turns |> Maybe.withDefault 0.0)
                        , \_ -> Expect.equal True (List.all orthonormal frames)
                        , \_ -> Expect.equal True (List.all identity ends)
                        , \_ -> Expect.lessThan -0.95 (List.map (.up >> Vec3.getY) frames |> List.minimum |> Maybe.withDefault 1.0)
                        , \_ -> Expect.atLeast (entryY + 2.0 * lp.radiusM - 2.5) topY
                        , \_ -> Expect.equal [] clashes
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) lp.laneHalfWidthM (Track.loopLaneHalfWidth lp pi)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) lp.apronHalfWidthM (Track.loopLaneHalfWidth lp 0.0)
                        , \_ -> Expect.greaterThan 0.0 (Track.loopFrame track lp pi 0.0).curvature
                        ]
                        ()


normalizeFlat : Vec3.Vec3 -> Vec3.Vec3
normalizeFlat v =
    Vec3.normalize (Vec3.vec3 (Vec3.getX v) 0 (Vec3.getZ v))


verifySideRamps : TrackId -> Int -> Test
verifySideRamps id seed =
    test (trackIdToString id ++ " seed " ++ String.fromInt seed ++ " has rounded side ramps on the road edge, clear of the gates, ramps, loop and scenery") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id seed)

                again =
                    Track.generate (Track.trackConfigFor id seed)

                overlaps a0 a1 b0 b1 =
                    a1 > b0 && a0 < b1

                badPlacement =
                    List.filter
                        (\sr ->
                            let
                                hw =
                                    (Track.splineSampleAtS track ((sr.startS + sr.endS) * 0.5)).halfWidth
                            in
                            (sr.innerOffsetM < hw - 1.05)
                                || List.any (\cp -> overlaps sr.startS sr.endS (cp.s - 10.0) (cp.s + 10.0) || overlaps sr.startS sr.endS (cp.s - 63.0) (cp.s - 47.0)) track.checkpoints
                                || List.any (\rp -> overlaps sr.startS sr.endS rp.startS (rp.endS + 22.0)) track.brickRamps
                                || List.any (\lp -> overlaps sr.startS sr.endS (lp.entryS - 80.0) (lp.exitS + 16.0)) track.loops
                                || sr.startS
                                < 50.0
                                || sr.endS
                                > track.finishS
                                - 20.0
                        )
                        track.sideRamps

                -- Its curve: flat where it leaves the road, steep at the lip, and
                -- only on its own side of the road.
                badShape =
                    List.filter
                        (\sr ->
                            let
                                mid =
                                    (sr.startS + sr.endS) * 0.5

                                lifts =
                                    List.map (\k -> (Track.sideRampLift track mid (sr.side * (sr.innerOffsetM + toFloat k * 0.1))).lift) (List.range 0 24)

                                rising =
                                    List.map2 (<) lifts (List.drop 1 lifts) |> List.all identity

                                nearInner =
                                    abs (Track.sideRampLift track mid (sr.side * (sr.innerOffsetM + 0.1))).sideGrade

                                atLip =
                                    abs (Track.sideRampLift track mid (sr.side * (sr.outerOffsetM - 0.01))).sideGrade

                                top =
                                    (Track.sideRampLift track mid (sr.side * (sr.outerOffsetM - 0.001))).lift

                                -- Behind the lip the ground keeps rising away from
                                -- the road: no hollow to get stuck in.
                                behind =
                                    List.map
                                        (\k ->
                                            let
                                                d =
                                                    sr.side * (sr.outerOffsetM + toFloat k * 0.5)

                                                smp =
                                                    Track.splineSampleAtS track mid

                                                p =
                                                    Vec3.add smp.center (Vec3.scale d (normalizeFlat smp.right))
                                            in
                                            Track.groundHeightAt track mid (Vec3.getX p) (Vec3.getZ p) + (Track.sideRampLift track mid d).lift
                                        )
                                        (List.range 0 36)

                                noHollow =
                                    List.map2 (\a b -> b >= a - 0.03) behind (List.drop 1 behind) |> List.all identity
                            in
                            not rising
                                || not noHollow
                                || nearInner
                                > 0.2
                                || atLip
                                < 1.2
                                || abs (top - sr.heightM)
                                > 0.01
                                || (Track.sideRampLift track mid 0.0).lift
                                /= 0.0
                                || (Track.sideRampLift track mid (-sr.side * (sr.outerOffsetM - 0.1))).lift
                                /= 0.0
                                || (Track.sideRampLift track (sr.startS - 0.5) (sr.side * sr.outerOffsetM)).lift
                                /= 0.0
                                || (Track.sideRampLift track (sr.endS + 0.5) (sr.side * sr.outerOffsetM)).lift
                                /= 0.0
                        )
                        track.sideRamps

                sceneryBehind =
                    List.filter
                        (\o ->
                            List.any
                                (\sr ->
                                    let
                                        q =
                                            Track.sampleAt track (Vec3.getX o.position) (Vec3.getZ o.position)
                                    in
                                    o.s > sr.startS - 10.0 && o.s < sr.endS + 33.0 && q.roadOffsetM * sr.side > 0.0
                                )
                                track.sideRamps
                        )
                        track.obstacles
            in
            Expect.all
                [ \_ -> Expect.atLeast 1 (List.length track.sideRamps)
                , \_ -> Expect.atMost 5 (List.length track.sideRamps)
                , \_ -> Expect.equal [] badPlacement
                , \_ -> Expect.equal [] badShape
                , \_ -> Expect.equal [] (List.map .id sceneryBehind)
                , \_ -> Expect.equal track.sideRamps again.sideRamps
                ]
                ()
