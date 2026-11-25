module Racer.Track exposing
    ( LoopFrame
    , RampLift
    , SideLift
    , brickRampLift
    , fieldUndulation
    , generate
    , groundHeightAt
    , loopFrame
    , loopLaneHalfWidth
    , mapForTrack
    , obstaclesNear
    , recoveryBankGrade
    , recoveryCorridorM
    , removeObstacles
    , sampleAt
    , sampleAtHinted
    , sideRampLift
    , splineSampleAtS
    , trackConfigFor
    , validationIssues
    , vergeWidthM
    )

{-| Procedural downhill countryside course generator and spatial terrain sampler.

Generates short, steep, deterministic Catmull-Rom tracks on unpaved roads:

  - `FarmLane`: dirt and gravel farm road with two clear bends and a steep drop
    into the farmyard,
  - `ForestTrail`: packed-earth forest track with a root-rippled section, a log
    hump and a fast gravel chute through a clearing,
  - `MeadowPath`: narrow, steepest grass footpath with a field hump and tussocks.

Each course is lined with brick-built scenery (trees, bushes, fences, rocks,
signposts, hay bales, cottages, barns, crates, barrels, garden gnomes, brick
wall sections, brick pallets, tile stacks, brick gates and slope blocks) that
the racer can smash apart, has a few brick-built launch ramps on one lane of
the road (`brickRamps`, see `brickRampLift`), a few rounded side ramps of
curved slope bricks along the road edges (`sideRamps`, see `sideRampLift`) and
one brick loop-the-loop (`loops`, see `loopFrame`).

Exposes continuous `sampleAt` / `sampleAtHinted` heightmap + finite-difference surface
normal queries and O(1) spatial-bucketed `obstaclesNear` collision lookups.

-}

import Array exposing (Array)
import Game.MapParams as Map
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Course as Course
import Racer.Types
    exposing
        ( BlueprintKnot
        , BrickRamp
        , Checkpoint
        , ControlPoint
        , GeneratedTrack
        , LoopSection
        , ObstacleKind(..)
        , RampZone
        , SegmentFeature(..)
        , SideRamp
        , SurfaceType(..)
        , TerrainSample
        , TrackConfig
        , TrackId(..)
        , TrackObstacle
        , TrackSample
        , WashZone
        )
import Random


{-| The course for a track id and seed. The id picks a map (its biome, with
summer and clear weather), and the map's seeded course shape gives the length,
drop and width. The first of eight seeded attempts that passes
`validationIssues` is used; should none pass, the first attempt is kept.
-}
trackConfigFor : TrackId -> Int -> TrackConfig
trackConfigFor id seed =
    let
        map =
            mapForTrack id seed

        cp =
            Course.courseParams map

        configAt attempt =
            { id = id
            , seed = seed
            , lengthM = cp.lengthM
            , dropM = cp.dropM
            , defaultHalfWidthM = cp.halfWidthM
            , knots = Course.generateKnots attempt map cp
            , map = map
            }

        firstValid attempt =
            let
                config =
                    configAt attempt
            in
            if attempt >= maxAttempts - 1 || List.isEmpty (validationIssues (generate config)) then
                config

            else
                firstValid (attempt + 1)
    in
    firstValid 0


{-| Attempts tried for each course before the first one is kept anyway.
-}
maxAttempts : Int
maxAttempts =
    8


{-| The map behind a track id: the three countryside courses are the
farmland, boreal and fell maps.
-}
mapForTrack : TrackId -> Int -> Map.MapParams
mapForTrack id seed =
    { biome =
        case id of
            FarmLane ->
                Map.Farmland

            ForestTrail ->
                Map.Boreal

            MeadowPath ->
                Map.Fell
    , season = Map.Summer
    , weather = Map.Clear
    , seed = clamp 1 999999 seed
    }


{-| Generate a complete deterministic `GeneratedTrack` from a `TrackConfig`.
-}
generate : TrackConfig -> GeneratedTrack
generate config =
    let
        sampleSpacingM =
            2.0

        bucketSizeM =
            25.0

        controlPoints =
            buildControlPoints config

        denseRaw =
            evaluateCatmullRomSeries 14 controlPoints

        samples =
            resampleUniform sampleSpacingM denseRaw
                |> computeCurvatureAndBanking
                |> Array.fromList

        sampleCount =
            Array.length samples

        totalLengthM =
            Array.get (sampleCount - 1) samples
                |> Maybe.map .s
                |> Maybe.withDefault config.lengthM

        startSample =
            Array.get 0 samples
                |> Maybe.withDefault
                    { s = 0.0
                    , center = vec3 0 0 0
                    , tangent = vec3 0 0 1
                    , right = vec3 -1 0 0
                    , normal = vec3 0 1 0
                    , halfWidth = config.defaultHalfWidthM
                    , bankingRad = 0.0
                    , surface = DirtRoad
                    }

        endSample =
            Array.get (sampleCount - 1) samples
                |> Maybe.withDefault startSample

        totalDropM =
            max 1.0 (Vec3.getY startSample.center - Vec3.getY endSample.center)

        startYaw =
            atan2 (Vec3.getX startSample.tangent) (Vec3.getZ startSample.tangent)

        finishS =
            max 120.0 (totalLengthM - 36.0)

        checkpoints =
            buildCheckpoints samples finishS

        ( ramps, washes ) =
            extractFeatureZones totalLengthM controlPoints

        bucketCount =
            max 1 (ceiling (totalLengthM / bucketSizeM) + 1)

        bareTrack =
            { config = config
            , samples = samples
            , sampleSpacingM = sampleSpacingM
            , totalLengthM = totalLengthM
            , totalDropM = totalDropM
            , startPosition = Vec3.add startSample.center (vec3 0 0.08 0)
            , startYaw = startYaw
            , finishS = finishS
            , checkpoints = checkpoints
            , obstacles = []
            , obstacleBuckets = Array.repeat bucketCount []
            , bucketSizeM = bucketSizeM
            , ramps = ramps
            , brickRamps = []
            , loops = []
            , sideRamps = []
            , washes = washes
            }

        loopedTrack =
            { bareTrack | loops = placeLoops bareTrack }

        bareRampedTrack =
            { loopedTrack | brickRamps = placeBrickRamps loopedTrack }

        rampedTrack =
            { bareRampedTrack | sideRamps = placeSideRamps bareRampedTrack }

        obstacles =
            generateScenery rampedTrack
    in
    { rampedTrack
        | obstacles = obstacles
        , obstacleBuckets = bucketObstacles bucketSizeM bucketCount obstacles
    }


{-| Checks a generated course must pass before it is used: its length and
grade, no steep 20 m stretch, no bend tighter than 18 m, legs far enough
apart not to touch, one loop, at least one launch ramp, and at least four
checkpoints in order. An empty list means the course passes.
-}
validationIssues : GeneratedTrack -> List String
validationIssues track =
    let
        config =
            track.config

        samples =
            track.samples

        n =
            Array.length samples

        sampleAt i =
            Array.get i samples

        lengthIssue =
            if abs (track.totalLengthM - config.lengthM) > 0.08 * config.lengthM then
                Just "length is off the course's target"

            else
                Nothing

        grade =
            track.totalDropM / max 1.0 track.totalLengthM

        gradeIssue =
            if grade < 0.25 || grade > 0.48 then
                Just "average grade is out of range"

            else
                Nothing

        steep i =
            case ( sampleAt i, sampleAt (i + 10) ) of
                ( Just a, Just b ) ->
                    let
                        ds =
                            b.s - a.s

                        dy =
                            Vec3.getY a.center - Vec3.getY b.center
                    in
                    ds > 0 && dy / ds > 0.65

                _ ->
                    False

        steepIssue =
            if List.any steep (List.range 0 (n - 11)) then
                Just "a stretch is steeper than 65 %"

            else
                Nothing

        -- Turn of the course over 4 m (two samples either side): the arc
        -- length divided by the turn angle is the bend radius, which must be 18 m or more.
        tight i =
            case ( sampleAt (i - 1), sampleAt i, sampleAt (i + 1) ) of
                ( Just a, _, Just b ) ->
                    let
                        cross =
                            Vec3.getX a.tangent * Vec3.getZ b.tangent - Vec3.getZ a.tangent * Vec3.getX b.tangent

                        dot =
                            Vec3.getX a.tangent * Vec3.getX b.tangent + Vec3.getZ a.tangent * Vec3.getZ b.tangent

                        turn =
                            abs (atan2 cross dot)

                        arc =
                            b.s - a.s
                    in
                    turn * 18.0 > arc

                _ ->
                    False

        curveIssue =
            if List.any tight (List.range 1 (n - 2)) then
                Just "a bend is tighter than 18 m"

            else
                Nothing

        -- Legs more than 80 m apart along the course must keep clear of each other.
        spaced =
            List.range 0 (n - 1)
                |> List.filter (\i -> modBy 4 i == 0)
                |> List.filterMap (\i -> Maybe.map (\sample -> ( i, sample )) (sampleAt i))

        touching ( i, a ) ( j, b ) =
            i
                < j
                && b.s
                - a.s
                > 80.0
                && sqrt ((Vec3.getX a.center - Vec3.getX b.center) ^ 2 + (Vec3.getZ a.center - Vec3.getZ b.center) ^ 2)
                < 2.0
                * (a.halfWidth + recoveryCorridorM)

        clearanceIssue =
            if List.any (\p -> List.any (\q -> touching p q) spaced) spaced then
                Just "two legs of the course touch"

            else
                Nothing

        loopIssue =
            if List.length track.loops == 1 then
                Nothing

            else
                Just "the course does not have exactly one loop"

        rampIssue =
            if List.isEmpty track.brickRamps then
                Just "the course has no launch ramp"

            else
                Nothing

        checkpointsIssue =
            let
                ss =
                    List.map .s track.checkpoints

                increasing =
                    List.map2 Tuple.pair ss (List.drop 1 ss)
                        |> List.all (\( a, b ) -> b > a)
            in
            if List.length track.checkpoints >= 4 && increasing then
                Nothing

            else
                Just "checkpoints are missing or out of order"
    in
    List.filterMap identity
        [ lengthIssue
        , gradeIssue
        , steepIssue
        , curveIssue
        , clearanceIssue
        , loopIssue
        , rampIssue
        , checkpointsIssue
        ]


{-| Remove smashed scenery pieces (by obstacle id) from collision lists and buckets.
-}
removeObstacles : List Int -> GeneratedTrack -> GeneratedTrack
removeObstacles ids track =
    if List.isEmpty ids then
        track

    else
        let
            keep o =
                not (List.member o.id ids)
        in
        { track
            | obstacles = List.filter keep track.obstacles
            , obstacleBuckets = Array.map (List.filter keep) track.obstacleBuckets
        }


{-| Query continuous terrain height, unit surface normal, surface material, and road offset
without a spline hint (full-track scan, suitable for respawns and tests).
-}
sampleAt : GeneratedTrack -> Float -> Float -> TerrainSample
sampleAt track worldX worldZ =
    sampleAtHinted -1.0 track worldX worldZ


{-| Query continuous terrain height, unit surface normal, surface material, and road offset
using the car's previous spline parameter `hintS` for O(1) local window search.
-}
sampleAtHinted : Float -> GeneratedTrack -> Float -> Float -> TerrainSample
sampleAtHinted hintS track worldX worldZ =
    let
        proj =
            findClosestSplineProjection hintS track worldX worldZ

        eps =
            0.35

        hCenter =
            heightWithProjection track proj worldX worldZ

        hRight =
            heightNearProjection track proj (worldX + eps) worldZ

        hLeft =
            heightNearProjection track proj (worldX - eps) worldZ

        hFwd =
            heightNearProjection track proj worldX (worldZ + eps)

        hBack =
            heightNearProjection track proj worldX (worldZ - eps)

        dhDx =
            (hRight - hLeft) / (2.0 * eps)

        dhDz =
            (hFwd - hBack) / (2.0 * eps)

        rawNormal =
            vec3 -dhDx 1.0 -dhDz

        unitNormal =
            normalizeSafe rawNormal (vec3 0 1 0)

        absD =
            abs proj.signedD

        effectiveSurface =
            if absD <= proj.halfWidth then
                proj.surface

            else if absD <= proj.halfWidth + vergeWidthM then
                RoughVerge

            else
                MeadowGrass
    in
    { height = hCenter
    , normal = unitNormal
    , surface = effectiveSurface
    , roadOffsetM = proj.signedD
    , nearestS = proj.s
    , nearestCenter = proj.center
    , nearestTangent = proj.tangent
    , halfWidthM = proj.halfWidth
    }


{-| Cheap ground height at world (x, z) near arc length `hintS` (no normal or
surface), used to land tumbling debris on the terrain.
-}
groundHeightAt : GeneratedTrack -> Float -> Float -> Float -> Float
groundHeightAt track hintS worldX worldZ =
    heightWithProjection track (findClosestSplineProjection hintS track worldX worldZ) worldX worldZ


{-| Fetch obstacles inside spatial buckets overlapping `[sCenter - windowM, sCenter + windowM]`.
-}
obstaclesNear : GeneratedTrack -> Float -> Float -> List TrackObstacle
obstaclesNear track sCenter windowM =
    let
        minBucket =
            max 0 (floor ((sCenter - windowM) / track.bucketSizeM))

        maxBucket =
            min (Array.length track.obstacleBuckets - 1) (ceiling ((sCenter + windowM) / track.bucketSizeM))
    in
    if maxBucket < minBucket then
        []

    else
        List.range minBucket maxBucket
            |> List.concatMap (\idx -> Array.get idx track.obstacleBuckets |> Maybe.withDefault [])


{-| Retrieve the interpolated `TrackSample` along the course centerline at arc-length `s`.
-}
splineSampleAtS : GeneratedTrack -> Float -> TrackSample
splineSampleAtS track targetS =
    let
        len =
            Array.length track.samples
    in
    if len == 0 then
        { s = 0.0
        , center = vec3 0 0 0
        , tangent = vec3 0 0 1
        , right = vec3 -1 0 0
        , normal = vec3 0 1 0
        , halfWidth = track.config.defaultHalfWidthM
        , bankingRad = 0.0
        , surface = DirtRoad
        }

    else
        let
            clampedS =
                clamp 0.0 track.totalLengthM targetS

            idxF =
                clampedS / track.sampleSpacingM

            i0 =
                clamp 0 (len - 2) (floor idxF)

            i1 =
                min (len - 1) (i0 + 1)
        in
        case ( Array.get i0 track.samples, Array.get i1 track.samples ) of
            ( Just s0, Just s1 ) ->
                let
                    span =
                        max 1.0e-5 (s1.s - s0.s)

                    t =
                        clamp 0.0 1.0 ((clampedS - s0.s) / span)
                in
                interpolateTrackSample s0 s1 t

            ( Just s0, Nothing ) ->
                s0

            _ ->
                { s = 0.0
                , center = vec3 0 0 0
                , tangent = vec3 0 0 1
                , right = vec3 -1 0 0
                , normal = vec3 0 1 0
                , halfWidth = track.config.defaultHalfWidthM
                , bankingRad = 0.0
                , surface = DirtRoad
                }


{-| Smooth deterministic rolling-field undulation for the open countryside beside the road.
-}
fieldUndulation : Int -> Float -> Float -> Float
fieldUndulation seed x z =
    let
        phaseA =
            toFloat (modBy 997 (abs seed + 17)) * 0.013

        phaseB =
            toFloat (modBy 613 (abs seed + 41)) * 0.019

        low =
            sin (x * 0.03 + phaseA) * cos (z * 0.026 - phaseB) * 3.2

        mid =
            sin ((x + z) * 0.07 - phaseB) * 1.1

        detail =
            cos (x * 0.17 - z * 0.13 + phaseA) * 0.25
    in
    low + mid + detail


{-| Width of the rough grass verge between the roadbed and the open field.
-}
vergeWidthM : Float
vergeWidthM =
    2.5


{-| Off-road band (m beyond the road edge) the car can reach: the valley
containment wall stands at its outer edge. Inside it the ground is a clean,
gentle bank that always slopes back down toward the road.
-}
recoveryCorridorM : Float
recoveryCorridorM =
    22.0


{-| Rise of the off-road bank per metre away from the road edge, at the edge.
Gentle enough to drive on, steep enough that a car left in the field rolls back
to the road.
-}
recoveryBankGrade : Float
recoveryBankGrade =
    0.08


{-| How quickly the bank steepens further out (rise = grade u + curve u^2), so
it still leads back to the road on the inside of bends, where the road's own
downhill grade pulls along it. About 25 % at the corridor wall.
-}
recoveryBankCurve : Float
recoveryBankCurve =
    0.004



-- ── Spline & Control Point Construction ───────────────────────────────────────


buildControlPoints : TrackConfig -> List ControlPoint
buildControlPoints config =
    let
        rawKnots =
            config.knots

        lenScale =
            config.lengthM

        startAltitude =
            config.dropM + 20.0

        ( finalKnots, _ ) =
            List.foldl
                (\knot ( acc, rng ) ->
                    let
                        ( jitterX, rng1 ) =
                            Random.step (Random.float -5.0 5.0) rng

                        ( jitterY, rng2 ) =
                            Random.step (Random.float -0.8 0.8) rng1

                        envelope =
                            if knot.sFrac <= 0.06 || knot.sFrac >= 0.92 then
                                0.0

                            else
                                sin (pi * clamp 0.0 1.0 ((knot.sFrac - 0.06) / 0.86))

                        x =
                            knot.xOffset + jitterX * envelope

                        z =
                            knot.zProgress * lenScale

                        y =
                            startAltitude - knot.yDropFrac * config.dropM + jitterY * envelope

                        cp : ControlPoint
                        cp =
                            { position = vec3 x y z
                            , roadHalfWidth = knot.halfWidth
                            , bankingRad = 0.0
                            , surface = knot.surface
                            , feature = knot.feature
                            }
                    in
                    ( cp :: acc, rng2 )
                )
                ( [], Random.initialSeed (config.seed * 7919 + 13) )
                rawKnots
    in
    List.reverse finalKnots


evaluateCatmullRomSeries : Int -> List ControlPoint -> List RawSplinePt
evaluateCatmullRomSeries subSteps controls =
    let
        arr =
            Array.fromList controls

        n =
            Array.length arr
    in
    if n < 2 then
        []

    else
        let
            getClamped idx =
                Array.get (clamp 0 (n - 1) idx) arr

            segmentPoints segIdx =
                case ( ( getClamped (segIdx - 1), getClamped segIdx ), ( getClamped (segIdx + 1), getClamped (segIdx + 2) ) ) of
                    ( ( Just p0, Just p1 ), ( Just p2, Just p3 ) ) ->
                        let
                            stepEnd =
                                if segIdx == n - 2 then
                                    subSteps

                                else
                                    subSteps - 1
                        in
                        List.map
                            (\k ->
                                let
                                    t =
                                        toFloat k / toFloat subSteps

                                    pos =
                                        catmullRomVec3 t p0.position p1.position p2.position p3.position

                                    hw =
                                        lerp t p1.roadHalfWidth p2.roadHalfWidth

                                    surf =
                                        if t < 0.5 then
                                            p1.surface

                                        else
                                            p2.surface
                                in
                                { center = pos
                                , halfWidth = hw
                                , surface = surf
                                }
                            )
                            (List.range 0 stepEnd)

                    _ ->
                        []
        in
        List.range 0 (n - 2)
            |> List.concatMap segmentPoints


type alias RawSplinePt =
    { center : Vec3
    , halfWidth : Float
    , surface : SurfaceType
    }


catmullRomVec3 : Float -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3
catmullRomVec3 t p0 p1 p2 p3 =
    let
        t2 =
            t * t

        t3 =
            t2 * t

        scalar k0 k1 k2 k3 =
            0.5
                * ((2.0 * k1)
                    + (-k0 + k2)
                    * t
                    + (2.0 * k0 - 5.0 * k1 + 4.0 * k2 - k3)
                    * t2
                    + (-k0 + 3.0 * k1 - 3.0 * k2 + k3)
                    * t3
                  )
    in
    vec3
        (scalar (Vec3.getX p0) (Vec3.getX p1) (Vec3.getX p2) (Vec3.getX p3))
        (scalar (Vec3.getY p0) (Vec3.getY p1) (Vec3.getY p2) (Vec3.getY p3))
        (scalar (Vec3.getZ p0) (Vec3.getZ p1) (Vec3.getZ p2) (Vec3.getZ p3))


resampleUniform : Float -> List RawSplinePt -> List RawTrackPt
resampleUniform spacing rawPts =
    case rawPts of
        [] ->
            []

        first :: _ ->
            let
                withArc =
                    accumulateArcLength 0.0 rawPts []

                totalS =
                    List.reverse withArc
                        |> List.head
                        |> Maybe.map .arcS
                        |> Maybe.withDefault 0.0

                count =
                    max 2 (floor (totalS / spacing) + 1)

                arr =
                    Array.fromList withArc

                sampleAtArc targetS =
                    findBracketSample 0 (Array.length arr - 1) targetS arr first
            in
            List.range 0 count
                |> List.map (\i -> sampleAtArc (min totalS (toFloat i * spacing)))


type alias RawTrackPt =
    { arcS : Float
    , center : Vec3
    , halfWidth : Float
    , surface : SurfaceType
    }


accumulateArcLength : Float -> List RawSplinePt -> List RawTrackPt -> List RawTrackPt
accumulateArcLength currS pts acc =
    case pts of
        [] ->
            List.reverse acc

        p0 :: [] ->
            List.reverse ({ arcS = currS, center = p0.center, halfWidth = p0.halfWidth, surface = p0.surface } :: acc)

        p0 :: p1 :: rest ->
            let
                entry =
                    { arcS = currS
                    , center = p0.center
                    , halfWidth = p0.halfWidth
                    , surface = p0.surface
                    }

                segLen =
                    Vec3.distance p0.center p1.center
            in
            accumulateArcLength (currS + segLen) (p1 :: rest) (entry :: acc)


findBracketSample : Int -> Int -> Float -> Array RawTrackPt -> RawSplinePt -> RawTrackPt
findBracketSample lo hi targetS arr fallback =
    if Array.length arr == 0 then
        { arcS = targetS, center = fallback.center, halfWidth = fallback.halfWidth, surface = fallback.surface }

    else if hi - lo <= 1 then
        case ( Array.get lo arr, Array.get hi arr ) of
            ( Just a, Just b ) ->
                let
                    span =
                        max 1.0e-5 (b.arcS - a.arcS)

                    t =
                        clamp 0.0 1.0 ((targetS - a.arcS) / span)
                in
                { arcS = targetS
                , center = vec3Lerp t a.center b.center
                , halfWidth = lerp t a.halfWidth b.halfWidth
                , surface =
                    if t < 0.5 then
                        a.surface

                    else
                        b.surface
                }

            ( Just a, Nothing ) ->
                { a | arcS = targetS }

            _ ->
                { arcS = targetS, center = fallback.center, halfWidth = fallback.halfWidth, surface = fallback.surface }

    else
        let
            mid =
                (lo + hi) // 2
        in
        case Array.get mid arr of
            Just m ->
                if m.arcS <= targetS then
                    findBracketSample mid hi targetS arr fallback

                else
                    findBracketSample lo mid targetS arr fallback

            Nothing ->
                { arcS = targetS, center = fallback.center, halfWidth = fallback.halfWidth, surface = fallback.surface }


computeCurvatureAndBanking : List RawTrackPt -> List TrackSample
computeCurvatureAndBanking pts =
    let
        arr =
            Array.fromList pts

        n =
            Array.length arr

        build i curr =
            let
                prevCenter =
                    Array.get (max 0 (i - 2)) arr
                        |> Maybe.map .center
                        |> Maybe.withDefault curr.center

                nextCenter =
                    Array.get (min (n - 1) (i + 2)) arr
                        |> Maybe.map .center
                        |> Maybe.withDefault curr.center

                prev1 =
                    Array.get (max 0 (i - 1)) arr
                        |> Maybe.map .center
                        |> Maybe.withDefault curr.center

                next1 =
                    Array.get (min (n - 1) (i + 1)) arr
                        |> Maybe.map .center
                        |> Maybe.withDefault curr.center

                rawTan =
                    Vec3.sub next1 prev1

                tangent =
                    normalizeSafe rawTan (vec3 0 0 1)

                tanIn =
                    normalizeXZ (Vec3.sub curr.center prevCenter)

                tanOut =
                    normalizeXZ (Vec3.sub nextCenter curr.center)

                -- Signed turning rate around +Y (positive = curve bends toward driver's left/+X)
                signedCurve =
                    Vec3.getZ tanIn * Vec3.getX tanOut - Vec3.getX tanIn * Vec3.getZ tanOut

                bankingEnvelope =
                    smoothstep 18.0 42.0 curr.arcS

                bankingRad =
                    clamp -0.25 0.25 (signedCurve * 2.2) * bankingEnvelope

                rightXZ =
                    normalizeSafe (vec3 -(Vec3.getZ tangent) 0 (Vec3.getX tangent)) (vec3 -1 0 0)

                normal0 =
                    normalizeSafe (Vec3.cross rightXZ tangent) (vec3 0 1 0)

                -- Rotate road normal around tangent by bankingRad
                bankedNormal =
                    normalizeSafe
                        (Vec3.add
                            (Vec3.scale (cos bankingRad) normal0)
                            (Vec3.scale (sin bankingRad) rightXZ)
                        )
                        (vec3 0 1 0)

                bankedRight =
                    normalizeSafe
                        (Vec3.sub
                            (Vec3.scale (cos bankingRad) rightXZ)
                            (Vec3.scale (sin bankingRad) normal0)
                        )
                        rightXZ
            in
            { s = curr.arcS
            , center = curr.center
            , tangent = tangent
            , right = bankedRight
            , normal = bankedNormal
            , halfWidth = curr.halfWidth
            , bankingRad = bankingRad
            , surface = curr.surface
            }
    in
    List.indexedMap build pts


normalizeXZ : Vec3 -> Vec3
normalizeXZ v =
    normalizeSafe (vec3 (Vec3.getX v) 0 (Vec3.getZ v)) (vec3 0 0 1)



-- ── Feature Zones, Checkpoints & Obstacles ────────────────────────────────────


extractFeatureZones : Float -> List ControlPoint -> ( List RampZone, List WashZone )
extractFeatureZones totalLen cps =
    let
        count =
            max 1 (List.length cps - 1)

        process i cp ( rampsAcc, washesAcc ) =
            let
                approxS =
                    totalLen * (toFloat i / toFloat count)
            in
            case cp.feature of
                Just (LaunchRamp r) ->
                    ( { startS = approxS - r.lengthM * 0.5
                      , endS = approxS + r.lengthM * 0.5
                      , lipHeightM = r.lipHeightM
                      , halfWidthM = cp.roadHalfWidth * 0.72
                      }
                        :: rampsAcc
                    , washesAcc
                    )

                Just (RoughWash w) ->
                    ( rampsAcc
                    , { startS = approxS - 16.0
                      , endS = approxS + 16.0
                      , amplitudeM = w.amplitudeM
                      , wavelengthM = w.wavelengthM
                      }
                        :: washesAcc
                    )

                Nothing ->
                    ( rampsAcc, washesAcc )

        ( rList, wList ) =
            List.foldl (\( idx, cp ) acc -> process idx cp acc) ( [], [] ) (List.indexedMap Tuple.pair cps)
    in
    ( List.reverse rList, List.reverse wList )


buildCheckpoints : Array TrackSample -> Float -> List Checkpoint
buildCheckpoints samples finishS =
    let
        targets =
            [ ( 0, finishS * 0.25, False )
            , ( 1, finishS * 0.5, False )
            , ( 2, finishS * 0.75, False )
            , ( 3, finishS, True )
            ]

        sampleAtIndex sVal =
            let
                idx =
                    clamp 0 (Array.length samples - 1) (round (sVal / 2.0))
            in
            Array.get idx samples
    in
    List.filterMap
        (\( idx, targetS, isFin ) ->
            sampleAtIndex targetS
                |> Maybe.map
                    (\smp ->
                        { index = idx
                        , s = smp.s
                        , center = smp.center
                        , tangent = smp.tangent
                        , halfWidth = smp.halfWidth
                        , isFinish = isFin
                        }
                    )
        )
        targets


{-| How densely each course is dressed and which scenery kinds it favours.
-}
type alias SceneryTheme =
    { fenceChance : Float
    , density : Float
    , weights : List ( ObstacleKind, Float )
    }


sceneryTheme : Map.Biome -> SceneryTheme
sceneryTheme biome =
    case biome of
        Map.Farmland ->
            { fenceChance = 0.06
            , density = 0.2
            , weights =
                [ ( LeafyTree, 20 ), ( Bush, 15 ), ( HayBale, 15 ), ( Rock, 6 ), ( Signpost, 6 ), ( PineTree, 4 ), ( Cottage, 4 ), ( Barn, 5 ), ( Crate, 7 ), ( Barrel, 6 ), ( GardenGnome, 3 ), ( BrickWall, 4 ), ( BrickPallet, 4 ), ( TileStack, 3 ), ( BrickGate, 3 ), ( SlopeBlock, 3 ), ( CountdownBoard, 3 ), ( Chalet, 2 ), ( PitGarage, 2 ) ]
            }

        Map.Boreal ->
            { fenceChance = 0.015
            , density = 0.34
            , weights =
                [ ( PineTree, 38 ), ( LeafyTree, 16 ), ( Bush, 12 ), ( Rock, 12 ), ( Signpost, 5 ), ( HayBale, 2 ), ( Cottage, 3 ), ( Crate, 4 ), ( Barrel, 3 ), ( GardenGnome, 4 ), ( BrickWall, 1 ), ( BrickPallet, 3 ), ( TileStack, 3 ), ( BrickGate, 2 ), ( SlopeBlock, 2 ), ( CountdownBoard, 2 ), ( Chalet, 3 ), ( PitGarage, 1 ) ]
            }

        Map.Fell ->
            { fenceChance = 0.04
            , density = 0.17
            , weights =
                [ ( Bush, 21 ), ( LeafyTree, 14 ), ( Rock, 13 ), ( HayBale, 12 ), ( Signpost, 6 ), ( PineTree, 3 ), ( Cottage, 4 ), ( Barn, 3 ), ( Crate, 3 ), ( Barrel, 2 ), ( GardenGnome, 6 ), ( BrickWall, 4 ), ( BrickPallet, 3 ), ( TileStack, 3 ), ( BrickGate, 3 ), ( SlopeBlock, 3 ), ( CountdownBoard, 3 ), ( Chalet, 2 ), ( PitGarage, 2 ) ]
            }

        Map.Desert ->
            { fenceChance = 0.02
            , density = 0.22
            , weights =
                [ ( PalmTree, 18 ), ( Rock, 12 ), ( Bush, 6 ), ( Crate, 10 ), ( Barrel, 10 ), ( Cottage, 4 ), ( Signpost, 4 ), ( CountdownBoard, 2 ), ( BrickWall, 2 ), ( GardenGnome, 2 ), ( BrickPallet, 2 ), ( TileStack, 2 ), ( BrickGate, 2 ), ( SlopeBlock, 2 ), ( Chalet, 2 ), ( PitGarage, 2 ) ]
            }


pickWeighted : List ( ObstacleKind, Float ) -> Float -> ObstacleKind
pickWeighted weights roll =
    let
        total =
            List.sum (List.map Tuple.second weights)

        target =
            roll * total

        go remaining acc =
            case remaining of
                [] ->
                    Bush

                [ ( k, _ ) ] ->
                    k

                ( k, w ) :: rest ->
                    if target < acc + w then
                        k

                    else
                        go rest (acc + w)
    in
    go weights 0.0


{-| Lateral distance beyond the road edge, collision radius and height for one scenery kind.
`offRoll` and `sizeRoll` are uniform in [0, 1].
-}
scenerySpec : ObstacleKind -> Float -> Float -> { offsetM : Float, radiusM : Float, heightM : Float }
scenerySpec kind offRoll sizeRoll =
    case kind of
        LeafyTree ->
            { offsetM = 2.6 + offRoll * 7.0, radiusM = 0.7, heightM = 4.0 + 1.6 * sizeRoll }

        PineTree ->
            { offsetM = 2.4 + offRoll * 8.0, radiusM = 0.65, heightM = 5.0 + 2.0 * sizeRoll }

        PalmTree ->
            { offsetM = 2.4 + offRoll * 8.0, radiusM = 0.65, heightM = 5.0 + 2.0 * sizeRoll }

        Bush ->
            { offsetM = 1.7 + offRoll * 2.2, radiusM = 0.8, heightM = 0.9 }

        Rock ->
            { offsetM = 1.9 + offRoll * 4.0, radiusM = 0.7 + 0.35 * sizeRoll, heightM = 0.9 + 0.4 * sizeRoll }

        Signpost ->
            { offsetM = 1.3 + offRoll * 0.6, radiusM = 0.35, heightM = 1.9 }

        HayBale ->
            { offsetM = 1.9 + offRoll * 3.0
            , radiusM = 0.8
            , heightM =
                if sizeRoll > 0.55 then
                    1.7

                else
                    0.85
            }

        FenceSection ->
            { offsetM = 1.6, radiusM = 1.2, heightM = 1.0 }

        Cottage ->
            { offsetM = 10.0 + offRoll * 6.0, radiusM = 2.8, heightM = 4.6 }

        Barn ->
            { offsetM = 11.0 + offRoll * 6.0, radiusM = 3.3, heightM = 5.6 }

        Crate ->
            { offsetM = 1.4 + offRoll * 1.6
            , radiusM = 0.75
            , heightM =
                if sizeRoll > 0.5 then
                    1.8

                else
                    0.9
            }

        Barrel ->
            { offsetM = 1.4 + offRoll * 1.8, radiusM = 0.7, heightM = 1.0 }

        GardenGnome ->
            { offsetM = 1.2 + offRoll * 1.4, radiusM = 0.35, heightM = 0.8 }

        BrickWall ->
            { offsetM = 1.7 + offRoll * 0.8, radiusM = 1.3, heightM = 1.1 }

        BrickPallet ->
            { offsetM = 1.5 + offRoll * 1.8, radiusM = 0.65, heightM = 1.2 }

        TileStack ->
            { offsetM = 1.4 + offRoll * 1.6, radiusM = 0.65, heightM = 0.8 }

        BrickGate ->
            { offsetM = 1.6 + offRoll * 1.0, radiusM = 0.9, heightM = 1.6 }

        SlopeBlock ->
            { offsetM = 1.5 + offRoll * 1.0, radiusM = 1.0, heightM = 0.8 }

        CountdownBoard ->
            { offsetM = 1.4 + offRoll * 0.4, radiusM = 0.4, heightM = 1.2 }

        Chalet ->
            { offsetM = 3.2 + offRoll * 1.5, radiusM = 1.6, heightM = 3.6 }

        PitGarage ->
            { offsetM = 2.4 + offRoll * 1.0, radiusM = 1.4, heightM = 2.4 }


{-| The brick-built crashables of `Racer.SceneryModels` made only of the car
editor's dock parts. They also stay 6 m clear of the launch ramps, so a car
coming off a ramp does not land in one.
-}
dockBuiltScenery : List ObstacleKind
dockBuiltScenery =
    [ BrickPallet, TileStack, BrickGate, SlopeBlock, CountdownBoard, Chalet, PitGarage ]


type alias SceneryCursor =
    { acc : List TrackObstacle
    , rng : Random.Seed
    , nextId : Int
    , fenceLeft : Int
    , fenceRight : Int
    }


{-| Dress both road edges with brick-built scenery every 3 m of arc length.

Fences come in short runs parallel to the road; every other piece is picked from
the course theme. Pieces never overlap each other or the roadbed (checked against
the real nearest-road projection so bends cannot pull the road into them), and
the start pad, the checkpoint arches and the loops stay clear, and nothing
stands behind a side ramp or where a car flying off it comes down.

-}
generateScenery : GeneratedTrack -> List TrackObstacle
generateScenery track =
    let
        theme =
            sceneryTheme track.config.map.biome

        stepM =
            3.0

        startS =
            24.0

        endS =
            track.finishS - 10.0

        positions =
            List.range 0 (max 0 (floor ((endS - startS) / stepM)))
                |> List.map (\k -> startS + toFloat k * stepM)

        nearGate sVal =
            List.any (\cp -> abs (cp.s - sVal) < 7.0) track.checkpoints
                || List.any (\lp -> sVal > lp.entryS - loopSceneryClearM && sVal < lp.exitS + loopSceneryClearM) track.loops

        nearRamp sVal =
            List.any (\rp -> sVal > rp.startS - 6.0 && sVal < rp.endS + 6.0) track.brickRamps
                || List.any (\rp -> sVal > rp.startS - 6.0 && sVal < rp.endS + 6.0) track.ramps

        nearSideRamp side sVal =
            -- Room behind a side ramp for a car that flies off it.
            List.any (\sr -> sr.side == side && sVal > sr.startS - 12.0 && sVal < sr.endS + 35.0) track.sideRamps

        tryPlace kind side sVal offRoll sizeRoll yawRoll cur =
            let
                smp =
                    splineSampleAtS track sVal

                spec =
                    scenerySpec kind offRoll sizeRoll

                rightFlat =
                    normalizeXZ smp.right

                lateralD =
                    side * (smp.halfWidth + spec.offsetM)

                posXZ =
                    Vec3.add smp.center (Vec3.scale lateralD rightFlat)

                q =
                    sampleAtHinted smp.s track (Vec3.getX posXZ) (Vec3.getZ posXZ)

                heading =
                    atan2 (Vec3.getX smp.tangent) (Vec3.getZ smp.tangent)

                yaw =
                    case kind of
                        FenceSection ->
                            heading

                        Signpost ->
                            heading + (yawRoll - 0.5) * 0.3

                        Cottage ->
                            heading + (yawRoll - 0.5) * 0.4

                        Barn ->
                            heading + (yawRoll - 0.5) * 0.4

                        BrickWall ->
                            heading + (yawRoll - 0.5) * 0.1

                        BrickGate ->
                            heading + (yawRoll - 0.5) * 0.2

                        SlopeBlock ->
                            heading + (yawRoll - 0.5) * 0.2

                        CountdownBoard ->
                            heading + (yawRoll - 0.5) * 0.1

                        Chalet ->
                            heading + (yawRoll - 0.5) * 0.3

                        PitGarage ->
                            heading

                        GardenGnome ->
                            -- Gnomes face the road, give or take.
                            heading + side * (pi / 2.0) + (yawRoll - 0.5) * 0.8

                        _ ->
                            yawRoll * 2.0 * pi

                clearOfRoad =
                    abs q.roadOffsetM - spec.radiusM >= q.halfWidthM + 0.3

                clearOfOthers =
                    List.take 12 cur.acc
                        |> List.all
                            (\o ->
                                let
                                    dx =
                                        Vec3.getX o.position - Vec3.getX posXZ

                                    dz =
                                        Vec3.getZ o.position - Vec3.getZ posXZ

                                    minGap =
                                        if kind == FenceSection && o.kind == FenceSection then
                                            0.0

                                        else
                                            o.radiusM + spec.radiusM + 0.3
                                in
                                dx * dx + dz * dz >= minGap * minGap
                            )

                clearOfRamps =
                    not (List.member kind dockBuiltScenery && nearRamp sVal)
            in
            if clearOfRoad && clearOfOthers && clearOfRamps && not (nearSideRamp side sVal) then
                Just
                    { id = cur.nextId
                    , kind = kind
                    , s = smp.s
                    , position = vec3 (Vec3.getX posXZ) (q.height - 0.12) (Vec3.getZ posXZ)
                    , radiusM = spec.radiusM
                    , heightM = spec.heightM
                    , yaw = yaw
                    }

            else
                Nothing

        withFence side n cur =
            if side < 0 then
                { cur | fenceLeft = n }

            else
                { cur | fenceRight = n }

        placeSide side sVal cur =
            let
                fenceRemaining =
                    if side < 0 then
                        cur.fenceLeft

                    else
                        cur.fenceRight

                ( rolls, nextRng ) =
                    Random.step (Random.list 5 (Random.float 0.0 1.0)) cur.rng

                roll i =
                    List.drop i rolls |> List.head |> Maybe.withDefault 0.5

                advanced =
                    { cur | rng = nextRng }

                push maybeObs fenceLeftAfter c =
                    case maybeObs of
                        Just obs ->
                            withFence side fenceLeftAfter { c | acc = obs :: c.acc, nextId = c.nextId + 1 }

                        Nothing ->
                            withFence side 0 c
            in
            if fenceRemaining > 0 then
                push (tryPlace FenceSection side sVal 0.0 0.0 0.0 advanced) (fenceRemaining - 1) advanced

            else if roll 0 < theme.fenceChance then
                push (tryPlace FenceSection side sVal 0.0 0.0 0.0 advanced) (2 + floor (roll 1 * 4.0)) advanced

            else if roll 0 < theme.fenceChance + theme.density then
                let
                    kind =
                        pickWeighted theme.weights (roll 1)
                in
                push (tryPlace kind side sVal (roll 2) (roll 3) (roll 4) advanced) 0 advanced

            else
                advanced

        step sVal cur =
            if nearGate sVal then
                { cur | fenceLeft = 0, fenceRight = 0 }

            else
                cur
                    |> placeSide -1.0 sVal
                    |> placeSide 1.0 sVal

        final =
            List.foldl step
                { acc = []
                , rng = Random.initialSeed (track.config.seed * 3571 + 91)
                , nextId = 1
                , fenceLeft = 0
                , fenceRight = 0
                }
                positions
    in
    List.reverse final.acc
        |> List.sortBy .s


{-| How high a brick ramp lifts the ground at one point, and how fast that lift
grows per meter of arc length (`grade`), so the car can climb it with the right
vertical speed.
-}
type alias RampLift =
    { lift : Float
    , grade : Float
    }


{-| Extra height of the brick ramps at arc length `s` and signed road offset
`signedD` (as in `TerrainSample.roadOffsetM`). Zero away from a ramp.

The ramps are not part of the ground heightfield: the ground mesh stays flat
under them and `Racer.TerrainMesh` draws their bricks with this same lift, while
`Racer.Physics` adds it to its wheel probes.

-}
brickRampLift : GeneratedTrack -> Float -> Float -> RampLift
brickRampLift track s signedD =
    List.foldl
        (\rp acc ->
            if s >= rp.startS && s < rp.endS then
                let
                    lenM =
                        max 0.5 (rp.endS - rp.startS)

                    lat =
                        brickRampLateral rp signedD
                in
                if lat <= 0.0 then
                    acc

                else
                    { lift = acc.lift + rp.lipHeightM * lat * (s - rp.startS) / lenM
                    , grade = acc.grade + rp.lipHeightM * lat / lenM
                    }

            else
                acc
        )
        { lift = 0.0, grade = 0.0 }
        track.brickRamps


{-| Full height over the middle of the ramp, falling in a straight line to the
road over its outer quarter on each side.
-}
brickRampLateral : BrickRamp -> Float -> Float
brickRampLateral rp signedD =
    let
        fromEdge =
            rp.halfWidthM - abs (signedD - rp.centerOffsetM)
    in
    clamp 0.0 1.0 (fromEdge / max 0.05 (rp.halfWidthM * 0.28))


{-| Put a few brick launch ramps on the course, seeded by the course seed.

Each one stands on straight, unbanked road away from the start, the checkpoint
arches, the finish, the humps, the rough patches and the loop (a car must reach
the loop on its wheels), with a clear landing after it. It covers one lane only
(about 40 % of the road width), on a seeded side, so there is always a free
lane beside it. They are tried every 10 m down the course (seeded shift and
side), at least 50 m apart, up to eight per course: rising 0.9 m over 8 m, or
only 0.7 m on pitches steeper than 38 %, where the road falls away under a
flying car faster; none on pitches over 46 %.

-}
placeBrickRamps : GeneratedTrack -> List BrickRamp
placeBrickRamps track =
    let
        rng0 =
            Random.initialSeed (track.config.seed * 7919 + 413)

        slotM =
            10.0

        slotCount =
            max 1 (floor (track.finishS / slotM))

        ( rolls, _ ) =
            Random.step (Random.list (slotCount + 1) (Random.float 0.0 1.0)) rng0

        rollArr =
            Array.fromList rolls

        roll i =
            Array.get i rollArr |> Maybe.withDefault 0.5

        lengthM =
            8.0

        lipHeightM =
            0.9

        steepLipHeightM =
            0.7

        landingM =
            22.0

        -- Ramps start at least this far apart, so each has its own run-up.
        spacingM =
            50.0

        candidates =
            List.range 0 (slotCount - 1)
                |> List.map
                    (\i ->
                        { startS = roll slotCount * slotM + toFloat i * slotM
                        , side =
                            if roll i < 0.5 then
                                -1.0

                            else
                                1.0
                        }
                    )

        steepestTangentY c =
            List.range 0 (round ((lengthM + landingM) / 3.0))
                |> List.map (\k -> Vec3.getY (splineSampleAtS track (c.startS + toFloat k * 3.0)).tangent)
                |> List.minimum
                |> Maybe.withDefault 0.0

        clearOf zoneStart zoneEnd c =
            c.startS + lengthM + landingM < zoneStart - 6.0 || c.startS > zoneEnd + 6.0

        -- Straight and unbanked, and not on the steepest pitches, where the
        -- road falls away under a flying car so fast that it lands hard.
        straightEnough c =
            List.range 0 (round ((lengthM + landingM) / 3.0))
                |> List.all
                    (\k ->
                        let
                            smp =
                                splineSampleAtS track (c.startS + toFloat k * 3.0)
                        in
                        abs smp.bankingRad < 0.06 && Vec3.getY smp.tangent > -0.46
                    )

        usable c =
            (c.startS > 40.0)
                && (c.startS + lengthM + landingM < track.finishS - 8.0)
                && List.all (\cp -> clearOf (cp.s - 8.0) (cp.s + 8.0) c) track.checkpoints
                && List.all (\rp -> clearOf rp.startS rp.endS c) track.ramps
                && List.all (\w -> clearOf w.startS w.endS c) track.washes
                && List.all (\lp -> clearOf (lp.entryS - loopRunInM) (lp.exitS + loopRunOutM) c) track.loops
                && straightEnough c

        toRamp c =
            let
                hw =
                    (splineSampleAtS track (c.startS + lengthM * 0.5)).halfWidth
            in
            { startS = c.startS
            , endS = c.startS + lengthM
            , lipHeightM =
                if steepestTangentY c < -0.38 then
                    steepLipHeightM

                else
                    lipHeightM
            , centerOffsetM = c.side * hw * 0.42
            , halfWidthM = hw * 0.4
            }

        spaced c picked =
            if List.all (\p -> abs (p.startS - c.startS) >= spacingM) picked then
                c :: picked

            else
                picked
    in
    candidates
        |> List.filter usable
        |> List.foldl spaced []
        |> List.reverse
        |> List.take 8
        |> List.map toRamp



-- ── Rounded side ramps ───────────────────────────────────────────────────────


{-| Height of the side ramps at one point, how fast it grows per metre of arc
length (`grade`) and per metre of signed road offset (`sideGrade`), so the car
can climb them with the right vertical speed and feel their slope.
-}
type alias SideLift =
    { lift : Float
    , grade : Float
    , sideGrade : Float
    }


{-| How far (m) behind its lip a side ramp's top fades back into the field.
Slowly enough that the ground behind it still rises away from the road, so a
car that flies over the lip rolls back to it instead of being trapped.
-}
sideRampBackM : Float
sideRampBackM =
    16.0


{-| Length (m) over which a side ramp's ends are rounded off along the road.
-}
sideRampTaperM : Float
sideRampTaperM =
    4.0


{-| Extra height of the rounded side ramps at arc length `s` and signed road
offset `signedD` (as in `TerrainSample.roadOffsetM`). Zero away from them.

Like the brick ramps they are not part of the ground heightfield:
`Racer.TerrainMesh` draws their bricks with this lift and `Racer.Physics` adds
it (and its slope) to its wheel probes.

-}
sideRampLift : GeneratedTrack -> Float -> Float -> SideLift
sideRampLift track s signedD =
    List.foldl
        (\sr acc ->
            if s > sr.startS && s < sr.endS && signedD * sr.side > sr.innerOffsetM then
                let
                    p =
                        sideRampProfile sr (abs signedD)

                    taper =
                        smoothstep sr.startS (sr.startS + sideRampTaperM) s
                            * (1.0 - smoothstep (sr.endS - sideRampTaperM) sr.endS s)

                    dTaper =
                        (smoothstep sr.startS (sr.startS + sideRampTaperM) (s + 0.01)
                            * (1.0 - smoothstep (sr.endS - sideRampTaperM) sr.endS (s + 0.01))
                            - taper
                        )
                            / 0.01
                in
                { lift = acc.lift + sr.heightM * taper * p.height
                , grade = acc.grade + sr.heightM * dTaper * p.height
                , sideGrade = acc.sideGrade + sr.side * sr.heightM * taper * p.slope
                }

            else
                acc
        )
        { lift = 0.0, grade = 0.0, sideGrade = 0.0 }
        track.sideRamps


{-| The quarter-pipe cross-section of a side ramp at distance `absD` from the
road centre: height as a share of its full height, and its slope per metre.
A circular arc that starts flat at the inner edge and steepens to about 60
degrees at the outer edge, then a grassy shelf that fades gently back into the
field.
-}
sideRampProfile : SideRamp -> Float -> { height : Float, slope : Float }
sideRampProfile sr absD =
    let
        widthM =
            max 0.1 (sr.outerOffsetM - sr.innerOffsetM)

        k =
            0.9

        norm =
            1.0 - sqrt (1.0 - k * k)

        u =
            (absD - sr.innerOffsetM) / widthM
    in
    if u <= 0.0 then
        { height = 0.0, slope = 0.0 }

    else if u <= 1.0 then
        { height = (1.0 - sqrt (1.0 - k * k * u * u)) / norm
        , slope = (k * k * u / sqrt (1.0 - k * k * u * u)) / norm / widthM
        }

    else
        let
            back =
                (absD - sr.outerOffsetM) / sideRampBackM
        in
        if back < 1.0 then
            { height = 1.0 - back * back * (3.0 - 2.0 * back)
            , slope = -6.0 * back * (1.0 - back) / sideRampBackM
            }

        else
            { height = 0.0, slope = 0.0 }


{-| Put a few rounded side ramps along the road edges, seeded by the course
seed (which 12 m slots are tried first, and on which side).

Each is 16 m long and 1.2 m high, on a seeded side, covering only the outer
metre of the road (so the road stays free) and the verge beside it, on road
that is not banked hard, away from the start, the finish, the checkpoint arches
and their gantries, the humps, the rough patches, the brick ramps and their
landings, and the loop. Up to five per course, at least 45 m apart.

-}
placeSideRamps : GeneratedTrack -> List SideRamp
placeSideRamps track =
    let
        lengthM =
            16.0

        slotM =
            12.0

        slotCount =
            max 1 (floor (track.finishS / slotM))

        ( rolls, _ ) =
            Random.step (Random.list (2 * slotCount + 1) (Random.float 0.0 1.0)) (Random.initialSeed (track.config.seed * 7919 + 1601))

        rollArr =
            Array.fromList rolls

        roll i =
            Array.get i rollArr |> Maybe.withDefault 0.5

        -- One candidate every 12 m (with a seeded shift), on a seeded side,
        -- tried in a seeded order, so the ramps land in different places on
        -- every seed but always where there is room.
        candidates =
            List.range 0 (slotCount - 1)
                |> List.map
                    (\i ->
                        { startS = roll (2 * slotCount) * slotM + toFloat i * slotM
                        , side =
                            if roll (2 * i + 1) < 0.5 then
                                -1.0

                            else
                                1.0
                        , order = roll (2 * i)
                        }
                    )
                |> List.sortBy .order

        clearOf zoneStart zoneEnd c =
            c.startS + lengthM < zoneStart - 6.0 || c.startS > zoneEnd + 6.0

        gentle c =
            List.range 0 (round (lengthM / 2.0))
                |> List.all (\k -> abs (splineSampleAtS track (c.startS + toFloat k * 2.0)).bankingRad < 0.15)

        usable c =
            (c.startS > 50.0)
                && (c.startS + lengthM < track.finishS - 20.0)
                && List.all (\cp -> clearOf (cp.s - 10.0) (cp.s + 10.0) c) track.checkpoints
                && List.all (\cp -> clearOf (cp.s - 63.0) (cp.s - 47.0) c) track.checkpoints
                && List.all (\rp -> clearOf rp.startS rp.endS c) track.ramps
                && List.all (\w -> clearOf w.startS w.endS c) track.washes
                && List.all (\rp -> clearOf rp.startS (rp.endS + 22.0) c) track.brickRamps
                -- Well before a loop, so a car that rode one still has the
                -- speed to get round.
                && List.all (\lp -> clearOf (lp.entryS - loopRunInM - 50.0) (lp.exitS + loopRunOutM) c) track.loops
                && gentle c

        toSideRamp c =
            let
                hw =
                    (splineSampleAtS track (c.startS + lengthM * 0.5)).halfWidth
            in
            { startS = c.startS
            , endS = c.startS + lengthM
            , side = c.side
            , innerOffsetM = hw - 1.0
            , outerOffsetM = hw + 1.4
            , heightM = 1.2
            }

        spaced c picked =
            if List.all (\p -> abs (p.startS - c.startS) >= 45.0) picked then
                c :: picked

            else
                picked
    in
    candidates
        |> List.filter usable
        |> List.foldl spaced []
        |> List.reverse
        |> List.take 5
        |> List.map toSideRamp



-- ── Brick loop-the-loops ─────────────────────────────────────────────────────


{-| Straight road (m) kept before a loop: no ramp lands in it, so a car reaches
the loop on its wheels.
-}
loopRunInM : Float
loopRunInM =
    30.0


{-| Straight road (m) kept after a loop for the car to settle.
-}
loopRunOutM : Float
loopRunOutM =
    16.0


{-| No scenery stands within this many metres of a loop, along the road.
-}
loopSceneryClearM : Float
loopSceneryClearM =
    8.0


{-| Angle round the loop (rad from each end) over which the lane narrows from
the full road to its own width.
-}
loopApronRad : Float
loopApronRad =
    0.7


{-| Angle round the loop (rad from each end) at which the lane has swung fully
to one side. The way up and the way down cross (seen from the side) near here,
so they are then a full `2 * driftM` apart.
-}
loopSwingRad : Float
loopSwingRad =
    1.0


{-| Put one brick loop-the-loop on the course, seeded by the course seed.

It goes near a seeded point a little before half way, where cars are up to
speed (about 15 to 23 m/s), on the nearest stretch of straight, unbanked road
with room for the run-in and run-out, away from the checkpoint arches and their
sponsor gantries, the humps and the rough patches. The 3.5 m radius loop needs
about 12 m/s at the entry; slower cars fall off near the top or roll back.

-}
placeLoops : GeneratedTrack -> List LoopSection
placeLoops track =
    let
        ( rolls, _ ) =
            Random.step (Random.list 2 (Random.float 0.0 1.0)) (Random.initialSeed (track.config.seed * 7919 + 977))

        roll i =
            List.drop i rolls |> List.head |> Maybe.withDefault 0.5

        radiusM =
            3.5

        stretchM =
            9.8

        preferredS =
            track.finishS * (0.4 + 0.16 * roll 0)

        side =
            if roll 1 < 0.5 then
                -1.0

            else
                1.0

        -- Nearest first: preferred, +3 m, -3 m, +6 m, ...
        candidates =
            List.range 0 140
                |> List.map
                    (\k ->
                        if modBy 2 k == 0 then
                            preferredS + 3.0 * toFloat (k // 2)

                        else
                            preferredS - 3.0 * toFloat ((k + 1) // 2)
                    )

        zoneOf entryS =
            { start = entryS - loopRunInM, end = entryS + stretchM + loopRunOutM }

        clearOf zoneStart zoneEnd z =
            z.end < zoneStart - 6.0 || z.start > zoneEnd + 6.0

        flatTangent sVal =
            normalizeXZ (splineSampleAtS track sVal).tangent

        straight z =
            List.all (\k -> abs (splineSampleAtS track (z.start + toFloat k * 2.0)).bankingRad < 0.05)
                (List.range 0 (round ((z.end - z.start) / 2.0)))
                && Vec3.dot (flatTangent z.start) (flatTangent z.end)
                > 0.995

        usable entryS =
            let
                z =
                    zoneOf entryS
            in
            (entryS > 160.0)
                && (z.end < track.finishS - 40.0)
                && List.all (\cp -> clearOf (cp.s - 8.0) (cp.s + 8.0) z) track.checkpoints
                && List.all (\cp -> clearOf (cp.s - 63.0) (cp.s - 47.0) z) track.checkpoints
                && List.all (\rp -> clearOf rp.startS rp.endS z) track.ramps
                && List.all (\w -> clearOf w.startS w.endS z) track.washes
                && straight z

        toLoop entryS =
            { entryS = entryS
            , exitS = entryS + stretchM
            , radiusM = radiusM
            , driftM = side * 2.0
            , laneHalfWidthM = 1.3
            , apronHalfWidthM = (splineSampleAtS track entryS).halfWidth
            }
    in
    candidates
        |> List.filter usable
        |> List.head
        |> Maybe.map (toLoop >> List.singleton)
        |> Maybe.withDefault []


{-| Where a car is on a loop and which way it faces, at angle `theta` round it
(0 at the entry, `2 pi` at the exit) and `lateral` metres off the lane centre
(signed like `roadOffsetM`).

`position` is on the lane surface; `tangent` is the unit direction of travel,
`up` the unit lane normal (towards the middle of the loop, so pointing down at
the top) and `right` the unit lane right, all at right angles. `curvature` is
how fast the lane bends towards `up` per metre (1 / radius at the top), and
`arcPerRad` the metres of lane per radian of `theta`.

-}
type alias LoopFrame =
    { position : Vec3
    , tangent : Vec3
    , up : Vec3
    , right : Vec3
    , curvature : Float
    , arcPerRad : Float
    }


{-| The lane of `loop` at angle `theta`, `lateral` metres off its centre (see
`LoopFrame`).

The lane is a circle of the loop's radius in the road's vertical plane, carried
along the road from the entry to the exit as it turns, and swung sideways by
the drift. Its ends lie on the road surface, along the road. It only reads the
road at the loop's two ends, so it is smooth everywhere.

-}
loopFrame : GeneratedTrack -> LoopSection -> Float -> Float -> LoopFrame
loopFrame track loop theta lateral =
    let
        entry =
            splineSampleAtS track loop.entryS

        exit =
            splineSampleAtS track loop.exitS

        f0 =
            normalizeSafe entry.tangent (vec3 0 0 1)

        f1 =
            normalizeSafe exit.tangent f0

        r0 =
            normalizeXZ entry.right

        r1 =
            normalizeXZ exit.right

        pointAt th =
            let
                u =
                    th / (2.0 * pi)

                fwd =
                    normalizeSafe (vec3Lerp u f0 f1) f0

                rightFlat =
                    normalizeSafe (vec3Lerp u r0 r1) r0

                upward =
                    normalizeSafe (Vec3.cross rightFlat fwd) (vec3 0 1 0)
            in
            vec3Lerp u entry.center exit.center
                |> Vec3.add (Vec3.scale (loop.radiusM * sin th) fwd)
                |> Vec3.add (Vec3.scale (loop.radiusM * (1.0 - cos th)) upward)
                |> Vec3.add (Vec3.scale (loopDrift loop th + lateral) rightFlat)

        h =
            1.0e-3

        p0 =
            pointAt theta

        pPlus =
            pointAt (theta + h)

        pMinus =
            pointAt (theta - h)

        d1 =
            Vec3.scale (0.5 / h) (Vec3.sub pPlus pMinus)

        d2 =
            Vec3.scale (1.0 / (h * h)) (Vec3.add (Vec3.sub pPlus (Vec3.scale 2.0 p0)) pMinus)

        arcPerRad =
            max 1.0e-4 (Vec3.length d1)

        tangent =
            Vec3.scale (1.0 / arcPerRad) d1

        bend =
            Vec3.scale (1.0 / (arcPerRad * arcPerRad))
                (Vec3.sub d2 (Vec3.scale (Vec3.dot d2 tangent) tangent))

        laneRightRaw =
            normalizeSafe (vec3Lerp (theta / (2.0 * pi)) r0 r1) r0

        laneRight =
            normalizeSafe (Vec3.sub laneRightRaw (Vec3.scale (Vec3.dot laneRightRaw tangent) tangent)) laneRightRaw

        up =
            normalizeSafe (Vec3.cross laneRight tangent) (vec3 0 1 0)
    in
    { position = p0
    , tangent = tangent
    , up = up
    , right = laneRight
    , curvature = Vec3.dot bend up
    , arcPerRad = arcPerRad
    }


{-| Sideways swing of the lane centre at angle `theta`: none at the ends, to
`driftM` on the way up, across the top to `-driftM` on the way down, and back.
Flat at both ends, so the lane meets the road straight.
-}
loopDrift : LoopSection -> Float -> Float
loopDrift loop theta =
    loop.driftM
        * (smoothstep 0.0 loopSwingRad theta
            - 2.0
            * smoothstep loopSwingRad (2.0 * pi - loopSwingRad) theta
            + smoothstep (2.0 * pi - loopSwingRad) (2.0 * pi) theta
          )


{-| Half width of a loop's lane at angle `theta`: the full road at both ends,
narrowing to the lane's own width over the first and last `loopApronRad`.
-}
loopLaneHalfWidth : LoopSection -> Float -> Float
loopLaneHalfWidth loop theta =
    let
        fromEnd =
            min theta (2.0 * pi - theta)
    in
    loop.laneHalfWidthM
        + (loop.apronHalfWidthM - loop.laneHalfWidthM)
        * (1.0 - smoothstep 0.0 loopApronRad fromEnd)


bucketObstacles : Float -> Int -> List TrackObstacle -> Array (List TrackObstacle)
bucketObstacles bucketSizeM bucketCount obsList =
    let
        initBuckets =
            Array.repeat bucketCount []

        insert obs buckets =
            let
                idx =
                    clamp 0 (bucketCount - 1) (floor (obs.s / bucketSizeM))

                existing =
                    Array.get idx buckets |> Maybe.withDefault []
            in
            Array.set idx (obs :: existing) buckets
    in
    List.foldl insert initBuckets obsList



-- ── Heightfield & Projection ──────────────────────────────────────────────────


type alias SplineProjection =
    { idx : Int
    , s : Float
    , overhangM : Float
    , center : Vec3
    , tangent : Vec3
    , right : Vec3
    , halfWidth : Float
    , bankingRad : Float
    , surface : SurfaceType
    , signedD : Float
    }


findClosestSplineProjection : Float -> GeneratedTrack -> Float -> Float -> SplineProjection
findClosestSplineProjection hintS track x z =
    let
        n =
            Array.length track.samples

        maxSegIdx =
            n - 2
    in
    if n < 2 then
        { idx = 0
        , s = 0.0
        , overhangM = 0.0
        , center = vec3 0 0 0
        , tangent = vec3 0 0 1
        , right = vec3 -1 0 0
        , halfWidth = track.config.defaultHalfWidthM
        , bankingRad = 0.0
        , surface = DirtRoad
        , signedD = 0.0
        }

    else if hintS < 0.0 then
        scanRange track.samples maxSegIdx 0 maxSegIdx x z

    else
        let
            hintIdx =
                clamp 0 maxSegIdx (floor (hintS / track.sampleSpacingM))

            lo =
                max 0 (hintIdx - 18)

            hi =
                min maxSegIdx (hintIdx + 24)

            localBest =
                scanRange track.samples maxSegIdx lo hi x z

            distFromCenterSq =
                let
                    dx =
                        x - Vec3.getX localBest.center

                    dz =
                        z - Vec3.getZ localBest.center
                in
                dx * dx + dz * dz
        in
        if (localBest.idx == lo || localBest.idx == hi) && distFromCenterSq > 900.0 then
            scanRange track.samples maxSegIdx 0 maxSegIdx x z

        else
            localBest


scanRange : Array TrackSample -> Int -> Int -> Int -> Float -> Float -> SplineProjection
scanRange samples maxSegIdx lo hi x z =
    let
        step idx best =
            case ( Array.get idx samples, Array.get (idx + 1) samples ) of
                ( Just s0, Just s1 ) ->
                    let
                        ax =
                            Vec3.getX s0.center

                        az =
                            Vec3.getZ s0.center

                        bx =
                            Vec3.getX s1.center

                        bz =
                            Vec3.getZ s1.center

                        vx =
                            bx - ax

                        vz =
                            bz - az

                        segLenSq =
                            vx * vx + vz * vz

                        rawT =
                            if segLenSq < 1.0e-7 then
                                0.0

                            else
                                ((x - ax) * vx + (z - az) * vz) / segLenSq

                        t =
                            clamp 0.0 1.0 rawT

                        px =
                            ax + t * vx

                        pz =
                            az + t * vz

                        dx =
                            x - px

                        dz =
                            z - pz

                        distSq =
                            dx * dx + dz * dz
                    in
                    if distSq < best.distSq then
                        let
                            smp =
                                interpolateTrackSample s0 s1 t

                            rightFlat =
                                normalizeXZ smp.right

                            signedD =
                                dx * Vec3.getX rightFlat + dz * Vec3.getZ rightFlat

                            segLenXZ =
                                sqrt (max 0.0 segLenSq)

                            overhangM =
                                if idx == 0 && rawT < 0.0 then
                                    rawT * segLenXZ

                                else if idx == maxSegIdx && rawT > 1.0 then
                                    (rawT - 1.0) * segLenXZ

                                else
                                    0.0
                        in
                        { distSq = distSq
                        , proj =
                            { idx = idx
                            , s = smp.s
                            , overhangM = overhangM
                            , center = smp.center
                            , tangent = smp.tangent
                            , right = smp.right
                            , halfWidth = smp.halfWidth
                            , bankingRad = smp.bankingRad
                            , surface = smp.surface
                            , signedD = signedD
                            }
                        }

                    else
                        best

                _ ->
                    best

        initProj =
            { distSq = 1.0e18
            , proj =
                { idx = lo
                , s = 0.0
                , overhangM = 0.0
                , center = vec3 0 0 0
                , tangent = vec3 0 0 1
                , right = vec3 -1 0 0
                , halfWidth = 6.0
                , bankingRad = 0.0
                , surface = DirtRoad
                , signedD = 0.0
                }
            }
    in
    List.range lo hi
        |> List.foldl step initProj
        |> .proj


heightNearProjection : GeneratedTrack -> SplineProjection -> Float -> Float -> Float
heightNearProjection track baseProj x z =
    let
        n =
            Array.length track.samples
    in
    if n < 2 then
        0.0

    else
        let
            maxSegIdx =
                n - 2

            lo =
                max 0 (baseProj.idx - 4)

            hi =
                min maxSegIdx (baseProj.idx + 4)

            localProj =
                scanRange track.samples maxSegIdx lo hi x z
        in
        heightWithProjection track localProj x z


heightWithProjection : GeneratedTrack -> SplineProjection -> Float -> Float -> Float
heightWithProjection track proj x z =
    let
        horizTanLen =
            max 0.25 (sqrt (Vec3.getX proj.tangent ^ 2 + Vec3.getZ proj.tangent ^ 2))

        extrapolatedSlopeY =
            proj.overhangM * (Vec3.getY proj.tangent / horizTanLen)

        baseY =
            Vec3.getY proj.center + extrapolatedSlopeY

        roadBankedY =
            baseY + proj.signedD * tan proj.bankingRad

        rampDelta =
            rampHeightOffset track.ramps proj.s proj.signedD

        washDelta =
            washHeightOffset track.washes proj.s proj.signedD proj.halfWidth

        pureRoadY =
            roadBankedY + rampDelta + washDelta

        absD =
            abs proj.signedD

        hw =
            proj.halfWidth
    in
    if absD <= hw then
        pureRoadY

    else
        let
            edgeBankedY =
                baseY + clamp -hw hw proj.signedD * tan proj.bankingRad

            edgeDelta =
                rampHeightOffset track.ramps proj.s (clamp -hw hw proj.signedD)

            edgeY =
                edgeBankedY + edgeDelta

            -- Metres beyond the road edge.
            u =
                absD - hw

            noiseVal =
                fieldUndulation track.config.seed x z

            -- Inside the recovery corridor the bank rises steadily away from the
            -- road with only a faint ripple (its slope stays well below the
            -- bank's), so the ground everywhere the car can reach leads back
            -- down to the road. The rolling fields start past the corridor wall.
            ripple =
                noiseVal * 0.03 * smoothstep 0.0 8.0 u

            fields =
                noiseVal * smoothstep recoveryCorridorM (recoveryCorridorM + 24.0) u
        in
        edgeY + recoveryBankGrade * u + recoveryBankCurve * u * u + ripple + fields


rampHeightOffset : List RampZone -> Float -> Float -> Float
rampHeightOffset ramps s signedD =
    List.foldl
        (\rp acc ->
            if s >= rp.startS && s <= rp.endS && abs signedD <= rp.halfWidthM then
                let
                    u =
                        clamp 0.0 1.0 ((s - rp.startS) / max 0.5 (rp.endS - rp.startS))

                    latBlend =
                        1.0 - smoothstep (rp.halfWidthM * 0.72) rp.halfWidthM (abs signedD)

                    profile =
                        if u < 0.86 then
                            (u / 0.86) ^ 1.35

                        else
                            max 0.0 (1.0 - (u - 0.86) / 0.14)
                in
                acc + rp.lipHeightM * profile * latBlend

            else
                acc
        )
        0.0
        ramps


washHeightOffset : List WashZone -> Float -> Float -> Float -> Float
washHeightOffset washes s signedD halfWidth =
    if abs signedD > halfWidth then
        0.0

    else
        List.foldl
            (\w acc ->
                if s >= w.startS && s <= w.endS then
                    let
                        env =
                            smoothstep w.startS (w.startS + 6.0) s
                                * (1.0 - smoothstep (w.endS - 6.0) w.endS s)
                    in
                    acc + sin (2.0 * pi * s / max 0.8 w.wavelengthM) * w.amplitudeM * env

                else
                    acc
            )
            0.0
            washes


interpolateTrackSample : TrackSample -> TrackSample -> Float -> TrackSample
interpolateTrackSample a b t =
    { s = lerp t a.s b.s
    , center = vec3Lerp t a.center b.center
    , tangent = normalizeSafe (vec3Lerp t a.tangent b.tangent) a.tangent
    , right = normalizeSafe (vec3Lerp t a.right b.right) a.right
    , normal = normalizeSafe (vec3Lerp t a.normal b.normal) a.normal
    , halfWidth = lerp t a.halfWidth b.halfWidth
    , bankingRad = lerp t a.bankingRad b.bankingRad
    , surface =
        if t < 0.5 then
            a.surface

        else
            b.surface
    }


lerp : Float -> Float -> Float -> Float
lerp t a b =
    a + (b - a) * t


vec3Lerp : Float -> Vec3 -> Vec3 -> Vec3
vec3Lerp t a b =
    vec3
        (lerp t (Vec3.getX a) (Vec3.getX b))
        (lerp t (Vec3.getY a) (Vec3.getY b))
        (lerp t (Vec3.getZ a) (Vec3.getZ b))


smoothstep : Float -> Float -> Float -> Float
smoothstep edge0 edge1 x =
    let
        denom =
            max 1.0e-5 (edge1 - edge0)

        t =
            clamp 0.0 1.0 ((x - edge0) / denom)
    in
    t * t * (3.0 - 2.0 * t)


normalizeSafe : Vec3 -> Vec3 -> Vec3
normalizeSafe v fallback =
    let
        len =
            Vec3.length v
    in
    if len < 1.0e-7 then
        fallback

    else
        Vec3.scale (1.0 / len) v
