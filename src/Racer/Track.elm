module Racer.Track exposing
    ( RampLift
    , brickRampLift
    , fieldUndulation
    , generate
    , groundHeightAt
    , obstaclesNear
    , recoveryBankGrade
    , recoveryCorridorM
    , removeObstacles
    , sampleAt
    , sampleAtHinted
    , splineSampleAtS
    , trackConfigFor
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
the racer can smash apart, and has a few brick-built launch
ramps on one lane of the road (`brickRamps`, see `brickRampLift`).

Exposes continuous `sampleAt` / `sampleAtHinted` heightmap + finite-difference surface
normal queries and O(1) spatial-bucketed `obstaclesNear` collision lookups.

-}

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Types
    exposing
        ( BrickRamp
        , Checkpoint
        , ControlPoint
        , GeneratedTrack
        , ObstacleKind(..)
        , RampZone
        , SegmentFeature(..)
        , SurfaceType(..)
        , TerrainSample
        , TrackConfig
        , TrackId(..)
        , TrackObstacle
        , TrackSample
        , WashZone
        )
import Random


{-| Return the canonical configuration preset for a given `TrackId` and seed.

All courses are short (about 0.4–0.55 km) and steep (about 30–35 % average grade).

-}
trackConfigFor : TrackId -> Int -> TrackConfig
trackConfigFor id seed =
    case id of
        FarmLane ->
            { id = FarmLane
            , seed = seed
            , lengthM = 960.0
            , dropM = 300.0
            , defaultHalfWidthM = 4.4
            }

        ForestTrail ->
            { id = ForestTrail
            , seed = seed
            , lengthM = 1080.0
            , dropM = 350.0
            , defaultHalfWidthM = 3.9
            }

        MeadowPath ->
            { id = MeadowPath
            , seed = seed
            , lengthM = 840.0
            , dropM = 300.0
            , defaultHalfWidthM = 3.3
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
            , washes = washes
            }

        rampedTrack =
            { bareTrack | brickRamps = placeBrickRamps bareTrack }

        obstacles =
            generateScenery rampedTrack
    in
    { rampedTrack
        | obstacles = obstacles
        , obstacleBuckets = bucketObstacles bucketSizeM bucketCount obstacles
    }


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


type alias BlueprintKnot =
    { sFrac : Float
    , xOffset : Float
    , yDropFrac : Float
    , zProgress : Float
    , halfWidth : Float
    , surface : SurfaceType
    , feature : Maybe SegmentFeature
    }


buildControlPoints : TrackConfig -> List ControlPoint
buildControlPoints config =
    let
        rawKnots =
            case config.id of
                FarmLane ->
                    farmLaneKnots

                ForestTrail ->
                    forestTrailKnots

                MeadowPath ->
                    meadowPathKnots

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


{-| Farm lane: run-in, steep left-hand drop, gravel right-hand sweep with ruts,
then the steepest pitch down into the farmyard. `xOffset` is in meters.
-}
farmLaneKnots : List BlueprintKnot
farmLaneKnots =
    [ { sFrac = 0.0, xOffset = 0.0, yDropFrac = 0.0, zProgress = 0.0, halfWidth = 4.6, surface = DirtRoad, feature = Nothing }
    , { sFrac = 0.11, xOffset = 0.0, yDropFrac = 0.06, zProgress = 0.1, halfWidth = 4.5, surface = DirtRoad, feature = Nothing }
    , { sFrac = 0.27, xOffset = -42.0, yDropFrac = 0.24, zProgress = 0.25, halfWidth = 4.3, surface = DirtRoad, feature = Nothing }
    , { sFrac = 0.43, xOffset = -56.0, yDropFrac = 0.38, zProgress = 0.4, halfWidth = 4.3, surface = GravelRoad, feature = Nothing }
    , { sFrac = 0.59, xOffset = 7.0, yDropFrac = 0.55, zProgress = 0.55, halfWidth = 4.2, surface = GravelRoad, feature = Just (RoughWash { amplitudeM = 0.06, wavelengthM = 2.6 }) }
    , { sFrac = 0.75, xOffset = 52.0, yDropFrac = 0.73, zProgress = 0.7, halfWidth = 4.3, surface = DirtRoad, feature = Nothing }
    , { sFrac = 0.9, xOffset = 35.0, yDropFrac = 0.93, zProgress = 0.85, halfWidth = 4.6, surface = DirtRoad, feature = Nothing }
    , { sFrac = 1.0, xOffset = 28.0, yDropFrac = 1.0, zProgress = 0.95, halfWidth = 5.0, surface = DirtRoad, feature = Nothing }
    ]


{-| Forest trail: packed-earth track with a right-hand bend over roots, a steep
gravel chute through a clearing, a left-hand bend and a log hump.
-}
forestTrailKnots : List BlueprintKnot
forestTrailKnots =
    [ { sFrac = 0.0, xOffset = 0.0, yDropFrac = 0.0, zProgress = 0.0, halfWidth = 4.1, surface = ForestTrack, feature = Nothing }
    , { sFrac = 0.1, xOffset = 0.0, yDropFrac = 0.05, zProgress = 0.09, halfWidth = 4.0, surface = ForestTrack, feature = Nothing }
    , { sFrac = 0.24, xOffset = 26.0, yDropFrac = 0.2, zProgress = 0.22, halfWidth = 3.8, surface = ForestTrack, feature = Nothing }
    , { sFrac = 0.38, xOffset = 34.0, yDropFrac = 0.33, zProgress = 0.35, halfWidth = 3.8, surface = ForestTrack, feature = Just (RoughWash { amplitudeM = 0.08, wavelengthM = 2.2 }) }
    , { sFrac = 0.53, xOffset = 6.0, yDropFrac = 0.52, zProgress = 0.49, halfWidth = 4.0, surface = GravelRoad, feature = Nothing }
    , { sFrac = 0.67, xOffset = -26.0, yDropFrac = 0.67, zProgress = 0.62, halfWidth = 3.8, surface = ForestTrack, feature = Nothing }
    , { sFrac = 0.8, xOffset = -20.0, yDropFrac = 0.81, zProgress = 0.75, halfWidth = 3.9, surface = ForestTrack, feature = Just (LaunchRamp { lengthM = 9.0, lipHeightM = 0.55 }) }
    , { sFrac = 0.92, xOffset = 0.0, yDropFrac = 0.94, zProgress = 0.87, halfWidth = 4.2, surface = GravelRoad, feature = Nothing }
    , { sFrac = 1.0, xOffset = 4.0, yDropFrac = 1.0, zProgress = 0.94, halfWidth = 4.6, surface = GravelRoad, feature = Nothing }
    ]


{-| Meadow footpath: narrowest and steepest, a field hump on the first bend,
tussocky grass on the last drop.
-}
meadowPathKnots : List BlueprintKnot
meadowPathKnots =
    [ { sFrac = 0.0, xOffset = 0.0, yDropFrac = 0.0, zProgress = 0.0, halfWidth = 3.6, surface = GrassPath, feature = Nothing }
    , { sFrac = 0.12, xOffset = 0.0, yDropFrac = 0.06, zProgress = 0.11, halfWidth = 3.4, surface = GrassPath, feature = Nothing }
    , { sFrac = 0.27, xOffset = -18.0, yDropFrac = 0.25, zProgress = 0.26, halfWidth = 3.2, surface = GrassPath, feature = Nothing }
    , { sFrac = 0.42, xOffset = 10.0, yDropFrac = 0.43, zProgress = 0.41, halfWidth = 3.2, surface = GrassPath, feature = Just (LaunchRamp { lengthM = 8.0, lipHeightM = 0.5 }) }
    , { sFrac = 0.58, xOffset = 26.0, yDropFrac = 0.6, zProgress = 0.56, halfWidth = 3.2, surface = DirtRoad, feature = Nothing }
    , { sFrac = 0.74, xOffset = 6.0, yDropFrac = 0.79, zProgress = 0.71, halfWidth = 3.2, surface = GrassPath, feature = Just (RoughWash { amplitudeM = 0.07, wavelengthM = 3.0 }) }
    , { sFrac = 0.89, xOffset = -12.0, yDropFrac = 0.94, zProgress = 0.86, halfWidth = 3.5, surface = GrassPath, feature = Nothing }
    , { sFrac = 1.0, xOffset = -10.0, yDropFrac = 1.0, zProgress = 0.96, halfWidth = 4.0, surface = GrassPath, feature = Nothing }
    ]


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


sceneryTheme : TrackId -> SceneryTheme
sceneryTheme trackId =
    case trackId of
        FarmLane ->
            { fenceChance = 0.06
            , density = 0.2
            , weights =
                [ ( LeafyTree, 20 ), ( Bush, 15 ), ( HayBale, 15 ), ( Rock, 6 ), ( Signpost, 6 ), ( PineTree, 4 ), ( Cottage, 4 ), ( Barn, 5 ), ( Crate, 7 ), ( Barrel, 6 ), ( GardenGnome, 3 ), ( BrickWall, 4 ), ( BrickPallet, 4 ), ( TileStack, 3 ), ( BrickGate, 3 ), ( SlopeBlock, 3 ) ]
            }

        ForestTrail ->
            { fenceChance = 0.015
            , density = 0.34
            , weights =
                [ ( PineTree, 38 ), ( LeafyTree, 16 ), ( Bush, 12 ), ( Rock, 12 ), ( Signpost, 5 ), ( HayBale, 2 ), ( Cottage, 3 ), ( Crate, 4 ), ( Barrel, 3 ), ( GardenGnome, 4 ), ( BrickWall, 1 ), ( BrickPallet, 3 ), ( TileStack, 3 ), ( BrickGate, 2 ), ( SlopeBlock, 2 ) ]
            }

        MeadowPath ->
            { fenceChance = 0.04
            , density = 0.17
            , weights =
                [ ( Bush, 21 ), ( LeafyTree, 14 ), ( Rock, 13 ), ( HayBale, 12 ), ( Signpost, 6 ), ( PineTree, 3 ), ( Cottage, 4 ), ( Barn, 3 ), ( Crate, 3 ), ( Barrel, 2 ), ( GardenGnome, 6 ), ( BrickWall, 4 ), ( BrickPallet, 3 ), ( TileStack, 3 ), ( BrickGate, 3 ), ( SlopeBlock, 3 ) ]
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

        Bleachers ->
            { offsetM = 3.6, radiusM = 2.2, heightM = 1.8 }


{-| The brick-built crashables of `Racer.SceneryModels` made only of the car
editor's dock parts. They also stay 6 m clear of the launch ramps, so a car
coming off a ramp does not land in one.
-}
dockBuiltScenery : List ObstacleKind
dockBuiltScenery =
    [ BrickPallet, TileStack, BrickGate, SlopeBlock ]


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
the start pad and checkpoint arches stay clear.

-}
generateScenery : GeneratedTrack -> List TrackObstacle
generateScenery track =
    let
        theme =
            sceneryTheme track.config.id

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

        nearRamp sVal =
            List.any (\rp -> sVal > rp.startS - 6.0 && sVal < rp.endS + 6.0) track.brickRamps
                || List.any (\rp -> sVal > rp.startS - 6.0 && sVal < rp.endS + 6.0) track.ramps

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

                        Bleachers ->
                            heading + side * (pi / 2.0)

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
            if clearOfRoad && clearOfOthers && clearOfRamps then
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

        placeBleacher sVal side cur =
            case tryPlace Bleachers side sVal 0.0 0.0 0.0 cur of
                Just obs ->
                    { cur | acc = obs :: cur.acc, nextId = cur.nextId + 1 }

                Nothing ->
                    let
                        smp =
                            splineSampleAtS track sVal

                        spec =
                            scenerySpec Bleachers 0.0 0.0

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
                    in
                    { cur
                        | acc =
                            { id = cur.nextId
                            , kind = Bleachers
                            , s = smp.s
                            , position = vec3 (Vec3.getX posXZ) (q.height - 0.12) (Vec3.getZ posXZ)
                            , radiusM = spec.radiusM
                            , heightM = spec.heightM
                            , yaw = heading + side * (pi / 2.0)
                            }
                                :: cur.acc
                        , nextId = cur.nextId + 1
                    }

        startCursor =
            { acc = []
            , rng = Random.initialSeed (track.config.seed * 3571 + 91)
            , nextId = 1
            , fenceLeft = 0
            , fenceRight = 0
            }
                |> placeBleacher 10.0 -1.0
                |> placeBleacher 10.0 1.0

        procedural =
            List.foldl step startCursor positions

        finishBleacherS =
            track.finishS - 6.0

        final =
            procedural
                |> placeBleacher finishBleacherS -1.0
                |> placeBleacher finishBleacherS 1.0
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
arches, the finish, the humps and the rough patches, with a clear landing after
it. It covers one lane only (about 40 % of the road width), on a seeded side, so
there is always a free lane beside it.

-}
placeBrickRamps : GeneratedTrack -> List BrickRamp
placeBrickRamps track =
    let
        rng0 =
            Random.initialSeed (track.config.seed * 7919 + 413)

        ( rolls, _ ) =
            Random.step (Random.list 8 (Random.float 0.0 1.0)) rng0

        roll i =
            List.drop i rolls |> List.head |> Maybe.withDefault 0.5

        lengthM =
            6.5

        lipHeightM =
            0.75

        landingM =
            18.0

        candidates =
            [ 0.15, 0.36, 0.6, 0.86 ]
                |> List.indexedMap
                    (\i frac ->
                        { startS = track.finishS * frac + (roll (2 * i) - 0.5) * 8.0
                        , side =
                            if roll (2 * i + 1) < 0.5 then
                                -1.0

                            else
                                1.0
                        }
                    )

        clearOf zoneStart zoneEnd c =
            c.startS + lengthM + landingM < zoneStart - 6.0 || c.startS > zoneEnd + 6.0

        straightEnough c =
            List.range 0 (round ((lengthM + landingM) / 3.0))
                |> List.all
                    (\k ->
                        abs (splineSampleAtS track (c.startS + toFloat k * 3.0)).bankingRad < 0.06
                    )

        usable c =
            (c.startS > 40.0)
                && (c.startS + lengthM + landingM < track.finishS - 8.0)
                && List.all (\cp -> clearOf (cp.s - 8.0) (cp.s + 8.0) c) track.checkpoints
                && List.all (\rp -> clearOf rp.startS rp.endS c) track.ramps
                && List.all (\w -> clearOf w.startS w.endS c) track.washes
                && straightEnough c

        toRamp c =
            let
                hw =
                    (splineSampleAtS track (c.startS + lengthM * 0.5)).halfWidth
            in
            { startS = c.startS
            , endS = c.startS + lengthM
            , lipHeightM = lipHeightM
            , centerOffsetM = c.side * hw * 0.42
            , halfWidthM = hw * 0.4
            }
    in
    candidates
        |> List.filter usable
        |> List.take 3
        |> List.map toRamp


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
