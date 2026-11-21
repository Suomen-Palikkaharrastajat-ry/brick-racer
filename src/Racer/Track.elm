module Racer.Track exposing
    ( generate
    , mountainRidgeHeight
    , obstaclesNear
    , sampleAt
    , sampleAtHinted
    , splineSampleAtS
    , trackConfigFor
    )

{-| Procedural 3D downhill mountain course generator and spatial terrain sampler.

Generates deterministic Catmull-Rom 3D tracks for:

  - `PineRidge`: wide sweeping tarmac descent with jump ramps,
  - `QuarrySwitchbacks`: tight banked hairpins, chicanes, and cobble flats,
  - `AlpineGlacier`: steep high-altitude alpine chutes and washboard frost ribs.

Exposes continuous `sampleAt` / `sampleAtHinted` heightmap + finite-difference surface
normal queries and O(1) spatial-bucketed `obstaclesNear` collision lookups.

-}

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Types
    exposing
        ( Checkpoint
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
-}
trackConfigFor : TrackId -> Int -> TrackConfig
trackConfigFor id seed =
    case id of
        PineRidge ->
            { id = PineRidge
            , seed = seed
            , lengthM = 1100.0
            , dropM = 220.0
            , defaultHalfWidthM = 6.4
            }

        QuarrySwitchbacks ->
            { id = QuarrySwitchbacks
            , seed = seed
            , lengthM = 1350.0
            , dropM = 310.0
            , defaultHalfWidthM = 5.8
            }

        AlpineGlacier ->
            { id = AlpineGlacier
            , seed = seed
            , lengthM = 1600.0
            , dropM = 410.0
            , defaultHalfWidthM = 5.4
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
                    , surface = AsphaltTrack
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

        ( ramps, washes, chicaneZones ) =
            extractFeatureZones totalLengthM controlPoints

        obstacles =
            generateObstacles config.seed samples ramps checkpoints chicaneZones finishS

        bucketCount =
            max 1 (ceiling (totalLengthM / bucketSizeM) + 1)

        obstacleBuckets =
            bucketObstacles bucketSizeM bucketCount obstacles
    in
    { config = config
    , samples = samples
    , sampleSpacingM = sampleSpacingM
    , totalLengthM = totalLengthM
    , totalDropM = totalDropM
    , startPosition = Vec3.add startSample.center (vec3 0 0.08 0)
    , startYaw = startYaw
    , finishS = finishS
    , checkpoints = checkpoints
    , obstacles = obstacles
    , obstacleBuckets = obstacleBuckets
    , bucketSizeM = bucketSizeM
    , ramps = ramps
    , washes = washes
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

            else if absD <= proj.halfWidth + 3.5 then
                GravelShoulder

            else
                AlpineGrass
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
        , surface = AsphaltTrack
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
                , surface = AsphaltTrack
                }


{-| Smooth deterministic off-track mountain ridge height modulation around a valley floor.
-}
mountainRidgeHeight : Int -> Float -> Float -> Float
mountainRidgeHeight seed x z =
    let
        phaseA =
            toFloat (modBy 997 (abs seed + 17)) * 0.013

        phaseB =
            toFloat (modBy 613 (abs seed + 41)) * 0.019

        low =
            sin (x * 0.022 + phaseA) * cos (z * 0.019 - phaseB) * 9.5

        mid =
            sin ((x + z) * 0.051 - phaseB) * 3.8

        detail =
            cos (x * 0.14 - z * 0.11 + phaseA) * 0.9
    in
    low + mid + detail



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
                PineRidge ->
                    pineRidgeKnots

                QuarrySwitchbacks ->
                    quarrySwitchbackKnots

                AlpineGlacier ->
                    alpineGlacierKnots

        lenScale =
            config.lengthM

        startAltitude =
            config.dropM + 20.0

        ( finalKnots, _ ) =
            List.foldl
                (\knot ( acc, rng ) ->
                    let
                        ( jitterX, rng1 ) =
                            Random.step (Random.float -9.5 9.5) rng

                        ( jitterY, rng2 ) =
                            Random.step (Random.float -1.4 1.4) rng1

                        envelope =
                            if knot.sFrac <= 0.06 || knot.sFrac >= 0.92 then
                                0.0

                            else
                                sin (pi * clamp 0.0 1.0 ((knot.sFrac - 0.06) / 0.86))

                        x =
                            knot.xOffset * (lenScale / 1100.0) + jitterX * envelope

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


pineRidgeKnots : List BlueprintKnot
pineRidgeKnots =
    [ { sFrac = 0.0, xOffset = 0.0, yDropFrac = 0.0, zProgress = 0.0, halfWidth = 6.5, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.03, xOffset = 0.0, yDropFrac = 0.06, zProgress = 0.035, halfWidth = 6.5, surface = AsphaltTrack, feature = Just SpeedGate }
    , { sFrac = 0.11, xOffset = 22.0, yDropFrac = 0.14, zProgress = 0.12, halfWidth = 6.3, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.21, xOffset = 68.0, yDropFrac = 0.22, zProgress = 0.21, halfWidth = 6.2, surface = AsphaltTrack, feature = Just (LaunchRamp { lengthM = 14.0, lipHeightM = 0.85 }) }
    , { sFrac = 0.32, xOffset = 20.0, yDropFrac = 0.32, zProgress = 0.31, halfWidth = 6.2, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.44, xOffset = -64.0, yDropFrac = 0.44, zProgress = 0.42, halfWidth = 6.1, surface = AsphaltTrack, feature = Just (RoughWash { amplitudeM = 0.09, wavelengthM = 2.8 }) }
    , { sFrac = 0.56, xOffset = -92.0, yDropFrac = 0.56, zProgress = 0.53, halfWidth = 6.2, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.67, xOffset = -18.0, yDropFrac = 0.68, zProgress = 0.64, halfWidth = 6.2, surface = AsphaltTrack, feature = Just (LaunchRamp { lengthM = 15.0, lipHeightM = 1.05 }) }
    , { sFrac = 0.78, xOffset = 62.0, yDropFrac = 0.81, zProgress = 0.75, halfWidth = 6.2, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.89, xOffset = 28.0, yDropFrac = 0.93, zProgress = 0.86, halfWidth = 6.4, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.96, xOffset = 10.0, yDropFrac = 0.985, zProgress = 0.94, halfWidth = 6.8, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 1.0, xOffset = 10.0, yDropFrac = 1.0, zProgress = 0.99, halfWidth = 7.0, surface = AsphaltTrack, feature = Nothing }
    ]


quarrySwitchbackKnots : List BlueprintKnot
quarrySwitchbackKnots =
    [ { sFrac = 0.0, xOffset = 0.0, yDropFrac = 0.0, zProgress = 0.0, halfWidth = 5.8, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.04, xOffset = 0.0, yDropFrac = 0.065, zProgress = 0.035, halfWidth = 5.8, surface = AsphaltTrack, feature = Just SpeedGate }
    , { sFrac = 0.14, xOffset = 58.0, yDropFrac = 0.15, zProgress = 0.11, halfWidth = 5.5, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.24, xOffset = 112.0, yDropFrac = 0.24, zProgress = 0.18, halfWidth = 5.4, surface = GravelShoulder, feature = Just (Chicane { sideOffsetM = 2.0, barrierCount = 4 }) }
    , { sFrac = 0.35, xOffset = 18.0, yDropFrac = 0.36, zProgress = 0.26, halfWidth = 5.4, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.46, xOffset = -102.0, yDropFrac = 0.48, zProgress = 0.34, halfWidth = 5.4, surface = CobbleStone, feature = Just (LaunchRamp { lengthM = 13.0, lipHeightM = 0.9 }) }
    , { sFrac = 0.58, xOffset = -45.0, yDropFrac = 0.61, zProgress = 0.44, halfWidth = 5.5, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.69, xOffset = 94.0, yDropFrac = 0.74, zProgress = 0.52, halfWidth = 5.3, surface = AsphaltTrack, feature = Just (Chicane { sideOffsetM = 1.9, barrierCount = 5 }) }
    , { sFrac = 0.8, xOffset = 40.0, yDropFrac = 0.85, zProgress = 0.63, halfWidth = 5.5, surface = CobbleStone, feature = Nothing }
    , { sFrac = 0.9, xOffset = -28.0, yDropFrac = 0.94, zProgress = 0.74, halfWidth = 5.8, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.96, xOffset = -14.0, yDropFrac = 0.988, zProgress = 0.82, halfWidth = 6.2, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 1.0, xOffset = -14.0, yDropFrac = 1.0, zProgress = 0.87, halfWidth = 6.4, surface = AsphaltTrack, feature = Nothing }
    ]


alpineGlacierKnots : List BlueprintKnot
alpineGlacierKnots =
    [ { sFrac = 0.0, xOffset = 0.0, yDropFrac = 0.0, zProgress = 0.0, halfWidth = 5.6, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.04, xOffset = 0.0, yDropFrac = 0.07, zProgress = 0.032, halfWidth = 5.4, surface = AsphaltTrack, feature = Just SpeedGate }
    , { sFrac = 0.13, xOffset = -44.0, yDropFrac = 0.15, zProgress = 0.11, halfWidth = 5.1, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.24, xOffset = -96.0, yDropFrac = 0.27, zProgress = 0.2, halfWidth = 5.0, surface = CobbleStone, feature = Just (RoughWash { amplitudeM = 0.12, wavelengthM = 2.4 }) }
    , { sFrac = 0.35, xOffset = -22.0, yDropFrac = 0.39, zProgress = 0.3, halfWidth = 5.0, surface = AsphaltTrack, feature = Just (LaunchRamp { lengthM = 16.0, lipHeightM = 1.25 }) }
    , { sFrac = 0.47, xOffset = 78.0, yDropFrac = 0.51, zProgress = 0.4, halfWidth = 4.9, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.58, xOffset = 104.0, yDropFrac = 0.64, zProgress = 0.5, halfWidth = 4.9, surface = GravelShoulder, feature = Just (Chicane { sideOffsetM = 1.8, barrierCount = 5 }) }
    , { sFrac = 0.7, xOffset = 18.0, yDropFrac = 0.76, zProgress = 0.61, halfWidth = 5.0, surface = AsphaltTrack, feature = Just (RoughWash { amplitudeM = 0.1, wavelengthM = 2.6 }) }
    , { sFrac = 0.81, xOffset = -68.0, yDropFrac = 0.87, zProgress = 0.72, halfWidth = 5.1, surface = AsphaltTrack, feature = Just (LaunchRamp { lengthM = 15.0, lipHeightM = 1.15 }) }
    , { sFrac = 0.9, xOffset = -24.0, yDropFrac = 0.95, zProgress = 0.82, halfWidth = 5.4, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 0.96, xOffset = 0.0, yDropFrac = 0.989, zProgress = 0.89, halfWidth = 5.8, surface = AsphaltTrack, feature = Nothing }
    , { sFrac = 1.0, xOffset = 0.0, yDropFrac = 1.0, zProgress = 0.94, halfWidth = 6.0, surface = AsphaltTrack, feature = Nothing }
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


extractFeatureZones : Float -> List ControlPoint -> ( List RampZone, List WashZone, List ( Float, Float, Int ) )
extractFeatureZones totalLen cps =
    let
        count =
            max 1 (List.length cps - 1)

        process i cp ( rampsAcc, washesAcc, chicanesAcc ) =
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
                    , chicanesAcc
                    )

                Just (RoughWash w) ->
                    ( rampsAcc
                    , { startS = approxS - 28.0
                      , endS = approxS + 28.0
                      , amplitudeM = w.amplitudeM
                      , wavelengthM = w.wavelengthM
                      }
                        :: washesAcc
                    , chicanesAcc
                    )

                Just (Chicane c) ->
                    ( rampsAcc
                    , washesAcc
                    , ( approxS, c.sideOffsetM, c.barrierCount ) :: chicanesAcc
                    )

                _ ->
                    ( rampsAcc, washesAcc, chicanesAcc )

        ( rList, wList, cList ) =
            List.foldl (\( idx, cp ) acc -> process idx cp acc) ( [], [], [] ) (List.indexedMap Tuple.pair cps)
    in
    ( List.reverse rList, List.reverse wList, List.reverse cList )


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


generateObstacles :
    Int
    -> Array TrackSample
    -> List RampZone
    -> List Checkpoint
    -> List ( Float, Float, Int )
    -> Float
    -> List TrackObstacle
generateObstacles seed samples ramps checkpoints chicaneZones finishS =
    let
        rng0 =
            Random.initialSeed (seed * 3571 + 91)

        totalS =
            Array.get (Array.length samples - 1) samples
                |> Maybe.map .s
                |> Maybe.withDefault finishS

        sampleAtStep sVal =
            Array.get (clamp 0 (Array.length samples - 1) (floor (sVal / 2.0))) samples

        nearCheckpoint sVal =
            List.any (\cp -> abs (cp.s - sVal) < 16.0) checkpoints

        nearRamp sVal =
            List.any (\rp -> sVal >= rp.startS - 6.0 && sVal <= rp.endS + 14.0) ramps

        stepSList =
            List.range 6 (floor ((totalS - 20.0) / 9.0))
                |> List.map (\k -> toFloat k * 9.0)

        ( roadsideObs, rngAfterRoad, nextId0 ) =
            List.foldl
                (\sVal ( acc, rng, obsId ) ->
                    if sVal < 58.0 || sVal > finishS - 18.0 || nearCheckpoint sVal then
                        ( acc, rng, obsId )

                    else
                        case sampleAtStep sVal of
                            Nothing ->
                                ( acc, rng, obsId )

                            Just smp ->
                                let
                                    ( roll, rng1 ) =
                                        Random.step (Random.float 0.0 1.0) rng

                                    ( sideSignFloat, rng2 ) =
                                        Random.step (Random.uniform -1.0 [ 1.0 ]) rng1

                                    ( extraOffset, rng3 ) =
                                        Random.step (Random.float 3.4 13.5) rng2

                                    ( kindRoll, rng4 ) =
                                        Random.step (Random.float 0.0 1.0) rng3

                                    ( sizeScale, rng5 ) =
                                        Random.step (Random.float 0.85 1.35) rng4
                                in
                                if roll < 0.25 then
                                    ( acc, rng5, obsId )

                                else
                                    let
                                        lateralD =
                                            sideSignFloat * (smp.halfWidth + extraOffset)

                                        rightFlat =
                                            normalizeXZ smp.right

                                        posXZ =
                                            Vec3.add smp.center (Vec3.scale lateralD rightFlat)

                                        -- Road verge/mountain height estimate for clean grounding
                                        groundY =
                                            Vec3.getY smp.center + lateralD * tan smp.bankingRad + max 0.0 (extraOffset - 3.5) * 0.28

                                        ( kind, rad, ht ) =
                                            if kindRoll < 0.62 then
                                                ( PineTree, 0.72 * sizeScale, 5.2 * sizeScale )

                                            else if kindRoll < 0.88 then
                                                ( Boulder, 0.95 * sizeScale, 1.45 * sizeScale )

                                            else
                                                ( BarrierPost, 0.45, 1.25 )

                                        obs =
                                            { id = obsId
                                            , kind = kind
                                            , s = smp.s
                                            , position = vec3 (Vec3.getX posXZ) groundY (Vec3.getZ posXZ)
                                            , radiusM = rad
                                            , heightM = ht
                                            }
                                    in
                                    ( obs :: acc, rng5, obsId + 1 )
                )
                ( [], rng0, 1 )
                stepSList

        ( chicaneObs, _ ) =
            List.foldl
                (\( centerS, offsetM, count ) ( acc, oid ) ->
                    let
                        indices =
                            List.range 0 (max 1 count - 1)

                        created =
                            List.filterMap
                                (\idx ->
                                    let
                                        gateS =
                                            centerS + (toFloat idx - toFloat (count - 1) * 0.5) * 14.0
                                    in
                                    if nearRamp gateS || gateS < 65.0 || gateS > finishS - 25.0 then
                                        Nothing

                                    else
                                        sampleAtStep gateS
                                            |> Maybe.map
                                                (\smp ->
                                                    let
                                                        side =
                                                            if modBy 2 idx == 0 then
                                                                1.0

                                                            else
                                                                -1.0

                                                        d =
                                                            clamp -(smp.halfWidth * 0.52) (smp.halfWidth * 0.52) (side * offsetM)

                                                        rightFlat =
                                                            normalizeXZ smp.right

                                                        posXZ =
                                                            Vec3.add smp.center (Vec3.scale d rightFlat)

                                                        y =
                                                            Vec3.getY smp.center + d * tan smp.bankingRad
                                                    in
                                                    { id = oid + idx
                                                    , kind = ChicaneBlock
                                                    , s = smp.s
                                                    , position = vec3 (Vec3.getX posXZ) y (Vec3.getZ posXZ)
                                                    , radiusM = 0.85
                                                    , heightM = 1.05
                                                    }
                                                )
                                )
                                indices
                    in
                    ( created ++ acc, oid + max 1 count + 1 )
                )
                ( [], nextId0 + 100 )
                chicaneZones

        _ =
            rngAfterRoad
    in
    List.reverse roadsideObs
        ++ List.reverse chicaneObs
        |> List.sortBy .s


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
        , surface = AsphaltTrack
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
                , surface = AsphaltTrack
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

        shoulderOuter =
            hw + 3.5
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

            shoulderDip =
                -0.18 * smoothstep hw shoulderOuter absD

            noiseVal =
                mountainRidgeHeight track.config.seed x z

            valleyRise =
                max 0.0 (absD - shoulderOuter) * 0.34

            mountainTarget =
                edgeY + valleyRise + noiseVal * smoothstep hw (hw + 26.0) absD
        in
        if absD <= shoulderOuter then
            let
                t =
                    smoothstep hw shoulderOuter absD
            in
            lerp t edgeY (edgeY + shoulderDip + noiseVal * 0.08)

        else
            let
                t =
                    smoothstep shoulderOuter (hw + 26.0) absD
            in
            lerp t (edgeY + shoulderDip + noiseVal * 0.08) mountainTarget


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
