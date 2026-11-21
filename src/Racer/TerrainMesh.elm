module Racer.TerrainMesh exposing
    ( TrackChunk
    , TrackRenderBundle
    , buildGaragePlinthMesh
    , buildTrackRenderBundle
    )

{-| Chunked WebGL terrain, roadbed ribbon, brick apex curbs, launch ramps,
checkpoint/finish arches, authentic brick-built pine trees, faceted boulders,
chicane barriers, bollards, and garage display plinth meshes.
-}

import Array exposing (Array)
import LDraw.Colors as Colors
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.TerrainShader exposing (TerrainVertex)
import Racer.Track as Track
import Racer.Types
    exposing
        ( Checkpoint
        , GeneratedTrack
        , ObstacleKind(..)
        , TrackId(..)
        , TrackObstacle
        , TrackSample
        )
import WebGL


{-| One spatial `100 m` slice of terrain, roadbed, and static trackside geometry.
-}
type alias TrackChunk =
    { chunkIndex : Int
    , minS : Float
    , maxS : Float
    , boundsCenter : Vec3
    , boundsRadius : Float
    , mesh : WebGL.Mesh TerrainVertex
    }


{-| Pre-built chunk array for O(1) visible-window frustum culling during gameplay.
-}
type alias TrackRenderBundle =
    { trackId : TrackId
    , seed : Int
    , chunkSizeM : Float
    , chunks : Array TrackChunk
    }


{-| Resolve official LDraw color code to RGB vector.
-}
ldrawColor : Int -> Vec3
ldrawColor code =
    let
        c =
            Colors.resolveColor 0 code
    in
    vec3 c.r c.g c.b


{-| Build chunked static WebGL meshes (`100 m` chunks) once per track load.
-}
buildTrackRenderBundle : GeneratedTrack -> TrackRenderBundle
buildTrackRenderBundle track =
    let
        chunkSizeM =
            100.0

        numChunks =
            max 1 (ceiling (track.totalLengthM / chunkSizeM))

        chunks =
            List.range 0 (numChunks - 1)
                |> List.map (buildChunk track chunkSizeM numChunks)
                |> Array.fromList
    in
    { trackId = track.config.id
    , seed = track.config.seed
    , chunkSizeM = chunkSizeM
    , chunks = chunks
    }


{-| Circular display plinth with tarmac & red/white LEGO curb blocks with studs for Garage inspection.
-}
buildGaragePlinthMesh : WebGL.Mesh TerrainVertex
buildGaragePlinthMesh =
    let
        segments =
            32

        radius =
            2.6

        topY =
            -0.01

        botY =
            -0.22

        topCol =
            vec3 0.24 0.26 0.3

        curbRed =
            ldrawColor 4

        curbWhite =
            ldrawColor 15

        ring i =
            let
                a0 =
                    2.0 * pi * toFloat i / toFloat segments

                a1 =
                    2.0 * pi * toFloat (i + 1) / toFloat segments

                ( c0, s0 ) =
                    ( cos a0, sin a0 )

                ( c1, s1 ) =
                    ( cos a1, sin a1 )

                rIn =
                    radius * 0.86

                curbCol =
                    if modBy 2 i == 0 then
                        curbRed

                    else
                        curbWhite

                centerV =
                    vec3 0 topY 0

                in0 =
                    vec3 (rIn * c0) topY (rIn * s0)

                in1 =
                    vec3 (rIn * c1) topY (rIn * s1)

                out0 =
                    vec3 (radius * c0) topY (radius * s0)

                out1 =
                    vec3 (radius * c1) topY (radius * s1)

                low0 =
                    vec3 (radius * c0) botY (radius * s0)

                low1 =
                    vec3 (radius * c1) botY (radius * s1)

                upN =
                    vec3 0 1 0

                sideN =
                    normalizeSafe (vec3 (c0 + c1) 0 (s0 + s1)) (vec3 0 1 0)

                -- 3D stud on the curb block
                aMid =
                    (a0 + a1) * 0.5

                rMid =
                    (radius + rIn) * 0.5

                studCenter =
                    vec3 (rMid * cos aMid) topY (rMid * sin aMid)

                studTris =
                    cylinderAt studCenter 0.045 0.035 6 curbCol
            in
            [ tri centerV in1 in0 upN topCol ]
                ++ quad out0 in0 in1 out1 upN curbCol
                ++ quad low0 out0 out1 low1 sideN (vec3 0.35 0.36 0.38)
                ++ studTris
    in
    List.concatMap ring (List.range 0 (segments - 1))
        |> WebGL.triangles


buildChunk : GeneratedTrack -> Float -> Int -> Int -> TrackChunk
buildChunk track chunkSizeM numChunks chunkIdx =
    let
        minS =
            toFloat chunkIdx * chunkSizeM

        maxS =
            min track.totalLengthM (minS + chunkSizeM)

        iStart =
            clamp 0 (Array.length track.samples - 2) (floor (minS / track.sampleSpacingM))

        iEnd =
            let
                rawEnd =
                    clamp iStart (Array.length track.samples - 2) (ceiling (maxS / track.sampleSpacingM) - 1)
            in
            if chunkIdx == numChunks - 1 then
                max iStart (Array.length track.samples - 2)

            else
                rawEnd

        startApronTris =
            if chunkIdx == 0 then
                buildStartApronTris track

            else
                []

        ribbonTris =
            startApronTris
                ++ (List.range iStart iEnd
                        |> List.concatMap (buildStripBetweenSamples track)
                   )

        gateTris =
            let
                startGate =
                    if chunkIdx == 0 then
                        Array.get 0 track.samples
                            |> Maybe.map (\s0 -> buildArchTris s0 False True)
                            |> Maybe.withDefault []

                    else
                        []

                cpGates =
                    track.checkpoints
                        |> List.filter (\cp -> cp.s >= minS && cp.s < maxS + 2.0)
                        |> List.concatMap (\cp -> buildCheckpointTris track cp)
            in
            startGate ++ cpGates

        obstacleTris =
            track.obstacles
                |> List.filter (\o -> o.s >= minS && o.s < maxS)
                |> List.concatMap (buildObstacleTris track.config.id)

        allTris =
            ribbonTris ++ gateTris ++ obstacleTris

        centerSample =
            Track.splineSampleAtS track ((minS + maxS) * 0.5)
    in
    { chunkIndex = chunkIdx
    , minS = minS
    , maxS = maxS
    , boundsCenter = centerSample.center
    , boundsRadius = chunkSizeM * 0.85 + 52.0
    , mesh = WebGL.triangles allTris
    }


buildStartApronTris : GeneratedTrack -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildStartApronTris track =
    case Array.get 0 track.samples of
        Nothing ->
            []

        Just s0 ->
            let
                fwdXZ =
                    normalizeXZ s0.tangent

                shiftedSample offsetM =
                    { s0
                        | s = 0.0
                        , center = Vec3.add s0.center (Vec3.scale offsetM fwdXZ)
                    }

                rowBack2 =
                    sampleCrossSectionVertices track (shiftedSample -16.0)

                rowBack1 =
                    sampleCrossSectionVertices track (shiftedSample -8.0)

                rowZero =
                    sampleCrossSectionVertices track s0
            in
            stitchCrossSections track.config.id -2 False rowBack2 rowBack1
                ++ stitchCrossSections track.config.id -1 False rowBack1 rowZero


buildStripBetweenSamples : GeneratedTrack -> Int -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildStripBetweenSamples track idx =
    case ( Array.get idx track.samples, Array.get (idx + 1) track.samples ) of
        ( Just s0, Just s1 ) ->
            let
                row0 =
                    sampleCrossSectionVertices track s0

                row1 =
                    sampleCrossSectionVertices track s1

                stepIsDash =
                    idx >= 2 && modBy 4 idx < 2
            in
            stitchCrossSections track.config.id idx stepIsDash row0 row1

        _ ->
            []


type alias CrossVertex =
    { pos : Vec3
    , normal : Vec3
    , dFrac : Float
    }


sampleCrossSectionVertices : GeneratedTrack -> TrackSample -> List CrossVertex
sampleCrossSectionVertices track smp =
    let
        hw =
            smp.halfWidth

        stations =
            [ -(hw + 38.0)
            , -(hw + 18.0)
            , -(hw + 7.5)
            , -(hw + 3.5)
            , -(hw + 0.45)
            , -hw
            , -0.11
            , 0.11
            , hw
            , hw + 0.45
            , hw + 3.5
            , hw + 7.5
            , hw + 18.0
            , hw + 38.0
            ]

        rightXZ =
            normalizeXZ smp.right

        evalStation d =
            let
                ptXZ =
                    Vec3.add smp.center (Vec3.scale d rightXZ)

                q =
                    Track.sampleAtHinted smp.s track (Vec3.getX ptXZ) (Vec3.getZ ptXZ)

                absD =
                    abs d

                adjustedHeight =
                    if absD >= hw - 0.02 && absD <= hw + 0.46 then
                        -- Raised LEGO curb plate (1 plate = 0.04m above roadbed)
                        q.height + 0.04

                    else if absD >= hw + 7.5 then
                        -- Discrete LEGO brick layer terracing (0.30m = 2 LEGO bricks step)
                        toFloat (round (q.height / 0.3)) * 0.3

                    else if absD > hw + 3.5 then
                        -- Smooth blend from gravel shoulder edge into terraced slopes
                        let
                            terraceY =
                                toFloat (round (q.height / 0.3)) * 0.3

                            t =
                                (absD - (hw + 3.5)) / 4.0
                        in
                        q.height * (1.0 - t) + terraceY * t

                    else
                        q.height
            in
            { pos = vec3 (Vec3.getX ptXZ) adjustedHeight (Vec3.getZ ptXZ)
            , normal = q.normal
            , dFrac = d
            }
    in
    List.map evalStation stations


stitchCrossSections :
    TrackId
    -> Int
    -> Bool
    -> List CrossVertex
    -> List CrossVertex
    -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
stitchCrossSections trackId segIdx dashOn r0 r1 =
    let
        pairs0 =
            adjacentPairs r0

        pairs1 =
            adjacentPairs r1

        stepBand bandIdx ( ( a0, b0 ), ( a1, b1 ) ) =
            let
                col =
                    bandColor trackId segIdx dashOn bandIdx
            in
            [ ( toTV a0 col, toTV b0 col, toTV b1 col )
            , ( toTV a0 col, toTV b1 col, toTV a1 col )
            ]
    in
    List.map2 Tuple.pair pairs0 pairs1
        |> List.indexedMap stepBand
        |> List.concat


bandColor : TrackId -> Int -> Bool -> Int -> Vec3
bandColor trackId segIdx dashOn bandIdx =
    let
        curbRed =
            ldrawColor 4

        curbWhite =
            ldrawColor 15

        curb =
            if modBy 2 segIdx == 0 then
                curbRed

            else
                curbWhite

        roadBase =
            vec3 0.24 0.26 0.29

        centerMark =
            if dashOn then
                ldrawColor 14

            else
                roadBase

        gravelCol =
            case trackId of
                QuarrySwitchbacks ->
                    vec3 0.58 0.5 0.41

                AlpineGlacier ->
                    vec3 0.62 0.68 0.75

                PineRidge ->
                    vec3 0.56 0.49 0.39

        grassInner =
            case trackId of
                PineRidge ->
                    ldrawColor 2

                QuarrySwitchbacks ->
                    ldrawColor 378

                AlpineGlacier ->
                    vec3 0.34 0.52 0.42

        cliffOuter =
            case trackId of
                PineRidge ->
                    ldrawColor 288

                QuarrySwitchbacks ->
                    ldrawColor 72

                AlpineGlacier ->
                    ldrawColor 15
    in
    case bandIdx of
        0 ->
            cliffOuter

        1 ->
            grassInner

        2 ->
            grassInner

        3 ->
            gravelCol

        4 ->
            curb

        5 ->
            roadBase

        6 ->
            centerMark

        7 ->
            roadBase

        8 ->
            curb

        9 ->
            gravelCol

        10 ->
            grassInner

        11 ->
            grassInner

        _ ->
            cliffOuter


buildCheckpointTris : GeneratedTrack -> Checkpoint -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildCheckpointTris track cp =
    let
        smp =
            Track.splineSampleAtS track cp.s
    in
    buildArchTris smp cp.isFinish False


buildArchTris : TrackSample -> Bool -> Bool -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildArchTris smp isFinish isStart =
    let
        rightXZ =
            normalizeXZ smp.right

        fwdXZ =
            normalizeXZ smp.tangent

        hw =
            smp.halfWidth + 0.3

        postHeight =
            4.8

        postCol =
            if isStart then
                ldrawColor 2

            else if isFinish then
                ldrawColor 0

            else
                ldrawColor 1

        bannerCol =
            if isStart then
                ldrawColor 14

            else if isFinish then
                ldrawColor 15

            else
                ldrawColor 191

        leftBase =
            Vec3.sub smp.center (Vec3.scale hw rightXZ)

        rightBase =
            Vec3.add smp.center (Vec3.scale hw rightXZ)

        leftPost =
            boxAt leftBase rightXZ fwdXZ 0.32 postHeight 0.32 postCol

        rightPost =
            boxAt rightBase rightXZ fwdXZ 0.32 postHeight 0.32 postCol

        beamCenter =
            Vec3.add smp.center (vec3 0 postHeight 0)

        crossBeam =
            boxAt beamCenter rightXZ fwdXZ (hw + 0.35) 0.75 0.28 bannerCol
    in
    leftPost ++ rightPost ++ crossBeam


buildObstacleTris : TrackId -> TrackObstacle -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildObstacleTris trackId obs =
    let
        base =
            obs.position
    in
    case obs.kind of
        PineTree ->
            buildLegoPineTree trackId base obs.radiusM obs.heightM

        Boulder ->
            buildLegoBoulder trackId base obs.radiusM obs.heightM

        ChicaneBlock ->
            buildLegoChicaneBlock trackId base obs.radiusM obs.heightM

        BarrierPost ->
            buildLegoBarrierPost trackId base obs.radiusM obs.heightM


buildLegoPineTree : TrackId -> Vec3 -> Float -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildLegoPineTree trackId base rad ht =
    let
        trunkCol =
            ldrawColor 70

        ( foliageBaseCol, foliageTopCol, snowCap ) =
            case trackId of
                AlpineGlacier ->
                    ( ldrawColor 288, ldrawColor 288, True )

                QuarrySwitchbacks ->
                    ( ldrawColor 378, ldrawColor 288, False )

                PineRidge ->
                    ( ldrawColor 288, ldrawColor 2, False )

        trunkRadius =
            max 0.12 (rad * 0.22)

        trunkHeight =
            ht * 0.28

        trunk =
            cylinderAt base trunkRadius trunkHeight 8 trunkCol

        tier0 =
            pineTierAt (Vec3.add base (vec3 0 (ht * 0.22) 0)) (rad * 1.05) (ht * 0.28) 0.0 foliageBaseCol

        tier1 =
            pineTierAt (Vec3.add base (vec3 0 (ht * 0.44) 0)) (rad * 0.82) (ht * 0.26) 0.39 foliageBaseCol

        tier2 =
            pineTierAt (Vec3.add base (vec3 0 (ht * 0.64) 0)) (rad * 0.58) (ht * 0.24) 0.78 foliageTopCol

        tier3 =
            let
                tCol =
                    if snowCap then
                        ldrawColor 15

                    else
                        foliageTopCol
            in
            pineTierAt (Vec3.add base (vec3 0 (ht * 0.8) 0)) (rad * 0.36) (ht * 0.2) 1.18 tCol

        apexY =
            ht * 0.96

        apexPos =
            Vec3.add base (vec3 0 apexY 0)

        apexCol =
            if snowCap then
                ldrawColor 15

            else
                foliageTopCol

        topStud =
            cylinderAt apexPos 0.045 0.035 8 apexCol
    in
    trunk ++ tier0 ++ tier1 ++ tier2 ++ tier3 ++ topStud


pineTierAt : Vec3 -> Float -> Float -> Float -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
pineTierAt tierBase rOut hTier rotAngle col =
    let
        numBoughs =
            8

        rIn =
            rOut * 0.55

        apex =
            Vec3.add tierBase (vec3 0 hTier 0)

        undersideCenter =
            tierBase

        bough i =
            let
                aMid =
                    rotAngle + 2.0 * pi * toFloat i / toFloat numBoughs

                aLeft =
                    aMid - pi / toFloat numBoughs

                aRight =
                    aMid + pi / toFloat numBoughs

                tip =
                    Vec3.add tierBase (vec3 (rOut * cos aMid) 0 (rOut * sin aMid))

                leftInner =
                    Vec3.add tierBase (vec3 (rIn * cos aLeft) 0 (rIn * sin aLeft))

                rightInner =
                    Vec3.add tierBase (vec3 (rIn * cos aRight) 0 (rIn * sin aRight))

                face1 =
                    triWithComputedNormal leftInner tip apex col

                face2 =
                    triWithComputedNormal tip rightInner apex col

                under1 =
                    triWithComputedNormal undersideCenter tip leftInner col

                under2 =
                    triWithComputedNormal undersideCenter rightInner tip col
            in
            [ face1, face2, under1, under2 ]
    in
    List.concatMap bough (List.range 0 (numBoughs - 1))


buildLegoBoulder : TrackId -> Vec3 -> Float -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildLegoBoulder trackId base rad ht =
    let
        ( darkRock, lightRock, snowCap ) =
            case trackId of
                AlpineGlacier ->
                    ( ldrawColor 72, vec3 0.58 0.64 0.72, True )

                QuarrySwitchbacks ->
                    ( ldrawColor 70, ldrawColor 72, False )

                PineRidge ->
                    ( ldrawColor 72, ldrawColor 71, False )

        y1 =
            ht * 0.42

        y2 =
            ht * 0.76

        y3 =
            ht

        tier0 =
            facetedRockTier base 0.0 y1 rad (rad * 0.85) 6 darkRock

        tier1 =
            facetedRockTier (Vec3.add base (vec3 0 y1 0)) 0.0 (y2 - y1) (rad * 0.85) (rad * 0.55) 6 lightRock

        topCol =
            if snowCap then
                ldrawColor 15

            else
                lightRock

        tier2 =
            facetedRockTier (Vec3.add base (vec3 0 y2 0)) 0.0 (y3 - y2) (rad * 0.55) (rad * 0.28) 6 topCol

        stud0 =
            cylinderAt (Vec3.add base (vec3 (rad * 0.15) y2 (rad * 0.12))) 0.045 0.03 6 topCol

        stud1 =
            cylinderAt (Vec3.add base (vec3 -(rad * 0.12) y2 -(rad * 0.1))) 0.045 0.03 6 topCol

        studTop =
            cylinderAt (Vec3.add base (vec3 0 y3 0)) 0.045 0.03 6 topCol
    in
    tier0 ++ tier1 ++ tier2 ++ stud0 ++ stud1 ++ studTop


facetedRockTier : Vec3 -> Float -> Float -> Float -> Float -> Int -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
facetedRockTier basePt yLow yHigh rBot rTop sides col =
    let
        step i =
            let
                a0 =
                    2.0 * pi * toFloat i / toFloat sides

                a1 =
                    2.0 * pi * toFloat (i + 1) / toFloat sides

                p0Bot =
                    Vec3.add basePt (vec3 (rBot * cos a0) yLow (rBot * sin a0))

                p1Bot =
                    Vec3.add basePt (vec3 (rBot * cos a1) yLow (rBot * sin a1))

                p0Top =
                    Vec3.add basePt (vec3 (rTop * cos a0) yHigh (rTop * sin a0))

                p1Top =
                    Vec3.add basePt (vec3 (rTop * cos a1) yHigh (rTop * sin a1))

                cTop =
                    Vec3.add basePt (vec3 0 yHigh 0)
            in
            [ triWithComputedNormal p0Bot p1Bot p1Top col
            , triWithComputedNormal p0Bot p1Top p0Top col
            , triWithComputedNormal cTop p0Top p1Top col
            ]
    in
    List.concatMap step (List.range 0 (sides - 1))


buildLegoChicaneBlock : TrackId -> Vec3 -> Float -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildLegoChicaneBlock trackId base rad ht =
    let
        xAxis =
            vec3 1 0 0

        zAxis =
            vec3 0 0 1

        halfW =
            rad

        halfD =
            max 0.2 (rad * 0.55)

        numCourses =
            3

        courseH =
            ht / toFloat numCourses

        col1 =
            case trackId of
                AlpineGlacier ->
                    ldrawColor 1

                _ ->
                    ldrawColor 4

        col2 =
            ldrawColor 15

        buildCourse i =
            let
                cCol =
                    if modBy 2 i == 0 then
                        col1

                    else
                        col2

                yOffset =
                    toFloat i * courseH

                courseBase =
                    Vec3.add base (vec3 0 yOffset 0)
            in
            boxAt courseBase xAxis zAxis halfW courseH halfD cCol

        courses =
            List.concatMap buildCourse (List.range 0 (numCourses - 1))

        studY =
            ht

        studSpacing =
            0.125

        studCols =
            max 2 (floor (halfW / studSpacing))

        studRow =
            List.range -studCols studCols
                |> List.concatMap
                    (\c ->
                        let
                            xPos =
                                toFloat c * studSpacing

                            sPos1 =
                                Vec3.add base (vec3 xPos studY (halfD * 0.45))

                            sPos2 =
                                Vec3.add base (vec3 xPos studY -(halfD * 0.45))

                            topCol =
                                if modBy 2 (numCourses - 1) == 0 then
                                    col1

                                else
                                    col2
                        in
                        cylinderAt sPos1 0.038 0.025 6 topCol
                            ++ cylinderAt sPos2 0.038 0.025 6 topCol
                    )
    in
    courses ++ studRow


buildLegoBarrierPost : TrackId -> Vec3 -> Float -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildLegoBarrierPost _ base rad ht =
    let
        col1 =
            ldrawColor 191

        col2 =
            ldrawColor 15

        r =
            max 0.14 (rad * 0.45)

        numBands =
            4

        bandH =
            ht / toFloat numBands

        buildBand i =
            let
                bCol =
                    if modBy 2 i == 0 then
                        col1

                    else
                        col2

                y =
                    toFloat i * bandH

                bandBase =
                    Vec3.add base (vec3 0 y 0)
            in
            cylinderAt bandBase r bandH 8 bCol

        bands =
            List.concatMap buildBand (List.range 0 (numBands - 1))

        basePlate =
            cylinderAt base (r * 1.5) (ht * 0.08) 8 (ldrawColor 72)

        topStud =
            cylinderAt (Vec3.add base (vec3 0 ht 0)) (r * 0.6) 0.035 8 col1
    in
    basePlate ++ bands ++ topStud


cylinderAt : Vec3 -> Float -> Float -> Int -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
cylinderAt baseCenter rad height segments color =
    let
        step i =
            let
                a0 =
                    2.0 * pi * toFloat i / toFloat segments

                a1 =
                    2.0 * pi * toFloat (i + 1) / toFloat segments

                ( c0, s0 ) =
                    ( cos a0, sin a0 )

                ( c1, s1 ) =
                    ( cos a1, sin a1 )

                b0 =
                    Vec3.add baseCenter (vec3 (rad * c0) 0 (rad * s0))

                b1 =
                    Vec3.add baseCenter (vec3 (rad * c1) 0 (rad * s1))

                t0 =
                    Vec3.add baseCenter (vec3 (rad * c0) height (rad * s0))

                t1 =
                    Vec3.add baseCenter (vec3 (rad * c1) height (rad * s1))

                topCenter =
                    Vec3.add baseCenter (vec3 0 height 0)

                sideNorm =
                    normalizeSafe (vec3 (c0 + c1) 0 (s0 + s1)) (vec3 0 1 0)

                upNorm =
                    vec3 0 1 0
            in
            [ ( { position = b0, normal = sideNorm, color = color }
              , { position = b1, normal = sideNorm, color = color }
              , { position = t1, normal = sideNorm, color = color }
              )
            , ( { position = b0, normal = sideNorm, color = color }
              , { position = t1, normal = sideNorm, color = color }
              , { position = t0, normal = sideNorm, color = color }
              )
            , ( { position = topCenter, normal = upNorm, color = color }
              , { position = t0, normal = upNorm, color = color }
              , { position = t1, normal = upNorm, color = color }
              )
            ]
    in
    List.concatMap step (List.range 0 (segments - 1))


boxAt : Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Float -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
boxAt baseCenter xAxis zAxis halfW height halfD color =
    let
        pt sx sy sz =
            baseCenter
                |> Vec3.add (Vec3.scale (sx * halfW) xAxis)
                |> Vec3.add (vec3 0 (sy * height) 0)
                |> Vec3.add (Vec3.scale (sz * halfD) zAxis)

        p000 =
            pt -1 0 -1

        p100 =
            pt 1 0 -1

        p110 =
            pt 1 1 -1

        p010 =
            pt -1 1 -1

        p001 =
            pt -1 0 1

        p101 =
            pt 1 0 1

        p111 =
            pt 1 1 1

        p011 =
            pt -1 1 1

        nTop =
            vec3 0 1 0

        nFront =
            zAxis

        nBack =
            Vec3.negate zAxis

        nRight =
            xAxis

        nLeft =
            Vec3.negate xAxis
    in
    quad p001 p101 p111 p011 nFront color
        ++ quad p100 p000 p010 p110 nBack color
        ++ quad p101 p100 p110 p111 nRight color
        ++ quad p000 p001 p011 p010 nLeft color
        ++ quad p011 p111 p110 p010 nTop color


toTV : CrossVertex -> Vec3 -> TerrainVertex
toTV cv col =
    { position = cv.pos
    , normal = cv.normal
    , color = col
    }


quad : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
quad p0 p1 p2 p3 n col =
    [ tri p0 p1 p2 n col
    , tri p0 p2 p3 n col
    ]


tri : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( TerrainVertex, TerrainVertex, TerrainVertex )
tri a b c n col =
    ( { position = a, normal = n, color = col }
    , { position = b, normal = n, color = col }
    , { position = c, normal = n, color = col }
    )


triWithComputedNormal : Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( TerrainVertex, TerrainVertex, TerrainVertex )
triWithComputedNormal a b c col =
    let
        ab =
            Vec3.sub b a

        ac =
            Vec3.sub c a

        n =
            normalizeSafe (Vec3.cross ab ac) (vec3 0 1 0)
    in
    tri a b c n col


adjacentPairs : List a -> List ( a, a )
adjacentPairs xs =
    case xs of
        a :: b :: rest ->
            ( a, b ) :: adjacentPairs (b :: rest)

        _ ->
            []


normalizeXZ : Vec3 -> Vec3
normalizeXZ v =
    normalizeSafe (vec3 (Vec3.getX v) 0 (Vec3.getZ v)) (vec3 0 0 1)


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
