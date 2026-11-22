module Racer.TerrainMesh exposing
    ( TrackChunk
    , TrackRenderBundle
    , buildGaragePlinthMesh
    , buildTrackRenderBundle
    , rebuildChunkScenery
    , sceneryDebrisStyle
    )

{-| Chunked WebGL countryside ground (unpaved road, verges and fields), launch
humps, checkpoint/finish arches, brick-built roadside
scenery (trees, bushes, fences, rocks, signposts, hay bales, cottages, barns,
crates, barrels, garden gnomes, brick walls), and the garage display plinth mesh.

Each chunk keeps its scenery in a separate mesh so smashed pieces can be removed
by rebuilding only that small mesh.

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
        , SurfaceType(..)
        , TrackId(..)
        , TrackObstacle
        , TrackSample
        )
import WebGL


{-| One spatial `100 m` slice of ground and arches (`mesh`) plus the brick-built
scenery still standing in it (`sceneryMesh`).
-}
type alias TrackChunk =
    { chunkIndex : Int
    , minS : Float
    , maxS : Float
    , boundsCenter : Vec3
    , boundsRadius : Float
    , mesh : WebGL.Mesh TerrainVertex
    , sceneryMesh : WebGL.Mesh TerrainVertex
    }


{-| Pre-built chunk array for O(1) visible-window frustum culling during gameplay.
-}
type alias TrackRenderBundle =
    { trackId : TrackId
    , seed : Int
    , chunkSizeM : Float
    , chunks : Array TrackChunk
    }


{-| Resolve an LDraw color code to an RGB vector.
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


{-| Circular display plinth with red/white studded curb blocks for Garage inspection.
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

        centerSample =
            Track.splineSampleAtS track ((minS + maxS) * 0.5)
    in
    { chunkIndex = chunkIdx
    , minS = minS
    , maxS = maxS
    , boundsCenter = centerSample.center
    , boundsRadius = chunkSizeM * 0.85 + 52.0
    , mesh = WebGL.triangles (ribbonTris ++ gateTris)
    , sceneryMesh = buildSceneryMesh (sceneryInRange minS maxS track.obstacles)
    }


sceneryInRange : Float -> Float -> List TrackObstacle -> List TrackObstacle
sceneryInRange minS maxS obstacles =
    List.filter (\o -> o.s >= minS && o.s < maxS) obstacles


{-| Rebuild only the scenery mesh of the chunk that holds arc length `s`, after
pieces there were smashed and removed from `track.obstacles`. The ground mesh is
left untouched, so this stays cheap enough to run mid-race.
-}
rebuildChunkScenery : GeneratedTrack -> Float -> TrackRenderBundle -> TrackRenderBundle
rebuildChunkScenery track s bundle =
    let
        idx =
            clamp 0 (Array.length bundle.chunks - 1) (floor (s / bundle.chunkSizeM))
    in
    case Array.get idx bundle.chunks of
        Just chunk ->
            { bundle
                | chunks =
                    Array.set idx
                        { chunk | sceneryMesh = buildSceneryMesh (sceneryInRange chunk.minS chunk.maxS track.obstacles) }
                        bundle.chunks
            }

        Nothing ->
            bundle


buildSceneryMesh : List TrackObstacle -> WebGL.Mesh TerrainVertex
buildSceneryMesh obstacles =
    WebGL.triangles (List.concatMap buildSceneryTris obstacles)


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
            stitchCrossSections track.config.id s0.surface -2 rowBack2 rowBack1
                ++ stitchCrossSections track.config.id s0.surface -1 rowBack1 rowZero


buildStripBetweenSamples : GeneratedTrack -> Int -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildStripBetweenSamples track idx =
    case ( Array.get idx track.samples, Array.get (idx + 1) track.samples ) of
        ( Just s0, Just s1 ) ->
            let
                row0 =
                    sampleCrossSectionVertices track s0

                row1 =
                    sampleCrossSectionVertices track s1
            in
            stitchCrossSections track.config.id s0.surface idx row0 row1

        _ ->
            []


type alias CrossVertex =
    { pos : Vec3
    , normal : Vec3
    , dFrac : Float
    }


{-| Cross-section stations from the far field on the left to the far field on the
right: field, field, field, verge, road edge, wheel track, center strip, wheel
track, road edge, verge, field, field, field. The ground follows the smooth
heightfield (no terracing, no raised curbs): it is an unpaved country road.
-}
sampleCrossSectionVertices : GeneratedTrack -> TrackSample -> List CrossVertex
sampleCrossSectionVertices track smp =
    let
        hw =
            smp.halfWidth

        strip =
            min 0.45 (hw * 0.12)

        stations =
            [ -(hw + 38.0)
            , -(hw + 18.0)
            , -(hw + 7.5)
            , -(hw + 2.5)
            , -(hw + 0.6)
            , -(hw - 0.5)
            , -strip
            , strip
            , hw - 0.5
            , hw + 0.6
            , hw + 2.5
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
            in
            { pos = vec3 (Vec3.getX ptXZ) q.height (Vec3.getZ ptXZ)
            , normal = q.normal
            , dFrac = d
            }
    in
    List.map evalStation stations


stitchCrossSections :
    TrackId
    -> SurfaceType
    -> Int
    -> List CrossVertex
    -> List CrossVertex
    -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
stitchCrossSections trackId surface segIdx r0 r1 =
    let
        pairs0 =
            adjacentPairs r0

        pairs1 =
            adjacentPairs r1

        stepBand bandIdx ( ( a0, b0 ), ( a1, b1 ) ) =
            let
                col =
                    bandColor trackId surface segIdx bandIdx
            in
            [ ( toTV a0 col, toTV b0 col, toTV b1 col )
            , ( toTV a0 col, toTV b1 col, toTV a1 col )
            ]
    in
    List.map2 Tuple.pair pairs0 pairs1
        |> List.indexedMap stepBand
        |> List.concat


{-| Cheap deterministic per-segment shade jitter in [-1, 1] for patchy unpaved ground.
-}
patchNoise : Int -> Int -> Float
patchNoise segIdx bandIdx =
    let
        h =
            modBy 997 (segIdx * 131 + bandIdx * 71 + (segIdx // 3) * 17)
    in
    toFloat h / 498.5 - 1.0


roadColors : SurfaceType -> { road : Vec3, edge : Vec3, center : Vec3 }
roadColors surface =
    case surface of
        DirtRoad ->
            { road = vec3 0.56 0.41 0.26, edge = vec3 0.5 0.44 0.27, center = vec3 0.42 0.54 0.25 }

        GravelRoad ->
            { road = vec3 0.67 0.63 0.55, edge = vec3 0.6 0.58 0.46, center = vec3 0.58 0.57 0.45 }

        ForestTrack ->
            { road = vec3 0.42 0.31 0.2, edge = vec3 0.36 0.33 0.2, center = vec3 0.38 0.34 0.2 }

        GrassPath ->
            { road = vec3 0.7 0.63 0.42, edge = vec3 0.56 0.6 0.34, center = vec3 0.5 0.62 0.3 }

        RoughVerge ->
            { road = vec3 0.46 0.56 0.28, edge = vec3 0.46 0.56 0.28, center = vec3 0.46 0.56 0.28 }

        MeadowGrass ->
            { road = vec3 0.42 0.62 0.26, edge = vec3 0.42 0.62 0.26, center = vec3 0.42 0.62 0.26 }


fieldColors : TrackId -> Int -> { verge : Vec3, near : Vec3, far : Vec3 }
fieldColors trackId segIdx =
    case trackId of
        FarmLane ->
            { verge = vec3 0.5 0.6 0.28
            , near = vec3 0.42 0.62 0.26
            , far =
                -- Alternating crop strips: ripening grain and pasture
                if modBy 10 (segIdx + 1000) < 5 then
                    vec3 0.78 0.66 0.32

                else
                    vec3 0.46 0.64 0.27
            }

        ForestTrail ->
            { verge = vec3 0.36 0.46 0.22
            , near = vec3 0.27 0.43 0.2
            , far = vec3 0.2 0.34 0.17
            }

        MeadowPath ->
            { verge = vec3 0.55 0.66 0.3
            , near = vec3 0.48 0.68 0.28
            , far = vec3 0.6 0.7 0.33
            }


bandColor : TrackId -> SurfaceType -> Int -> Int -> Vec3
bandColor trackId surface segIdx bandIdx =
    let
        road =
            roadColors surface

        field =
            fieldColors trackId segIdx

        base =
            case bandIdx of
                0 ->
                    field.far

                1 ->
                    field.near

                2 ->
                    field.near

                3 ->
                    field.verge

                4 ->
                    road.edge

                5 ->
                    road.road

                6 ->
                    road.center

                7 ->
                    road.road

                8 ->
                    road.edge

                9 ->
                    field.verge

                10 ->
                    field.near

                11 ->
                    field.near

                _ ->
                    field.far

        jitter =
            1.0 + 0.07 * patchNoise segIdx bandIdx
    in
    Vec3.scale jitter base


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



-- ── Brick-built scenery ──────────────────────────────────────────────────────
--
-- Every scenery piece is assembled from axis-aligned bricks (in the piece's own
-- yaw frame) with round studs on top, so it reads as a brick build and so the
-- same bricks can fly apart as debris when the racer smashes into it.


type alias Frame =
    { origin : Vec3
    , right : Vec3
    , fwd : Vec3
    }


frameFor : TrackObstacle -> Frame
frameFor obs =
    { origin = obs.position
    , right = vec3 (cos obs.yaw) 0 -(sin obs.yaw)
    , fwd = vec3 (sin obs.yaw) 0 (cos obs.yaw)
    }


localPt : Frame -> Float -> Float -> Float -> Vec3
localPt fr lx ly lz =
    fr.origin
        |> Vec3.add (Vec3.scale lx fr.right)
        |> Vec3.add (vec3 0 ly 0)
        |> Vec3.add (Vec3.scale lz fr.fwd)


{-| One brick: centered at local (lx, lz), bottom at ly, half-extents (hx, hz), height h.
-}
brick : Frame -> ( Float, Float, Float ) -> ( Float, Float, Float ) -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
brick fr ( lx, ly, lz ) ( hx, h, hz ) col =
    boxAt (localPt fr lx ly lz) fr.right fr.fwd hx h hz col


{-| A grid of `nx` × `nz` studs centered on local (lx, lz) at height ly.
-}
studs : Frame -> ( Float, Float, Float ) -> ( Int, Int ) -> Float -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
studs fr ( lx, ly, lz ) ( nx, nz ) spacing col =
    List.range 0 (nx - 1)
        |> List.concatMap
            (\i ->
                List.range 0 (nz - 1)
                    |> List.concatMap
                        (\k ->
                            let
                                ox =
                                    (toFloat i - toFloat (nx - 1) * 0.5) * spacing

                                oz =
                                    (toFloat k - toFloat (nz - 1) * 0.5) * spacing
                            in
                            cylinderAt (localPt fr (lx + ox) ly (lz + oz)) (spacing * 0.28) (spacing * 0.18) 6 col
                        )
            )


{-| How a smashed scenery piece breaks up: the brick colors it is built from,
how many loose bricks fly off (kept small for mobile), and their size scale.
-}
sceneryDebrisStyle : ObstacleKind -> { colors : List Vec3, count : Int, scale : Float }
sceneryDebrisStyle kind =
    case kind of
        LeafyTree ->
            { colors = [ ldrawColor 2, ldrawColor 10, ldrawColor 2, ldrawColor 70 ], count = 10, scale = 1.0 }

        PineTree ->
            { colors = [ ldrawColor 288, ldrawColor 288, ldrawColor 70 ], count = 9, scale = 1.0 }

        Bush ->
            { colors = [ ldrawColor 2, ldrawColor 10, ldrawColor 14 ], count = 6, scale = 0.75 }

        FenceSection ->
            { colors = [ ldrawColor 15, ldrawColor 15, ldrawColor 70 ], count = 6, scale = 0.8 }

        Rock ->
            { colors = [ ldrawColor 71, ldrawColor 72 ], count = 7, scale = 1.0 }

        Signpost ->
            { colors = [ ldrawColor 14, ldrawColor 70, ldrawColor 4 ], count = 4, scale = 0.7 }

        HayBale ->
            { colors = [ ldrawColor 19, ldrawColor 19, ldrawColor 28 ], count = 6, scale = 1.0 }

        Cottage ->
            { colors = [ ldrawColor 15, ldrawColor 15, ldrawColor 4, ldrawColor 4, ldrawColor 1 ], count = 14, scale = 1.3 }

        Barn ->
            { colors = [ ldrawColor 320, ldrawColor 320, ldrawColor 72, ldrawColor 15 ], count = 14, scale = 1.3 }

        Crate ->
            { colors = [ ldrawColor 70, ldrawColor 28, ldrawColor 70 ], count = 7, scale = 0.85 }

        Barrel ->
            { colors = [ ldrawColor 1, ldrawColor 1, ldrawColor 71 ], count = 6, scale = 0.8 }

        GardenGnome ->
            { colors = [ ldrawColor 4, ldrawColor 1, ldrawColor 15, ldrawColor 78 ], count = 4, scale = 0.5 }

        BrickWall ->
            { colors = [ ldrawColor 4, ldrawColor 320, ldrawColor 4, ldrawColor 71 ], count = 12, scale = 0.9 }


buildSceneryTris : TrackObstacle -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildSceneryTris obs =
    let
        fr =
            frameFor obs
    in
    case obs.kind of
        LeafyTree ->
            leafyTreeTris fr obs.heightM

        PineTree ->
            pineTreeTris fr obs.heightM

        Bush ->
            bushTris fr

        FenceSection ->
            fenceTris fr

        Rock ->
            rockTris fr obs.radiusM obs.heightM

        Signpost ->
            signpostTris fr obs.heightM

        HayBale ->
            hayBaleTris fr obs.heightM

        Cottage ->
            cottageTris fr

        Barn ->
            barnTris fr

        Crate ->
            crateTris fr obs.heightM

        Barrel ->
            barrelTris fr

        GardenGnome ->
            gnomeTris fr

        BrickWall ->
            brickWallTris fr


leafyTreeTris : Frame -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
leafyTreeTris fr ht =
    let
        k =
            ht / 4.5

        trunk =
            brick fr ( 0, 0, 0 ) ( 0.2 * k, 1.9 * k, 0.2 * k ) (ldrawColor 70)

        leafDark =
            ldrawColor 2

        leafLight =
            ldrawColor 10
    in
    trunk
        ++ brick fr ( 0, 1.6 * k, 0 ) ( 1.1 * k, 0.96 * k, 1.1 * k ) leafDark
        ++ brick fr ( 0.1 * k, 2.56 * k, -0.1 * k ) ( 0.95 * k, 0.96 * k, 0.95 * k ) leafLight
        ++ brick fr ( 0, 3.52 * k, 0 ) ( 0.55 * k, 0.6 * k, 0.55 * k ) leafDark
        ++ studs fr ( 0, 4.12 * k, 0 ) ( 2, 2 ) (0.5 * k) leafDark
        ++ studs fr ( 0.1 * k, 3.52 * k, -0.1 * k ) ( 1, 1 ) (0.5 * k) (ldrawColor 4)


pineTreeTris : Frame -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
pineTreeTris fr ht =
    let
        k =
            ht / 6.0

        needles =
            ldrawColor 288

        tier i =
            let
                half =
                    (1.25 - toFloat i * 0.28) * k

                y =
                    (1.2 + toFloat i * 1.05) * k
            in
            brick fr ( 0, y, 0 ) ( half, 1.05 * k, half ) needles
    in
    brick fr ( 0, 0, 0 ) ( 0.18 * k, 1.4 * k, 0.18 * k ) (ldrawColor 70)
        ++ List.concatMap tier (List.range 0 3)
        ++ brick fr ( 0, 5.4 * k, 0 ) ( 0.18 * k, 0.55 * k, 0.18 * k ) needles
        ++ studs fr ( 0, 5.95 * k, 0 ) ( 1, 1 ) (0.5 * k) needles


bushTris : Frame -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
bushTris fr =
    brick fr ( 0, 0, 0 ) ( 0.75, 0.5, 0.55 ) (ldrawColor 2)
        ++ brick fr ( 0.15, 0.5, 0.05 ) ( 0.5, 0.36, 0.4 ) (ldrawColor 10)
        ++ studs fr ( 0.15, 0.86, 0.05 ) ( 2, 1 ) 0.44 (ldrawColor 10)
        ++ studs fr ( -0.5, 0.5, 0.3 ) ( 1, 1 ) 0.4 (ldrawColor 14)


fenceTris : Frame -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
fenceTris fr =
    let
        post lz =
            brick fr ( 0, 0, lz ) ( 0.09, 1.05, 0.09 ) (ldrawColor 70)
                ++ studs fr ( 0, 1.05, lz ) ( 1, 1 ) 0.3 (ldrawColor 70)

        rail ly =
            brick fr ( 0, ly, 0 ) ( 0.05, 0.16, 1.5 ) (ldrawColor 15)
    in
    post -1.4 ++ post 1.4 ++ rail 0.35 ++ rail 0.75


rockTris : Frame -> Float -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
rockTris fr rad ht =
    let
        lower =
            ht * 0.55
    in
    brick fr ( 0, 0, 0 ) ( rad, lower, rad * 0.8 ) (ldrawColor 72)
        ++ brick fr ( rad * 0.15, lower, -(rad * 0.1) ) ( rad * 0.68, ht - lower, rad * 0.56 ) (ldrawColor 71)
        ++ studs fr ( rad * 0.15, ht, -(rad * 0.1) ) ( 2, 1 ) (rad * 0.55) (ldrawColor 71)


signpostTris : Frame -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
signpostTris fr ht =
    brick fr ( 0, 0, 0 ) ( 0.08, ht, 0.08 ) (ldrawColor 70)
        ++ brick fr ( 0, ht - 0.62, 0.1 ) ( 0.55, 0.42, 0.05 ) (ldrawColor 14)
        ++ brick fr ( 0, ht - 0.5, 0.16 ) ( 0.38, 0.16, 0.02 ) (ldrawColor 4)
        ++ studs fr ( 0, ht, 0 ) ( 1, 1 ) 0.3 (ldrawColor 70)


hayBaleTris : Frame -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
hayBaleTris fr ht =
    let
        hay =
            ldrawColor 19

        twine =
            ldrawColor 28

        bale ( lx, ly, lz ) =
            brick fr ( lx, ly, lz ) ( 0.7, 0.8, 0.42 ) hay
                ++ brick fr ( lx, ly + 0.2, lz ) ( 0.71, 0.08, 0.43 ) twine
                ++ brick fr ( lx, ly + 0.52, lz ) ( 0.71, 0.08, 0.43 ) twine
                ++ studs fr ( lx, ly + 0.8, lz ) ( 2, 1 ) 0.55 hay
    in
    if ht > 1.2 then
        bale ( 0, 0, -0.44 ) ++ bale ( 0, 0, 0.44 ) ++ bale ( 0.1, 0.8, 0 )

    else
        bale ( 0, 0, 0 )


cottageTris : Frame -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
cottageTris fr =
    let
        wall =
            ldrawColor 15

        roof =
            ldrawColor 4

        roofStep i =
            brick fr ( 0, 2.1 + toFloat i * 0.4, 0 ) ( 2.4, 0.4, 1.9 - toFloat i * 0.62 ) roof
    in
    brick fr ( 0, 0, 0 ) ( 2.0, 2.1, 1.5 ) wall
        ++ brick fr ( -0.9, 0, 1.52 ) ( 0.4, 1.3, 0.04 ) (ldrawColor 70)
        ++ brick fr ( 0.8, 0.9, 1.52 ) ( 0.45, 0.6, 0.04 ) (ldrawColor 1)
        ++ List.concatMap roofStep (List.range 0 2)
        ++ brick fr ( 1.2, 2.5, -0.5 ) ( 0.25, 1.6, 0.25 ) (ldrawColor 72)
        ++ studs fr ( 0, 3.3, 0 ) ( 4, 1 ) 1.0 roof


barnTris : Frame -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
barnTris fr =
    let
        wall =
            ldrawColor 320

        roof =
            ldrawColor 72

        roofStep i =
            brick fr ( 0, 2.8 + toFloat i * 0.45, 0 ) ( 2.9, 0.45, 2.3 - toFloat i * 0.72 ) roof
    in
    brick fr ( 0, 0, 0 ) ( 2.6, 2.8, 2.0 ) wall
        ++ brick fr ( 0, 0, 2.02 ) ( 1.0, 2.0, 0.04 ) (ldrawColor 15)
        ++ brick fr ( 0, 0, 2.06 ) ( 0.8, 1.84, 0.04 ) wall
        ++ List.concatMap roofStep (List.range 0 2)
        ++ studs fr ( 0, 4.15, 0 ) ( 5, 1 ) 1.0 roof


{-| A wooden crate (reddish brown 2 x 2 bricks framed in dark tan), or a stack of
two with a third beside them.
-}
crateTris : Frame -> Float -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
crateTris fr ht =
    let
        wood =
            ldrawColor 70

        frame =
            ldrawColor 28

        crate ( lx, ly, lz ) =
            brick fr ( lx, ly, lz ) ( 0.42, 0.84, 0.42 ) wood
                ++ brick fr ( lx, ly, lz ) ( 0.45, 0.1, 0.45 ) frame
                ++ brick fr ( lx, ly + 0.74, lz ) ( 0.45, 0.1, 0.45 ) frame
                ++ studs fr ( lx, ly + 0.84, lz ) ( 2, 2 ) 0.42 wood
    in
    if ht > 1.2 then
        crate ( -0.2, 0, 0 ) ++ crate ( -0.2, 0.86, 0 ) ++ crate ( 0.5, 0, 0.25 )

    else
        crate ( 0, 0, 0 )


{-| Two blue oil drums (round bricks with grey rims) and a third lying on top.
-}
barrelTris : Frame -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
barrelTris fr =
    let
        drum =
            ldrawColor 1

        rim =
            ldrawColor 71

        barrel lx lz =
            let
                base =
                    localPt fr lx 0 lz
            in
            cylinderAt base 0.32 0.95 8 drum
                ++ cylinderAt (Vec3.add base (vec3 0 0.3 0)) 0.335 0.06 8 rim
                ++ cylinderAt (Vec3.add base (vec3 0 0.65 0)) 0.335 0.06 8 rim
                ++ cylinderAt (Vec3.add base (vec3 0.12 0.95 0)) 0.08 0.04 6 rim
    in
    barrel -0.3 0.0 ++ barrel 0.32 -0.12 ++ studs fr ( 0, 0.95, 0.2 ) ( 1, 1 ) 0.5 drum


{-| A garden gnome: black boots, blue trousers, red coat, white beard, light
nougat face and a tall red hat of stacked bricks. It faces the road.
-}
gnomeTris : Frame -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
gnomeTris fr =
    let
        red =
            ldrawColor 4
    in
    brick fr ( 0, 0, 0 ) ( 0.16, 0.08, 0.12 ) (ldrawColor 0)
        ++ brick fr ( 0, 0.08, 0 ) ( 0.14, 0.14, 0.1 ) (ldrawColor 1)
        ++ brick fr ( 0, 0.22, 0 ) ( 0.16, 0.18, 0.12 ) red
        ++ brick fr ( 0, 0.4, 0 ) ( 0.11, 0.12, 0.09 ) (ldrawColor 78)
        ++ brick fr ( 0, 0.3, 0.1 ) ( 0.1, 0.16, 0.03 ) (ldrawColor 15)
        ++ brick fr ( 0, 0.52, 0 ) ( 0.13, 0.08, 0.11 ) red
        ++ brick fr ( 0, 0.6, 0 ) ( 0.08, 0.1, 0.07 ) red
        ++ brick fr ( 0, 0.7, -0.02 ) ( 0.04, 0.08, 0.04 ) red


{-| A section of brick garden wall along the road: four courses of red and
dark red bricks in running bond, capped with grey studded plates.
-}
brickWallTris : Frame -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
brickWallTris fr =
    let
        course i =
            let
                ly =
                    toFloat i * 0.24

                shift =
                    if modBy 2 i == 0 then
                        0.0

                    else
                        0.36

                brickAt j =
                    let
                        lz =
                            -1.08 + shift + toFloat j * 0.72

                        col =
                            if modBy 2 (i + j) == 0 then
                                ldrawColor 4

                            else
                                ldrawColor 320
                    in
                    if lz > 1.1 then
                        []

                    else
                        brick fr ( 0, ly, lz ) ( 0.2, 0.235, 0.35 ) col
            in
            List.concatMap brickAt (List.range 0 3)
    in
    List.concatMap course (List.range 0 3)
        ++ brick fr ( 0, 0.96, 0 ) ( 0.24, 0.1, 1.46 ) (ldrawColor 71)
        ++ studs fr ( 0, 1.06, 0 ) ( 1, 4 ) 0.72 (ldrawColor 71)


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
