module Racer.TerrainMesh exposing
    ( TrackChunk
    , TrackRenderBundle
    , buildGaragePlinthMesh
    , buildTrackRenderBundle
    , rebuildChunkScenery
    , sceneryDebrisStyle
    )

{-| Chunked WebGL countryside ground (unpaved road, verges and fields, launch
humps) in the brick look of LEGO plates and tiles, the brick launch ramps on
it, and the garage display plinth mesh.

Each chunk also lists the brick-built roadside scenery still standing in it
and the arches it holds; `Racer.Scenery` draws those from catalog models, so
removing a smashed piece only filters a short list.

-}

import Array exposing (Array)
import Game.BrickLook as BrickLook
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Scenery exposing (ArchKind(..), ArchSpot)
import Racer.TerrainShader exposing (TerrainVertex)
import Racer.Track as Track
import Racer.Types
    exposing
        ( BrickRamp
        , Checkpoint
        , GeneratedTrack
        , ObstacleKind(..)
        , SurfaceType(..)
        , TrackId(..)
        , TrackObstacle
        , TrackSample
        )
import WebGL


{-| One spatial `100 m` slice of ground and the brick launch ramps that start
in it (`mesh`), the brick-built scenery still standing in it (`scenery`) and its
start, checkpoint and finish arches.
-}
type alias TrackChunk =
    { chunkIndex : Int
    , minS : Float
    , maxS : Float
    , boundsCenter : Vec3
    , boundsRadius : Float
    , mesh : WebGL.Mesh TerrainVertex
    , scenery : List TrackObstacle
    , arches : List ArchSpot
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
ldrawColor =
    BrickLook.colour


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


{-| Circular display plinth: a dark grey studded plate ringed by red and white
curb plates, for Garage inspection.
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
            ldrawColor 72

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
            in
            [ tri centerV in1 in0 upN topCol ]
                ++ quad out0 in0 in1 out1 upN curbCol
                ++ quad low0 out0 out1 low1 sideN (vec3 0.35 0.36 0.38)
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

        arches =
            let
                startArch =
                    if chunkIdx == 0 then
                        Array.get 0 track.samples
                            |> Maybe.map (\s0 -> [ archSpot s0 StartArch ])
                            |> Maybe.withDefault []

                    else
                        []

                cpArches =
                    track.checkpoints
                        |> List.filter (\cp -> cp.s >= minS && cp.s < maxS + 2.0)
                        |> List.map
                            (\cp ->
                                archSpot (Track.splineSampleAtS track cp.s)
                                    (if cp.isFinish then
                                        FinishArch

                                     else
                                        CheckpointArch
                                    )
                            )
            in
            startArch ++ cpArches

        rampTris =
            track.brickRamps
                |> List.filter (\rp -> rp.startS >= minS && rp.startS < maxS)
                |> List.concatMap (buildBrickRampTris track)

        centerSample =
            Track.splineSampleAtS track ((minS + maxS) * 0.5)
    in
    { chunkIndex = chunkIdx
    , minS = minS
    , maxS = maxS
    , boundsCenter = centerSample.center
    , boundsRadius = chunkSizeM * 0.85 + 52.0
    , mesh = WebGL.triangles (ribbonTris ++ rampTris)
    , scenery = sceneryInRange minS maxS track.obstacles
    , arches = arches
    }


sceneryInRange : Float -> Float -> List TrackObstacle -> List TrackObstacle
sceneryInRange minS maxS obstacles =
    List.filter (\o -> o.s >= minS && o.s < maxS) obstacles


{-| Refresh only the scenery list of the chunk that holds arc length `s`, after
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
                        { chunk | scenery = sceneryInRange chunk.minS chunk.maxS track.obstacles }
                        bundle.chunks
            }

        Nothing ->
            bundle


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

                studs =
                    bandStuds surface bandIdx

                v cv =
                    toTV cv col studs
            in
            [ ( v a0, v b0, v b1 )
            , ( v a0, v b1, v a1 )
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


{-| Road colours, from the LDraw palette: reddish brown and dark tan dirt,
dark grey gravel, dark brown forest track, tan footpath, and green for the
grassy surfaces.
-}
roadColors : SurfaceType -> { road : Vec3, edge : Vec3, center : Vec3 }
roadColors surface =
    case surface of
        DirtRoad ->
            { road = ldrawColor 70, edge = ldrawColor 28, center = ldrawColor 2 }

        GravelRoad ->
            { road = ldrawColor 72, edge = ldrawColor 71, center = ldrawColor 71 }

        ForestTrack ->
            { road = ldrawColor 308, edge = ldrawColor 70, center = ldrawColor 70 }

        GrassPath ->
            { road = ldrawColor 19, edge = ldrawColor 28, center = ldrawColor 2 }

        RoughVerge ->
            { road = ldrawColor 2, edge = ldrawColor 2, center = ldrawColor 2 }

        MeadowGrass ->
            { road = ldrawColor 10, edge = ldrawColor 10, center = ldrawColor 10 }


{-| Whether a cross-section band is studded plates (1) or smooth tiles (0):
the road bands of an unpaved road are tiles, grass is plates.
-}
bandStuds : SurfaceType -> Int -> Float
bandStuds surface bandIdx =
    let
        isRoadBand =
            bandIdx >= 4 && bandIdx <= 8
    in
    case surface of
        RoughVerge ->
            1

        MeadowGrass ->
            1

        _ ->
            if isRoadBand && bandIdx /= 6 then
                0

            else if isRoadBand then
                -- The grassy strip down the middle of a dirt road or path.
                if surface == GravelRoad || surface == ForestTrack then
                    0

                else
                    1

            else
                1


fieldColors : TrackId -> Int -> { verge : Vec3, near : Vec3, far : Vec3 }
fieldColors trackId segIdx =
    case trackId of
        FarmLane ->
            { verge = ldrawColor 10
            , near = ldrawColor 2
            , far =
                -- Alternating crop strips: ripening grain and pasture
                if modBy 10 (segIdx + 1000) < 5 then
                    ldrawColor 191

                else
                    ldrawColor 10
            }

        ForestTrail ->
            { verge = ldrawColor 2
            , near = ldrawColor 288
            , far = ldrawColor 288
            }

        MeadowPath ->
            { verge = ldrawColor 10
            , near = ldrawColor 10
            , far = ldrawColor 326
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
            1.0 + 0.04 * patchNoise segIdx bandIdx
    in
    Vec3.scale jitter base


archSpot : TrackSample -> ArchKind -> ArchSpot
archSpot smp kind =
    { center = smp.center
    , right = normalizeXZ smp.right
    , forward = normalizeXZ smp.tangent
    , halfWidth = smp.halfWidth + 0.3
    , kind = kind
    }



-- ── Debris of smashed scenery ────────────────────────────────────────────────


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



-- ── Brick launch ramps ───────────────────────────────────────────────────────


{-| A launch ramp of slope bricks on one lane: rows of red and white slopes
rising along the road to a yellow-and-black lip with a row of studs on top, a
grey back face, and grey side slopes down to the road. The slopes are smooth
(no studs in the brick-look shader), like real slope bricks. Every corner height
comes from `Track.brickRampLift`, so the bricks match what the wheels feel.
-}
buildBrickRampTris : GeneratedTrack -> BrickRamp -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildBrickRampTris track rp =
    let
        rows =
            5

        lenM =
            rp.endS - rp.startS

        inner =
            rp.halfWidthM * 0.72

        -- Lateral stations: road edge, top edge, a seam per slope brick, top edge, road edge.
        lateral =
            [ -rp.halfWidthM, -inner, -(inner / 3.0), inner / 3.0, inner, rp.halfWidthM ]
                |> List.map (\d -> rp.centerOffsetM + d)

        -- Just short of the lip, where the lift is at full height.
        rowS k =
            rp.startS + lenM * min 0.999 (toFloat k / toFloat rows)

        groundAt sVal d =
            let
                smp =
                    Track.splineSampleAtS track sVal

                ptXZ =
                    Vec3.add smp.center (Vec3.scale d (normalizeXZ smp.right))
            in
            vec3 (Vec3.getX ptXZ) (Track.groundHeightAt track sVal (Vec3.getX ptXZ) (Vec3.getZ ptXZ) + 0.02) (Vec3.getZ ptXZ)

        topAt sVal d =
            Vec3.add (groundAt sVal d) (vec3 0 (Track.brickRampLift track sVal d).lift 0)

        faceNormal a b c =
            normalizeSafe (Vec3.cross (Vec3.sub b a) (Vec3.sub c a)) (vec3 0 1 0)

        face a b c d col =
            let
                n0 =
                    faceNormal a b c

                n =
                    if Vec3.getY n0 < 0.0 then
                        Vec3.negate n0

                    else
                        n0
            in
            smoothQuad a b c d n col

        slopeColor k j =
            if j == 0 || j == 4 then
                ldrawColor 71

            else if k == rows - 1 then
                if modBy 2 j == 0 then
                    ldrawColor 0

                else
                    ldrawColor 14

            else if modBy 2 (k + j) == 0 then
                ldrawColor 4

            else
                ldrawColor 15

        topQuads =
            List.range 0 (rows - 1)
                |> List.concatMap
                    (\k ->
                        adjacentPairs lateral
                            |> List.indexedMap
                                (\j ( d0, d1 ) ->
                                    face (topAt (rowS k) d0) (topAt (rowS k) d1) (topAt (rowS (k + 1)) d1) (topAt (rowS (k + 1)) d0) (slopeColor k j)
                                )
                            |> List.concat
                    )

        lipS =
            rowS rows

        backFace =
            adjacentPairs lateral
                |> List.concatMap
                    (\( d0, d1 ) ->
                        let
                            a =
                                groundAt lipS d0

                            b =
                                groundAt lipS d1

                            smp =
                                Track.splineSampleAtS track lipS
                        in
                        smoothQuad b a (topAt lipS d0) (topAt lipS d1) (normalizeXZ smp.tangent) (ldrawColor 72)
                    )

        lipStuds =
            List.range 0 3
                |> List.concatMap
                    (\i ->
                        cylinderAt (topAt (lipS - 0.35) (rp.centerOffsetM + (toFloat i - 1.5) * (inner * 0.6))) 0.13 0.1 6 (ldrawColor 14)
                    )
    in
    topQuads ++ backFace ++ lipStuds


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
            [ ( { position = b0, normal = sideNorm, color = color, studs = 0 }
              , { position = b1, normal = sideNorm, color = color, studs = 0 }
              , { position = t1, normal = sideNorm, color = color, studs = 0 }
              )
            , ( { position = b0, normal = sideNorm, color = color, studs = 0 }
              , { position = t1, normal = sideNorm, color = color, studs = 0 }
              , { position = t0, normal = sideNorm, color = color, studs = 0 }
              )
            , ( { position = topCenter, normal = upNorm, color = color, studs = 0 }
              , { position = t0, normal = upNorm, color = color, studs = 0 }
              , { position = t1, normal = upNorm, color = color, studs = 0 }
              )
            ]
    in
    List.concatMap step (List.range 0 (segments - 1))


toTV : CrossVertex -> Vec3 -> Float -> TerrainVertex
toTV cv col studs =
    { position = cv.pos
    , normal = cv.normal
    , color = col
    , studs = studs
    }


quad : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
quad p0 p1 p2 p3 n col =
    [ tri p0 p1 p2 n col
    , tri p0 p2 p3 n col
    ]


{-| A quad of smooth faces (tiles and slopes, no studs).
-}
smoothQuad : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
smoothQuad p0 p1 p2 p3 n col =
    List.map (\( a, b, c ) -> ( { a | studs = 0 }, { b | studs = 0 }, { c | studs = 0 } )) (quad p0 p1 p2 p3 n col)


tri : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( TerrainVertex, TerrainVertex, TerrainVertex )
tri a b c n col =
    ( { position = a, normal = n, color = col, studs = 1 }
    , { position = b, normal = n, color = col, studs = 1 }
    , { position = c, normal = n, color = col, studs = 1 }
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
