module Racer.TerrainMesh exposing
    ( TrackChunk
    , TrackRenderBundle
    , bandColor
    , buildGaragePlinthMesh
    , buildTrackRenderBundle
    , rebuildChunkScenery
    , roadColors
    , sceneryDebrisStyle
    )

{-| Chunked WebGL countryside ground (unpaved road, verges and fields, launch
humps) in the brick look of LEGO plates and tiles, the brick launch ramps and
loop-the-loops on it, and the garage display plinth mesh.

Each chunk also lists the brick-built roadside scenery still standing in it
and the arches it holds; `Racer.Scenery` draws those from catalog models, so
removing a smashed piece only filters a short list.

-}

import Array exposing (Array)
import Game.BrickLook as BrickLook
import Game.MapParams exposing (Biome(..), MapParams, Season(..))
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Scenery exposing (ArchKind(..), ArchSpot)
import Racer.TerrainShader exposing (TerrainVertex)
import Racer.Track as Track
import Racer.Types
    exposing
        ( BrickRamp
        , Checkpoint
        , GeneratedTrack
        , LoopSection
        , ObstacleKind(..)
        , SideRamp
        , SurfaceType(..)
        , TrackObstacle
        , TrackSample
        )
import WebGL


{-| One spatial `100 m` slice of ground and the brick launch ramps and loops
and side ramps that start in it (`mesh`), the brick-built scenery still standing in it (`scenery`) and its
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
    { map : MapParams
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
    { map = track.config.map
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

                gantryArches =
                    track.checkpoints
                        |> List.filterMap
                            (\cp ->
                                let
                                    gantryS =
                                        cp.s - 55.0
                                in
                                if gantryS >= minS && gantryS < maxS && gantryS > 40.0 then
                                    Just (archSpot (Track.splineSampleAtS track gantryS) SponsorGantry)

                                else
                                    Nothing
                            )
            in
            startArch ++ cpArches ++ gantryArches

        rampTris =
            track.brickRamps
                |> List.filter (\rp -> rp.startS >= minS && rp.startS < maxS)
                |> List.concatMap (buildBrickRampTris track)

        sideRampTris =
            track.sideRamps
                |> List.filter (\sr -> sr.startS >= minS && sr.startS < maxS)
                |> List.concatMap (buildSideRampTris track)

        loopTris =
            track.loops
                |> List.filter (\lp -> lp.entryS >= minS && lp.entryS < maxS)
                |> List.concatMap (buildLoopTris track)

        centerSample =
            Track.splineSampleAtS track ((minS + maxS) * 0.5)
    in
    { chunkIndex = chunkIdx
    , minS = minS
    , maxS = maxS
    , boundsCenter = centerSample.center
    , boundsRadius = chunkSizeM * 0.85 + 52.0
    , mesh = WebGL.triangles (ribbonTris ++ rampTris ++ sideRampTris ++ loopTris)
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
            stitchCrossSections track.config.map s0.surface -2 rowBack2 rowBack1
                ++ stitchCrossSections track.config.map s0.surface -1 rowBack1 rowZero


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
            stitchCrossSections track.config.map s0.surface idx row0 row1

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
    MapParams
    -> SurfaceType
    -> Int
    -> List CrossVertex
    -> List CrossVertex
    -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
stitchCrossSections map surface segIdx r0 r1 =
    let
        pairs0 =
            adjacentPairs r0

        pairs1 =
            adjacentPairs r1

        stepBand bandIdx ( ( a0, b0 ), ( a1, b1 ) ) =
            let
                col =
                    bandColor map surface segIdx bandIdx

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


{-| The field and verge colours of a biome in summer.
-}
fieldColors : Biome -> Int -> { verge : Vec3, near : Vec3, far : Vec3 }
fieldColors biome segIdx =
    case biome of
        Farmland ->
            { verge = ldrawColor 10
            , near = ldrawColor 2
            , far =
                -- Alternating crop strips: ripening grain and pasture
                if modBy 10 (segIdx + 1000) < 5 then
                    ldrawColor 191

                else
                    ldrawColor 10
            }

        Boreal ->
            { verge = ldrawColor 2
            , near = ldrawColor 288
            , far = ldrawColor 288
            }

        Fell ->
            { verge = ldrawColor 10
            , near = ldrawColor 10
            , far = ldrawColor 326
            }

        Desert ->
            { verge = ldrawColor 28
            , near = ldrawColor 19
            , far =
                -- Sand dunes: tan with dark tan ridges
                if modBy 10 (segIdx + 1000) < 3 then
                    ldrawColor 28

                else
                    ldrawColor 19
            }


{-| The field and verge colours of a course in a season. Summer keeps the
course's own colours. Spring is bright green, with light-green strips on the
farm; autumn is tan with dark orange strips and a dark tan verge; winter is
white snow with a light bluish grey verge. The desert keeps its sand in every
season (`groundSeason`).
-}
seasonFieldColors : MapParams -> Int -> { verge : Vec3, near : Vec3, far : Vec3 }
seasonFieldColors map segIdx =
    let
        strip =
            modBy 10 (segIdx + 1000) < 5
    in
    case groundSeason map of
        Summer ->
            fieldColors map.biome segIdx

        Spring ->
            { verge = ldrawColor 10
            , near = ldrawColor 10
            , far =
                if map.biome == Farmland && strip then
                    ldrawColor 326

                else
                    ldrawColor 2
            }

        Autumn ->
            { verge = ldrawColor 28
            , near = ldrawColor 19
            , far =
                if strip then
                    ldrawColor 484

                else
                    ldrawColor 19
            }

        Winter ->
            { verge = ldrawColor 71
            , near = ldrawColor 15
            , far = ldrawColor 15
            }


{-| The season the ground shows. The desert has no green spring, autumn
leaves or winter snow (its winter weathers have no snow either), so its
ground always shows its summer sand.
-}
groundSeason : MapParams -> Season
groundSeason map =
    if map.biome == Desert then
        Summer

    else
        map.season


{-| Road colours in a season: in winter the road edges and the grassy
surfaces are under snow (white).
-}
seasonRoadColors : Season -> SurfaceType -> { road : Vec3, edge : Vec3, center : Vec3 }
seasonRoadColors season surface =
    let
        road =
            roadColors surface
    in
    case season of
        Winter ->
            case surface of
                RoughVerge ->
                    { road = ldrawColor 15, edge = ldrawColor 15, center = ldrawColor 15 }

                MeadowGrass ->
                    { road = ldrawColor 15, edge = ldrawColor 15, center = ldrawColor 15 }

                DirtRoad ->
                    { road | edge = ldrawColor 15, center = ldrawColor 15 }

                GrassPath ->
                    { road | edge = ldrawColor 15, center = ldrawColor 15 }

                _ ->
                    { road | edge = ldrawColor 15 }

        _ ->
            road


{-| Color of a ground band at station index `bandIdx` (0 to 12) along cross-section segment `segIdx`,
in the map's season.
Segment -1 holds the crisp white starting line leading up to the start arch at `s = 0`,
while segment -2 provides alternating yellow hazard and white grid staging stalls.
-}
bandColor : MapParams -> SurfaceType -> Int -> Int -> Vec3
bandColor map surface segIdx bandIdx =
    let
        road =
            seasonRoadColors (groundSeason map) surface

        field =
            seasonFieldColors map segIdx

        -- Alternating red and white curb rumble strips along road edge bands (4 and 8)
        isCurb =
            (bandIdx == 4 || bandIdx == 8)
                && (surface == DirtRoad || surface == GravelRoad || surface == ForestTrack || surface == GrassPath)
                && (modBy 4 (segIdx // 2) < 2)

        isStartMark =
            (segIdx == -1 && bandIdx >= 5 && bandIdx <= 7)
                || (segIdx == -2 && bandIdx >= 5 && bandIdx <= 7)

        isRockCut =
            (bandIdx <= 1 || bandIdx >= 11)
                && (map.biome == Boreal)
                && (modBy 7 (segIdx + 1000) < 3)

        rockCutColor =
            if modBy 2 (segIdx + bandIdx) == 0 then
                ldrawColor 72

            else
                ldrawColor 71

        base =
            if isCurb then
                if modBy 4 (segIdx // 2) == 0 then
                    ldrawColor 4

                else
                    ldrawColor 15

            else if isRockCut then
                rockCutColor

            else if segIdx == -1 && bandIdx >= 5 && bandIdx <= 7 then
                ldrawColor 15

            else if segIdx == -2 && bandIdx == 5 then
                ldrawColor 14

            else if segIdx == -2 && bandIdx == 6 then
                road.road

            else if segIdx == -2 && bandIdx == 7 then
                ldrawColor 15

            else
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
            if isCurb || isStartMark then
                1.0

            else
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

        PalmTree ->
            { colors = [ ldrawColor 70, ldrawColor 2, ldrawColor 19 ], count = 9, scale = 1.0 }

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

        BrickPallet ->
            { colors = [ ldrawColor 4, ldrawColor 14, ldrawColor 1, ldrawColor 70 ], count = 10, scale = 0.9 }

        TileStack ->
            { colors = [ ldrawColor 71, ldrawColor 72, ldrawColor 15, ldrawColor 14 ], count = 8, scale = 0.7 }

        BrickGate ->
            { colors = [ ldrawColor 15, ldrawColor 15, ldrawColor 4, ldrawColor 14 ], count = 9, scale = 0.85 }

        SlopeBlock ->
            { colors = [ ldrawColor 14, ldrawColor 0, ldrawColor 14, ldrawColor 72 ], count = 10, scale = 0.85 }

        CountdownBoard ->
            { colors = [ ldrawColor 15, ldrawColor 4, ldrawColor 71 ], count = 6, scale = 0.7 }

        Chalet ->
            { colors = [ ldrawColor 70, ldrawColor 320, ldrawColor 72 ], count = 12, scale = 0.9 }

        PitGarage ->
            { colors = [ ldrawColor 71, ldrawColor 1, ldrawColor 0 ], count = 10, scale = 0.85 }



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



-- ── Rounded side ramps ───────────────────────────────────────────────────────


{-| A rounded side ramp of curved slope bricks: rows of red and white curves
rising from the road edge, a yellow lip along the top and a green shelf behind
it fading back into the field. Every point's height comes from `Track.sideRampLift`, so the bricks
match what the wheels feel.
-}
buildSideRampTris : GeneratedTrack -> SideRamp -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildSideRampTris track sr =
    let
        rows =
            16

        arcSteps =
            8

        rowS k =
            sr.startS + (sr.endS - sr.startS) * toFloat k / toFloat rows

        widthM =
            sr.outerOffsetM - sr.innerOffsetM

        -- Lateral stations (distance from the road centre): along the curve,
        -- just short of the lip, then across the shelf behind it.
        stations =
            List.map (\j -> sr.innerOffsetM + widthM * min 0.999 (toFloat j / toFloat arcSteps)) (List.range 0 arcSteps)
                ++ List.map (\j -> sr.outerOffsetM + 16.0 * toFloat j / 6.0) (List.range 1 6)

        topAt sVal absD =
            let
                smp =
                    Track.splineSampleAtS track sVal

                d =
                    sr.side * absD

                ptXZ =
                    Vec3.add smp.center (Vec3.scale d (normalizeXZ smp.right))

                ground =
                    Track.groundHeightAt track sVal (Vec3.getX ptXZ) (Vec3.getZ ptXZ)
            in
            vec3 (Vec3.getX ptXZ) (ground + (Track.sideRampLift track sVal d).lift + 0.02) (Vec3.getZ ptXZ)

        color k j =
            if j >= arcSteps then
                ldrawColor 2

            else if j == arcSteps - 1 then
                ldrawColor 14

            else if modBy 2 (k // 2 + j // 3) == 0 then
                ldrawColor 4

            else
                ldrawColor 15

        quadAt k j ( d0, d1 ) =
            let
                a =
                    topAt (rowS k) d0

                b =
                    topAt (rowS k) d1

                c =
                    topAt (rowS (k + 1)) d1

                d =
                    topAt (rowS (k + 1)) d0

                n0 =
                    normalizeSafe (Vec3.cross (Vec3.sub b a) (Vec3.sub d a)) (vec3 0 1 0)

                n =
                    if Vec3.getY n0 < 0.0 then
                        Vec3.negate n0

                    else
                        n0
            in
            facing n a b c d (color k j)
    in
    List.range 0 (rows - 1)
        |> List.concatMap (\k -> List.indexedMap (quadAt k) (adjacentPairs stations) |> List.concat)



-- ── Brick loops ──────────────────────────────────────────────────────────────


{-| A stunt loop-the-loop of bricks: an engineered rollercoaster structure with
seamless road-to-apron transitions, multi-tier foundation abutments embedded
deeply into the terrain, A-frame truss bents with lateral outrigger sway bracing,
a continuous box-girder spine arch rib, transverse cantilever bracket ties, and
banded safety kerbs with raised guardrails and race hazard chevrons.
Every surface station comes from `Track.loopFrame`.
-}
buildLoopTris : GeneratedTrack -> LoopSection -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
buildLoopTris track loop =
    let
        segments =
            96

        kerbW =
            0.26

        guardW =
            0.16

        skinM =
            0.22

        section i =
            let
                th =
                    2.0 * pi * toFloat i / toFloat segments

                fromEnd =
                    min th (2.0 * pi - th)

                hw =
                    Track.loopLaneHalfWidth loop th

                liftM =
                    0.003 + 0.027 * smoothstep 0.0 0.12 fromEnd

                effectiveSkinM =
                    skinM * smoothstep 0.0 0.22 fromEnd

                innerKerbH =
                    0.05 * smoothstep 0.0 0.2 fromEnd

                guardH =
                    0.42 * smoothstep 0.0 0.32 fromEnd

                spineDepth =
                    0.24 * smoothstep 0.0 0.25 fromEnd

                at lat along =
                    let
                        f =
                            Track.loopFrame track loop th lat
                    in
                    Vec3.add f.position (Vec3.scale along f.up)

                mid =
                    Track.loopFrame track loop th 0.0

                laneY =
                    liftM
            in
            { lane =
                [ at -hw laneY
                , at -0.14 laneY
                , at 0.14 laneY
                , at hw laneY
                ]
            , leftKerb =
                [ at -hw laneY
                , at -hw (laneY + innerKerbH)
                , at (-hw - kerbW) (laneY + innerKerbH)
                ]
            , leftGuard =
                [ at (-hw - kerbW) (laneY + innerKerbH)
                , at (-hw - kerbW) (laneY + guardH)
                , at (-hw - kerbW - guardW) (laneY + guardH)
                , at (-hw - kerbW - guardW) (laneY - effectiveSkinM)
                ]
            , rightKerb =
                [ at hw laneY
                , at hw (laneY + innerKerbH)
                , at (hw + kerbW) (laneY + innerKerbH)
                ]
            , rightGuard =
                [ at (hw + kerbW) (laneY + innerKerbH)
                , at (hw + kerbW) (laneY + guardH)
                , at (hw + kerbW + guardW) (laneY + guardH)
                , at (hw + kerbW + guardW) (laneY - effectiveSkinM)
                ]
            , skin =
                [ at (-hw - kerbW - guardW) (laneY - effectiveSkinM)
                , at -0.32 (laneY - effectiveSkinM)
                , at 0.32 (laneY - effectiveSkinM)
                , at (hw + kerbW + guardW) (laneY - effectiveSkinM)
                ]
            , spine =
                [ at -0.32 (laneY - effectiveSkinM)
                , at -0.24 (laneY - effectiveSkinM - spineDepth)
                , at 0.24 (laneY - effectiveSkinM - spineDepth)
                , at 0.32 (laneY - effectiveSkinM)
                ]
            , up = mid.up
            , right = mid.right
            , tangent = mid.tangent
            , hw = hw
            , fromEnd = fromEnd
            }

        sections =
            List.map section (List.range 0 segments)

        strips i ( a, b ) =
            let
                isApron =
                    a.fromEnd < Track.loopApronRad

                laneColor j =
                    if isApron then
                        if j == 1 then
                            ldrawColor 15

                        else if modBy 4 (i // 2) < 2 then
                            ldrawColor 14

                        else
                            ldrawColor 0

                    else if j == 1 then
                        if modBy 4 i < 2 then
                            ldrawColor 14

                        else
                            ldrawColor 71

                    else
                        ldrawColor 71

                kerbColor =
                    if modBy 4 (i // 2) < 2 then
                        ldrawColor 4

                    else
                        ldrawColor 15

                guardRailCapColor =
                    if modBy 4 (i // 2) < 2 then
                        ldrawColor 4

                    else
                        ldrawColor 14

                band pts0 pts1 normals colors =
                    List.map2 Tuple.pair (adjacentPairs pts0) (adjacentPairs pts1)
                        |> List.map3 (\n col ( ( p0, p1 ), ( q0, q1 ) ) -> facing n p0 p1 q1 q0 col) normals colors
                        |> List.concat

                lane =
                    band a.lane b.lane [ a.up, a.up, a.up ] [ laneColor 0, laneColor 1, laneColor 2 ]

                leftKerb =
                    band a.leftKerb b.leftKerb [ a.right, a.up ] [ kerbColor, kerbColor ]

                leftGuard =
                    band a.leftGuard b.leftGuard [ a.right, a.up, Vec3.negate a.right ] [ ldrawColor 14, guardRailCapColor, ldrawColor 72 ]

                rightKerb =
                    band a.rightKerb b.rightKerb [ Vec3.negate a.right, a.up ] [ kerbColor, kerbColor ]

                rightGuard =
                    band a.rightGuard b.rightGuard [ Vec3.negate a.right, a.up, a.right ] [ ldrawColor 14, guardRailCapColor, ldrawColor 72 ]

                skin =
                    band a.skin b.skin [ Vec3.negate a.up, Vec3.negate a.up, Vec3.negate a.up ] [ ldrawColor 25, ldrawColor 25, ldrawColor 25 ]

                spine =
                    band a.spine b.spine [ Vec3.negate a.right, Vec3.negate a.up, a.right ] [ ldrawColor 72, ldrawColor 71, ldrawColor 72 ]

                bracket =
                    if modBy 6 i == 0 && a.fromEnd >= 0.45 && a.fromEnd <= (pi - 0.2) then
                        let
                            tangentW =
                                Vec3.scale 0.08 a.tangent

                            pL_spine =
                                List.head a.spine |> Maybe.withDefault a.right

                            pL_outer =
                                List.head a.skin |> Maybe.withDefault a.right

                            pR_spine =
                                List.drop 3 a.spine |> List.head |> Maybe.withDefault a.right

                            pR_outer =
                                List.drop 3 a.skin |> List.head |> Maybe.withDefault a.right

                            bracketQuad pOuter pSpine col =
                                let
                                    b0 =
                                        Vec3.sub pOuter tangentW

                                    b1 =
                                        Vec3.add pOuter tangentW

                                    b2 =
                                        Vec3.add pSpine tangentW

                                    b3 =
                                        Vec3.sub pSpine tangentW
                                in
                                facing (Vec3.negate a.up) b0 b1 b2 b3 col
                        in
                        bracketQuad pL_outer pL_spine (ldrawColor 14)
                            ++ bracketQuad pR_outer pR_spine (ldrawColor 14)

                    else
                        []
            in
            lane ++ leftKerb ++ leftGuard ++ rightKerb ++ rightGuard ++ skin ++ spine ++ bracket

        apronEndCaps =
            let
                s0 =
                    List.head sections

                sLast =
                    List.drop (List.length sections - 1) sections |> List.head

                capAt s fwdSign =
                    case s of
                        Just sec ->
                            let
                                fwd =
                                    Vec3.scale fwdSign sec.tangent

                                roadL =
                                    Vec3.add (List.head sec.lane |> Maybe.withDefault sec.right) (Vec3.scale -0.003 sec.up)

                                roadR =
                                    Vec3.add (List.drop 3 sec.lane |> List.head |> Maybe.withDefault sec.right) (Vec3.scale -0.003 sec.up)

                                laneL =
                                    List.head sec.lane |> Maybe.withDefault sec.right

                                laneR =
                                    List.drop 3 sec.lane |> List.head |> Maybe.withDefault sec.right

                                thresholdPlate =
                                    facing sec.up (Vec3.add roadL fwd) (Vec3.add roadR fwd) laneR laneL (ldrawColor 72)

                                noseBlock guardPts nOut =
                                    case guardPts of
                                        _ :: pTopIn :: pTopOut :: _ ->
                                            let
                                                pTip =
                                                    Vec3.add (Vec3.scale 0.5 (Vec3.add pTopIn pTopOut)) (Vec3.add (Vec3.scale 0.55 fwd) (Vec3.scale -0.42 sec.up))
                                            in
                                            [ facingTri sec.up pTopIn pTopOut pTip (ldrawColor 14)
                                            , facingTri nOut pTopOut (Vec3.add pTopOut (Vec3.scale -0.42 sec.up)) pTip (ldrawColor 72)
                                            ]

                                        _ ->
                                            []

                                leftNose =
                                    noseBlock sec.leftGuard (Vec3.negate sec.right)

                                rightNose =
                                    noseBlock sec.rightGuard sec.right
                            in
                            thresholdPlate ++ leftNose ++ rightNose

                        Nothing ->
                            []
            in
            capAt s0 -1.0 ++ capAt sLast 1.0

        flankBent th =
            let
                f =
                    Track.loopFrame track loop th 0.0

                fromEnd =
                    min th (2.0 * pi - th)

                effectiveSkinM =
                    skinM * smoothstep 0.0 0.22 fromEnd

                spineDepth =
                    0.24 * smoothstep 0.0 0.25 fromEnd

                spinePt =
                    Vec3.add f.position (Vec3.scale -(effectiveSkinM + spineDepth) f.up)

                groundY =
                    Track.groundHeightAt track loop.entryS (Vec3.getX spinePt) (Vec3.getZ spinePt)

                across =
                    normalizeXZ f.right

                along =
                    vec3 -(Vec3.getZ across) 0 (Vec3.getX across)

                groundCenter =
                    vec3 (Vec3.getX spinePt) groundY (Vec3.getZ spinePt)

                anchorY =
                    groundY + 0.48

                anchorCenter =
                    vec3 (Vec3.getX spinePt) anchorY (Vec3.getZ spinePt)

                heightAboveGround =
                    Vec3.getY spinePt - groundY
            in
            if heightAboveGround > 0.8 then
                let
                    mainAbutment =
                        foundationAbutment groundCenter 1.1 across along

                    legW =
                        0.12

                    legFoot0 =
                        Vec3.add anchorCenter (Vec3.scale -0.32 across)

                    legFoot1 =
                        Vec3.add anchorCenter (Vec3.scale 0.32 across)

                    legTop0 =
                        Vec3.add spinePt (Vec3.scale -0.24 f.right)

                    legTop1 =
                        Vec3.add spinePt (Vec3.scale 0.24 f.right)

                    leftLeg =
                        strutBetween legFoot0 legTop0 legW (ldrawColor 72)

                    rightLeg =
                        strutBetween legFoot1 legTop1 legW (ldrawColor 72)

                    midP0 =
                        Vec3.scale 0.5 (Vec3.add legFoot0 legTop0)

                    midP1 =
                        Vec3.scale 0.5 (Vec3.add legFoot1 legTop1)

                    girt =
                        strutBetween midP0 midP1 0.08 (ldrawColor 71)

                    xBrace0 =
                        strutBetween legFoot0 midP1 0.06 (ldrawColor 14)

                    xBrace1 =
                        strutBetween legFoot1 midP0 0.06 (ldrawColor 14)

                    driftSign =
                        if loop.driftM >= 0 then
                            if th < pi then
                                1.0

                            else
                                -1.0

                        else if th < pi then
                            -1.0

                        else
                            1.0

                    outriggerOffset =
                        Vec3.scale (driftSign * 1.3) across

                    outriggerGround =
                        Vec3.add groundCenter outriggerOffset

                    outriggerGY =
                        Track.groundHeightAt track loop.entryS (Vec3.getX outriggerGround) (Vec3.getZ outriggerGround)

                    outriggerPos =
                        vec3 (Vec3.getX outriggerGround) outriggerGY (Vec3.getZ outriggerGround)

                    outriggerAnchor =
                        vec3 (Vec3.getX outriggerGround) (outriggerGY + 0.48) (Vec3.getZ outriggerGround)

                    outriggerAbutment =
                        foundationAbutment outriggerPos 0.8 across along

                    outriggerStrut =
                        strutBetween outriggerAnchor (Vec3.scale 0.5 (Vec3.add legTop0 legTop1)) 0.1 (ldrawColor 72)
                in
                mainAbutment ++ leftLeg ++ rightLeg ++ girt ++ xBrace0 ++ xBrace1 ++ outriggerAbutment ++ outriggerStrut

            else
                []

        riserBent th =
            let
                f =
                    Track.loopFrame track loop th 0.0

                fromEnd =
                    min th (2.0 * pi - th)

                effectiveSkinM =
                    skinM * smoothstep 0.0 0.22 fromEnd

                spineDepth =
                    0.24 * smoothstep 0.0 0.25 fromEnd

                spinePt =
                    Vec3.add f.position (Vec3.scale -(effectiveSkinM + spineDepth) f.up)

                groundY =
                    Track.groundHeightAt track loop.entryS (Vec3.getX spinePt) (Vec3.getZ spinePt)

                across =
                    normalizeXZ f.right

                along =
                    vec3 -(Vec3.getZ across) 0 (Vec3.getX across)

                groundCenter =
                    vec3 (Vec3.getX spinePt) groundY (Vec3.getZ spinePt)

                heightAboveGround =
                    Vec3.getY spinePt - groundY
            in
            if heightAboveGround > 0.35 then
                let
                    abutment =
                        foundationAbutment groundCenter 0.95 across along

                    anchorTop =
                        vec3 (Vec3.getX spinePt) (groundY + 0.48) (Vec3.getZ spinePt)

                    saddle =
                        strutBetween anchorTop spinePt 0.16 (ldrawColor 72)
                in
                abutment ++ saddle

            else
                []

        crownApexCrossTie =
            let
                fAsc =
                    Track.loopFrame track loop (0.94 * pi) 0.0

                fDesc =
                    Track.loopFrame track loop (1.06 * pi) 0.0

                pAsc =
                    Vec3.add fAsc.position (Vec3.scale -0.46 fAsc.up)

                pDesc =
                    Vec3.add fDesc.position (Vec3.scale -0.46 fDesc.up)

                tieBeam =
                    strutBetween pAsc pDesc 0.1 (ldrawColor 14)

                clampAsc =
                    orientedBox pAsc 0.18 0.18 0.14 fAsc.right fAsc.up fAsc.tangent (ldrawColor 72)

                clampDesc =
                    orientedBox pDesc 0.18 0.18 0.14 fDesc.right fDesc.up fDesc.tangent (ldrawColor 72)
            in
            tieBeam ++ clampAsc ++ clampDesc
    in
    (List.indexedMap strips (adjacentPairs sections) |> List.concat)
        ++ apronEndCaps
        ++ flankBent (0.46 * pi)
        ++ flankBent (1.54 * pi)
        ++ riserBent (0.24 * pi)
        ++ riserBent (1.76 * pi)
        ++ crownApexCrossTie


orientedBox :
    Vec3
    -> Float
    -> Float
    -> Float
    -> Vec3
    -> Vec3
    -> Vec3
    -> Vec3
    -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
orientedBox center hx hy hz ax ay az col =
    let
        corner sx sy sz =
            Vec3.add center
                (Vec3.add (Vec3.scale (sx * hx) ax)
                    (Vec3.add (Vec3.scale (sy * hy) ay) (Vec3.scale (sz * hz) az))
                )

        p000 =
            corner -1.0 -1.0 -1.0

        p001 =
            corner -1.0 -1.0 1.0

        p010 =
            corner -1.0 1.0 -1.0

        p011 =
            corner -1.0 1.0 1.0

        p100 =
            corner 1.0 -1.0 -1.0

        p101 =
            corner 1.0 -1.0 1.0

        p110 =
            corner 1.0 1.0 -1.0

        p111 =
            corner 1.0 1.0 1.0

        negAx =
            Vec3.negate ax

        negAy =
            Vec3.negate ay

        negAz =
            Vec3.negate az
    in
    facing ax p100 p110 p111 p101 col
        ++ facing negAx p000 p001 p011 p010 col
        ++ facing ay p010 p011 p111 p110 col
        ++ facing negAy p000 p100 p101 p001 col
        ++ facing az p001 p101 p111 p011 col
        ++ facing negAz p000 p010 p110 p100 col


strutBetween : Vec3 -> Vec3 -> Float -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
strutBetween p0 p1 halfW col =
    let
        delta =
            Vec3.sub p1 p0

        len =
            Vec3.length delta
    in
    if len < 0.1 then
        []

    else
        let
            ay =
                Vec3.scale (1.0 / len) delta

            rawAx =
                if abs (Vec3.getY ay) > 0.95 then
                    vec3 1 0 0

                else
                    vec3 0 1 0

            ax =
                normalizeSafe (Vec3.cross ay rawAx) (vec3 1 0 0)

            az =
                normalizeSafe (Vec3.cross ax ay) (vec3 0 0 1)

            mid =
                Vec3.scale 0.5 (Vec3.add p0 p1)
        in
        orientedBox mid halfW (len * 0.5) halfW ax ay az col


foundationAbutment : Vec3 -> Float -> Vec3 -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
foundationAbutment groundPos width across along =
    let
        gx =
            Vec3.getX groundPos

        gy =
            Vec3.getY groundPos

        gz =
            Vec3.getZ groundPos

        up =
            vec3 0 1 0

        footingH =
            0.65

        footingCenter =
            vec3 gx (gy - 0.45 + footingH * 0.5) gz

        footing =
            orientedBox footingCenter (width * 0.55) (footingH * 0.5) (width * 0.55) across up along (ldrawColor 72)

        pedestalH =
            0.22

        pedestalCenter =
            vec3 gx (gy + 0.2 + pedestalH * 0.5) gz

        pedestal =
            orientedBox pedestalCenter (width * 0.42) (pedestalH * 0.5) (width * 0.42) across up along (ldrawColor 71)

        plateH =
            0.06

        plateCenter =
            vec3 gx (gy + 0.42 + plateH * 0.5) gz

        plate =
            orientedBox plateCenter (width * 0.32) (plateH * 0.5) (width * 0.32) across up along (ldrawColor 0)
    in
    footing ++ pedestal ++ plate


smoothTri : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( TerrainVertex, TerrainVertex, TerrainVertex )
smoothTri a b c n col =
    ( { position = a, normal = n, color = col, studs = 0 }
    , { position = b, normal = n, color = col, studs = 0 }
    , { position = c, normal = n, color = col, studs = 0 }
    )


facingTri : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> ( TerrainVertex, TerrainVertex, TerrainVertex )
facingTri n a b c col =
    if Vec3.dot (Vec3.cross (Vec3.sub b a) (Vec3.sub c a)) n >= 0.0 then
        smoothTri a b c n col

    else
        smoothTri a c b n col


{-| A smooth quad `a b c d` wound so its front face looks along `n`, as the
ground's back-face culling needs.
-}
facing : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( TerrainVertex, TerrainVertex, TerrainVertex )
facing n a b c d col =
    if Vec3.dot (Vec3.cross (Vec3.sub b a) (Vec3.sub c a)) n >= 0.0 then
        smoothQuad a b c d n col

    else
        smoothQuad a d c b n col


smoothstep : Float -> Float -> Float -> Float
smoothstep edge0 edge1 x =
    let
        t =
            clamp 0.0 1.0 ((x - edge0) / max 1.0e-5 (edge1 - edge0))
    in
    t * t * (3.0 - 2.0 * t)


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
