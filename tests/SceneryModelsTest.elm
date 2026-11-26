module SceneryModelsTest exposing (suite)

{-| The dock-built crashable scenery (brick pallets, tile stacks, brick gates
and slope blocks): every model is made only of catalog parts from the car
editor's dock, and the seeded placement keeps the pieces off the road, off the
start pad, clear of the checkpoint and finish arches and clear of the ramps.
-}

import Dict
import Expect
import Game.BrickModels as BrickModels
import Game.BrickProps as BrickProps
import Game.MapParams as Map
import LDraw.Geometry as Geometry
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartStatus(..))
import Math.Vector3 as Vec3
import Racer.Editor as Editor
import Racer.Scenery as Scenery
import Racer.SceneryModels as SceneryModels
import Racer.Track as Track
import Racer.Types exposing (ObstacleKind(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Dock-built crashable scenery"
        [ describe "Models are made of catalog parts"
            (List.map verifyCatalogParts catalogVerifiedModels)
        , describe "Full-scale dimensions and continuous fence coverage"
            [ verifyFenceContinuity
            , verifyBuildingScale "cottage" SceneryModels.cottage 400 360 500
            , verifyBuildingScale "barn" SceneryModels.barn 400 600 500
            , verifyBuildingScale "chalet" SceneryModels.chalet 400 440 500
            , verifyBuildingScale "pitGarage" SceneryModels.pitGarage 400 500 280
            ]
        , describe "Placements stay off the road and clear of the start, arches and ramps"
            (List.concatMap (\id -> List.map (verifyPlacement id) [ 7, 42, 99, 1234 ]) courseBiomes)
        , describe "Every course has some of them"
            (List.map verifyPresence courseBiomes)
        , describe "Static mesh optimization & BFC certification"
            [ verifyBfcCertification
            , verifySceneryPropDetailOptions
            ]
        ]


newKinds : List ObstacleKind
newKinds =
    [ BrickPallet, TileStack, BrickGate, SlopeBlock, CountdownBoard, Chalet, PitGarage ]


newModels : List ( String, String )
newModels =
    [ ( "brickPallet", SceneryModels.brickPallet )
    , ( "tileStack", SceneryModels.tileStack )
    , ( "brickGate", SceneryModels.brickGate )
    , ( "slopeBlock", SceneryModels.slopeBlock )
    , ( "countdownBoard", SceneryModels.countdownBoard )
    , ( "chalet", SceneryModels.chalet )
    , ( "pitGarage", SceneryModels.pitGarage )
    ]


catalogVerifiedModels : List ( String, String )
catalogVerifiedModels =
    newModels
        ++ [ ( "fence", SceneryModels.fence )
           , ( "cottage", SceneryModels.cottage )
           , ( "barn", SceneryModels.barn )
           , ( "brickWall", SceneryModels.brickWall )
           ]


{-| A parsed type-1 placement row from an LDraw model.
-}
type alias PlacementRow =
    { file : String
    , x : Float
    , y : Float
    , z : Float
    }


parsePlacements : String -> List PlacementRow
parsePlacements ldr =
    String.lines ldr
        |> List.filterMap
            (\line ->
                case String.words line of
                    "1" :: _ :: xStr :: yStr :: zStr :: rest ->
                        case ( String.toFloat xStr, String.toFloat yStr, String.toFloat zStr ) of
                            ( Just x, Just y, Just z ) ->
                                List.reverse rest
                                    |> List.head
                                    |> Maybe.map (\file -> { file = file, x = x, y = y, z = z })

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )


{-| The part file of every type 1 line of an LDraw model.
-}
partFiles : String -> List String
partFiles ldr =
    parsePlacements ldr
        |> List.map .file


verifyFenceContinuity : Test
verifyFenceContinuity =
    test "fence horizontal 1x6 rails span the full 480 LDU (3.0 m) step without seam double-posts" <|
        \_ ->
            let
                rows =
                    parsePlacements SceneryModels.fence

                rails =
                    List.filter (\r -> r.file == "3009.dat") rows

                railZs =
                    List.map .z rails

                minRailZ =
                    List.minimum railZs |> Maybe.withDefault 0

                maxRailZ =
                    List.maximum railZs |> Maybe.withDefault 0

                postZs =
                    List.filter (\r -> r.file == "3070b.dat") rows
                        |> List.map .z
                        |> List.sort

                postGaps =
                    List.map2 (\a b -> b - a) postZs (List.drop 1 postZs)

                seamGap =
                    case ( List.head postZs, List.reverse postZs |> List.head ) of
                        ( Just firstZ, Just lastZ ) ->
                            (480 + firstZ) - lastZ

                        _ ->
                            0
            in
            Expect.all
                [ \_ -> Expect.equal -180 minRailZ
                , \_ -> Expect.equal 180 maxRailZ
                , \_ -> Expect.equal [ 160, 160 ] postGaps
                , \_ -> Expect.equal 160 seamGap
                ]
                ()


verifyBuildingScale : String -> String -> Float -> Float -> Float -> Test
verifyBuildingScale name ldr minSpanX minSpanZ minHeight =
    test (name ++ " is built at authentic 1:1 full building scale") <|
        \_ ->
            let
                rows =
                    parsePlacements ldr

                xs =
                    List.map .x rows

                ys =
                    List.map .y rows

                zs =
                    List.map .z rows

                span list =
                    (List.maximum list |> Maybe.withDefault 0) - (List.minimum list |> Maybe.withDefault 0)
            in
            Expect.all
                [ \_ -> Expect.atLeast minSpanX (span xs)
                , \_ -> Expect.atLeast minSpanZ (span zs)
                , \_ -> Expect.atLeast minHeight (span ys)
                ]
                ()


verifyCatalogParts : ( String, String ) -> Test
verifyCatalogParts ( name, ldr ) =
    test (name ++ " uses only the editor dock's catalog parts") <|
        \_ ->
            let
                dock =
                    List.map .file Editor.parts

                files =
                    partFiles ldr

                notInDock =
                    List.filter (\f -> not (List.member f dock)) files

                listed =
                    List.map Tuple.first SceneryModels.all
            in
            Expect.all
                [ \_ -> Expect.equal [] notInDock
                , \_ -> Expect.atLeast 5 (List.length files)
                , \_ -> Expect.equal True (List.member name listed)
                ]
                ()


{-| The biomes whose courses these tests sweep.
-}
courseBiomes : List Map.Biome
courseBiomes =
    Map.allBiomes


verifyPlacement : Map.Biome -> Int -> Test
verifyPlacement id seed =
    test (Map.biomeToString id ++ " seed " ++ String.fromInt seed ++ " places the new pieces off the road and clear of the gates and ramps") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor { biome = id, season = Map.Summer, weather = Map.Clear, seed = seed })

                pieces =
                    List.filter (\o -> List.member o.kind newKinds) track.obstacles

                onRoad =
                    List.filter
                        (\o ->
                            let
                                q =
                                    Track.sampleAt track (Vec3.getX o.position) (Vec3.getZ o.position)
                            in
                            abs q.roadOffsetM - o.radiusM < q.halfWidthM + 0.25
                        )
                        pieces

                atStartOrFinish =
                    List.filter (\o -> o.s < 20.0 || o.s > track.finishS - 5.0) pieces

                atGate =
                    List.filter (\o -> List.any (\cp -> abs (cp.s - o.s) < 6.5) track.checkpoints) pieces

                atRamp =
                    List.filter
                        (\o ->
                            List.any (\rp -> o.s > rp.startS - 5.5 && o.s < rp.endS + 5.5) track.brickRamps
                                || List.any (\rp -> o.s > rp.startS - 5.5 && o.s < rp.endS + 5.5) track.ramps
                        )
                        pieces
            in
            Expect.all
                [ \_ -> Expect.equal [] onRoad
                , \_ -> Expect.equal [] atStartOrFinish
                , \_ -> Expect.equal [] atGate
                , \_ -> Expect.equal [] atRamp
                ]
                ()


verifyPresence : Map.Biome -> Test
verifyPresence id =
    test (Map.biomeToString id ++ " has several dock-built crashables over a few seeds") <|
        \_ ->
            let
                counts =
                    List.map
                        (\seed ->
                            Track.generate (Track.trackConfigFor { biome = id, season = Map.Summer, weather = Map.Clear, seed = seed })
                                |> .obstacles
                                |> List.filter (\o -> List.member o.kind newKinds)
                                |> List.length
                        )
                        [ 7, 42, 99 ]
            in
            Expect.atLeast 3 (List.minimum counts |> Maybe.withDefault 0)


verifyBfcCertification : Test
verifyBfcCertification =
    test "every 1:1 scenery model certifies BFC CCW for back-face culling" <|
        \_ ->
            SceneryModels.all
                |> List.all (\( _, ldr ) -> String.contains "0 BFC CERTIFY CCW" ldr)
                |> Expect.equal True


verifySceneryPropDetailOptions : Test
verifySceneryPropDetailOptions =
    test "Racer.Scenery.refresh uses BoxStuds without hollows on 1:1 structures and keeps full detail on single parts" <|
        \_ ->
            let
                studPart =
                    -- 10 quads (20 tris) modeling a simplified cylindrical stud (>= BoxStuds's 10 tris)
                    Parser.parseFile
                        (String.join "\n"
                            ([ "0 BFC CERTIFY CCW" ]
                                ++ List.repeat 10 "4 16 -4 0 -4 4 0 -4 4 -4 4 -4 -4 4"
                            )
                        )

                hollowPart =
                    Parser.parseFile "0 BFC CERTIFY CCW\n4 16 -2 0 -2 2 0 -2 2 8 2 -2 8 2"

                brickWithDetails =
                    Parser.parseFile
                        (String.join "\n"
                            [ "0 BFC CERTIFY CCW"
                            , "4 16 -20 0 -20 20 0 -20 20 24 20 -20 24 20"
                            , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat"
                            , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 48/stud2a.dat"
                            , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud4.dat"
                            , "2 24 -20 0 -20 20 0 -20"
                            ]
                        )

                allPartNames =
                    List.map .file Editor.parts
                        ++ [ "2412b.dat", "2423.dat", "3002.dat", "3005.dat", "3010.dat", "3020.dat", "3021.dat", "3022.dat", "3023.dat", "3024.dat", "3039.dat", "3062b.dat", "3068b.dat", "3070b.dat", "3471.dat", "3623.dat", "3641.dat", "3660b.dat", "3795.dat", "3942c.dat", "3957a.dat", "54200.dat", "56890.dat", "56891.dat", "6014b.dat", "6141.dat", "87580.dat" ]

                cache =
                    allPartNames
                        |> List.map (\n -> ( n, Loaded brickWithDetails ))
                        |> (++)
                            [ ( "stud.dat", Loaded studPart )
                            , ( "48/stud2a.dat", Loaded studPart )
                            , ( "stud4.dat", Loaded hollowPart )
                            ]
                        |> Dict.fromList

                fullGarage =
                    BrickProps.build SceneryModels.pitGarage cache

                refreshed =
                    Scenery.refresh cache Scenery.empty
            in
            Expect.all
                [ \_ -> Expect.equal Geometry.BoxStuds (Scenery.propDetailOptions "pitGarage").studs
                , \_ -> Expect.equal False (Scenery.propDetailOptions "pitGarage").includeHollows
                , \_ -> Expect.equal True (Scenery.propDetailOptions "pitGarage").includeLines
                , \_ -> Expect.equal Geometry.FullStuds (Scenery.propDetailOptions "3001.dat").studs
                , \_ -> Expect.equal True (Scenery.propDetailOptions "3004.dat").includeHollows
                , \_ ->
                    Dict.get "pitGarage" refreshed.props
                        |> Maybe.map (\p -> ( p.triangleCount < fullGarage.triangleCount, p.scene.bfcCertified ))
                        |> Expect.equal (Just ( True, True ))
                , \_ ->
                    let
                        fullBrick =
                            BrickProps.build (BrickModels.singlePart "3001.dat") cache
                    in
                    Dict.get "3001.dat" refreshed.props
                        |> Maybe.map .triangleCount
                        |> Expect.equal (Just fullBrick.triangleCount)
                ]
                ()
