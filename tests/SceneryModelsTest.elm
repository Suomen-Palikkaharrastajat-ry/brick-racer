module SceneryModelsTest exposing (suite)

{-| The dock-built crashable scenery (brick pallets, tile stacks, brick gates
and slope blocks): every model is made only of catalog parts from the car
editor's dock, and the seeded placement keeps the pieces off the road, off the
start pad, clear of the checkpoint and finish arches and clear of the ramps.
-}

import Dict
import Expect
import LDraw.Catalog as Catalog
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Racer.Analyze as Analyze
import Racer.Editor as Editor
import Racer.Render as Render
import Racer.SceneryModels as SceneryModels
import Racer.StarterBases as StarterBases
import Racer.Track as Track
import Racer.Types exposing (ObstacleKind(..), TrackId, allTrackIds, trackIdToString)
import Render.Camera as Camera
import Render.Style as Style
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Dock-built crashable scenery"
        [ describe "Models are made of catalog parts"
            (List.map verifyCatalogParts newModels)
        , describe "Placements stay off the road and clear of the start, arches and ramps"
            (List.concatMap (\id -> List.map (verifyPlacement id) [ 7, 42, 99, 1234 ]) allTrackIds)
        , describe "Every course has some of them"
            (List.map verifyPresence allTrackIds)
        , describe "Driver model"
            [ test "Driver model is listed in SceneryModels.all and uses catalog parts" <|
                \_ ->
                    let
                        files =
                            partFiles SceneryModels.driver

                        listed =
                            List.map Tuple.first SceneryModels.all
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.member "driver" listed)
                        , \_ -> Expect.equal [ "3004.dat", "3062b.dat", "54200.dat" ] files
                        ]
                        ()
            , test "CarMeshBundle contains driver and renderCarWithStyleAndSpins renders with dynamic lean" <|
                \_ ->
                    case StarterBases.all of
                        starter :: _ ->
                            let
                                doc =
                                    Document.parse starter.text

                                analyzed =
                                    Analyze.analyzeDocument starter.file doc Dict.empty Catalog.emptyPartIndex

                                cam =
                                    Camera.init

                                entities =
                                    Render.renderCarWithStyleAndSpins
                                        Mat4.identity
                                        0.25
                                        [ 0.0, 0.0, 0.0, 0.0 ]
                                        cam
                                        Style.defaultStyle
                                        analyzed.meshes
                                        800
                                        600
                            in
                            Expect.all
                                [ \_ -> Expect.notEqual Nothing analyzed.meshes.driver
                                , \_ -> Expect.atLeast 1 (List.length entities)
                                ]
                                ()

                        [] ->
                            Expect.fail "No starter base available"
            ]
        ]


newKinds : List ObstacleKind
newKinds =
    [ BrickPallet, TileStack, BrickGate, SlopeBlock ]


newModels : List ( String, String )
newModels =
    [ ( "brickPallet", SceneryModels.brickPallet )
    , ( "tileStack", SceneryModels.tileStack )
    , ( "brickGate", SceneryModels.brickGate )
    , ( "slopeBlock", SceneryModels.slopeBlock )
    ]


{-| The part file of every type 1 line of an LDraw model.
-}
partFiles : String -> List String
partFiles ldr =
    String.lines ldr
        |> List.filterMap
            (\line ->
                case String.words line of
                    "1" :: rest ->
                        List.reverse rest |> List.head

                    _ ->
                        Nothing
            )


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


verifyPlacement : TrackId -> Int -> Test
verifyPlacement id seed =
    test (trackIdToString id ++ " seed " ++ String.fromInt seed ++ " places the new pieces off the road and clear of the gates and ramps") <|
        \_ ->
            let
                track =
                    Track.generate (Track.trackConfigFor id seed)

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


verifyPresence : TrackId -> Test
verifyPresence id =
    test (trackIdToString id ++ " has several dock-built crashables over a few seeds") <|
        \_ ->
            let
                counts =
                    List.map
                        (\seed ->
                            Track.generate (Track.trackConfigFor id seed)
                                |> .obstacles
                                |> List.filter (\o -> List.member o.kind newKinds)
                                |> List.length
                        )
                        [ 7, 42, 99 ]
            in
            Expect.atLeast 3 (List.minimum counts |> Maybe.withDefault 0)
