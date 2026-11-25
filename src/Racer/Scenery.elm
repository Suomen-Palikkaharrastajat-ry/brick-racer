module Racer.Scenery exposing
    ( ArchKind(..)
    , ArchSpot
    , Props
    , archEntities
    , debrisEntities
    , empty
    , obstacleEntities
    , refresh
    )

{-| The brick-built things along a track: roadside scenery, the start,
checkpoint and finish arches and the loose bricks of a crash.

Every one is a model from `Game.BrickModels` (or, for the crates, barrels,
garden gnomes, brick walls, brick pallets, tile stacks, brick gates and slope
blocks, `Racer.SceneryModels`), made of catalog parts and
loaded like the car (`Game.BrickProps`). Each model is built once into
`Props` and drawn once per piece of scenery with its own model matrix, so a
course with hundreds of trees keeps one copy of the tree's geometry.

Only the looks live here. Where a piece stands and how big its collision
cylinder is stays in `Racer.Track` and `Racer.Physics`; the models are
scaled to the same sizes the old blocky scenery had.

-}

import Dict exposing (Dict)
import Game.BrickLook as BrickLook
import Game.BrickModels as BrickModels
import Game.BrickProps as BrickProps exposing (PropModel)
import LDraw.Resolve exposing (PartCache)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Racer.SceneryModels as SceneryModels
import Racer.TerrainShader exposing (DebrisPiece)
import Racer.Types exposing (ObstacleKind(..), TrackObstacle)
import WebGL


{-| The prop models built so far, by name. A model appears once all its
parts are in the cache; until then the pieces that use it are not drawn.
-}
type alias Props =
    Dict String PropModel


{-| No models yet.
-}
empty : Props
empty =
    Dict.empty


{-| Every model the racer draws: the scenery and the single bricks of the
arches and the debris.
-}
sources : List ( String, String )
sources =
    [ ( "leafyTree", BrickModels.leafyTree )
    , ( "pineTree", BrickModels.pineTree )
    , ( "bush", BrickModels.bush )
    , ( "fence", BrickModels.fence )
    , ( "rock", BrickModels.rock )
    , ( "signpost", BrickModels.signpost )
    , ( "hayBale", BrickModels.hayBale )
    , ( "hayStack", BrickModels.hayStack )
    , ( "cottage", BrickModels.cottage )
    , ( "barn", BrickModels.barn )
    ]
        ++ SceneryModels.all
        ++ [ ( "3001.dat", BrickModels.singlePart "3001.dat" )
           , ( "3003.dat", BrickModels.singlePart "3003.dat" )
           , ( debrisPart, BrickModels.singlePart debrisPart )
           , ( "54200.dat", BrickModels.singlePart "54200.dat" )
           , ( "6141.dat", BrickModels.singlePart "6141.dat" )
           ]


debrisPart : String
debrisPart =
    "3004.dat"


{-| Build every model whose parts have all arrived, and list the part files
still to fetch (marked as loading in the returned cache).
-}
refresh : PartCache -> Props -> { props : Props, cache : PartCache, fetch : List String }
refresh =
    BrickProps.refresh sources



-- SCENERY


{-| Which model a scenery piece is drawn with, and how it is fitted to the
piece's size (base on the ground, in metres).
-}
modelFor : TrackObstacle -> ( String, PropModel -> Mat4 )
modelFor obs =
    case obs.kind of
        LeafyTree ->
            ( "leafyTree", BrickProps.fitHeight obs.heightM )

        PineTree ->
            ( "pineTree", BrickProps.fitHeight obs.heightM )

        Bush ->
            ( "bush", BrickProps.fitHeight obs.heightM )

        FenceSection ->
            ( "fence", BrickProps.fitLength 3.0 )

        Rock ->
            ( "rock", BrickProps.fitBox { width = 2 * obs.radiusM, height = obs.heightM, depth = 1.6 * obs.radiusM } )

        Signpost ->
            ( "signpost", BrickProps.fitHeight obs.heightM )

        HayBale ->
            if obs.heightM > 1.2 then
                ( "hayStack", BrickProps.fitHeight obs.heightM )

            else
                ( "hayBale", BrickProps.fitHeight obs.heightM )

        Cottage ->
            ( "cottage", BrickProps.fitWidth 4.0 )

        Barn ->
            ( "barn", BrickProps.fitWidth 5.2 )

        Crate ->
            if obs.heightM > 1.2 then
                ( "crateStack", BrickProps.fitHeight obs.heightM )

            else
                ( "crate", BrickProps.fitHeight obs.heightM )

        Barrel ->
            ( "barrels", BrickProps.fitHeight obs.heightM )

        GardenGnome ->
            ( "gnome", BrickProps.fitHeight obs.heightM )

        BrickWall ->
            ( "brickWall", BrickProps.fitBox { width = 0.48, height = obs.heightM, depth = 2.92 } )

        BrickPallet ->
            ( "brickPallet", BrickProps.fitHeight obs.heightM )

        TileStack ->
            ( "tileStack", BrickProps.fitHeight obs.heightM )

        BrickGate ->
            ( "brickGate", BrickProps.fitHeight obs.heightM )

        SlopeBlock ->
            ( "slopeBlock", BrickProps.fitHeight obs.heightM )


noTint : Vec4
noTint =
    vec4 1 1 1 1


{-| Edge lines only on props close enough for them to read as edges.
-}
edgeWidthAt : Float -> Float
edgeWidthAt dist =
    clamp 0 1.2 (1.2 * (32 - dist) / 12)


{-| Draw the scenery pieces that are on screen and near enough to matter:
small pieces drop out sooner than trees and buildings, well inside the fog.
-}
obstacleEntities : BrickProps.View -> (Vec3 -> Float -> Bool) -> Props -> List TrackObstacle -> List WebGL.Entity
obstacleEntities v inView props obstacles =
    List.concatMap
        (\obs ->
            let
                ( name, fit ) =
                    modelFor obs

                dist =
                    Vec3.distance v.eye obs.position

                reach =
                    110 + 70 * max 1 obs.heightM

                boundR =
                    max obs.radiusM (obs.heightM * 0.7) + 1.5
            in
            if dist > reach || not (inView (Vec3.add obs.position (vec3 0 (obs.heightM * 0.5) 0)) boundR) then
                []

            else
                case Dict.get name props of
                    Just prop ->
                        let
                            world =
                                Mat4.makeTranslate obs.position
                                    |> (\m -> Mat4.mul m (Mat4.makeRotate obs.yaw (vec3 0 1 0)))
                                    |> (\m -> Mat4.mul m (fit prop))
                        in
                        BrickProps.draw v world noTint (edgeWidthAt dist) prop

                    Nothing ->
                        []
        )
        obstacles



-- ARCHES


{-| Start, checkpoint or finish.
-}
type ArchKind
    = StartArch
    | CheckpointArch
    | FinishArch


{-| Where an arch stands: the road centre, the road's right and forward
directions (level), and the half width between the posts.
-}
type alias ArchSpot =
    { center : Vec3
    , right : Vec3
    , forward : Vec3
    , halfWidth : Float
    , kind : ArchKind
    }


{-| Draw arches of 2 x 2 brick posts and a 2 x 4 brick beam
(`Game.BrickModels.archBricks`), the size of the old arches.
-}
archEntities : BrickProps.View -> (Vec3 -> Float -> Bool) -> Props -> List ArchSpot -> List WebGL.Entity
archEntities v inView props spots =
    List.concatMap
        (\spot ->
            let
                ( postColor, banner ) =
                    case spot.kind of
                        StartArch ->
                            ( 2, [ 14 ] )

                        CheckpointArch ->
                            ( 1, [ 191, 14 ] )

                        FinishArch ->
                            ( 0, [ 15, 0 ] )

                frame =
                    Mat4.mul (Mat4.makeTranslate spot.center) (Mat4.makeBasis spot.right (vec3 0 1 0) spot.forward)

                dist =
                    Vec3.distance v.eye spot.center

                edges =
                    edgeWidthAt dist
            in
            if dist > 520 || not (inView (Vec3.add spot.center (vec3 0 2.5 0)) (spot.halfWidth + 3)) then
                []

            else
                BrickModels.archBricks
                    { halfWidth = spot.halfWidth
                    , postHeight = 4.8
                    , unit = 0.016
                    , postColor = postColor
                    , bannerColors = banner
                    }
                    |> List.concatMap
                        (\b ->
                            case Dict.get b.part props of
                                Just prop ->
                                    BrickProps.draw v (Mat4.mul frame b.transform) (tintOf b.color) edges prop

                                Nothing ->
                                    []
                        )
        )
        spots


tintOf : Int -> Vec4
tintOf code =
    let
        c =
            Vec3.toRecord (BrickLook.colour code)
    in
    vec4 c.x c.y c.z 1



-- DEBRIS


{-| Draw the loose bricks of a crash: catalog 1 x 2 bricks, 1 x 1 cheese slopes
and 1 x 1 round studs in each piece's colour, shrinking away over their last
half second.
-}
debrisEntities : BrickProps.View -> Props -> List DebrisPiece -> List WebGL.Entity
debrisEntities v props pieces =
    List.indexedMap Tuple.pair pieces
        |> List.concatMap
            (\( idx, piece ) ->
                let
                    partFile =
                        case modBy 3 idx of
                            0 ->
                                debrisPart

                            1 ->
                                "54200.dat"

                            _ ->
                                "6141.dat"
                in
                case Dict.get partFile props of
                    Just prop ->
                        let
                            fade =
                                clamp 0.0 1.0 ((piece.lifetime - piece.age) / 0.5)

                            s =
                                Vec3.toRecord (Vec3.scale fade piece.size)

                            c =
                                Vec3.toRecord piece.color

                            world =
                                Mat4.makeTranslate piece.position
                                    |> (\m -> Mat4.mul m (Mat4.makeRotate piece.rotation (vec3 1 0.7 0.4)))
                                    |> (\m -> Mat4.mul m (Mat4.makeTranslate3 0 (-s.y / 2) 0))
                                    |> (\m -> Mat4.mul m (BrickProps.fitBox { width = max 0.001 s.x, height = max 0.001 s.y, depth = max 0.001 s.z } prop))
                        in
                        BrickProps.draw v world (vec4 c.x c.y c.z 1) 0 prop

                    Nothing ->
                        []
            )
