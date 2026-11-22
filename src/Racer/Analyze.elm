module Racer.Analyze exposing
    ( AnalyzedVehicle
    , CarMeshBundle
    , DynamicSubMesh
    , FallbackCacheMode(..)
    , analyzeDocument
    , analyzeDocumentWithMode
    , buildFallbackCache
    , buildVehicleScenes
    , carWeightKgFromGrams
    , yUpFlip
    )

{-| Vehicle decomposition, mechanical validation, and sub-assembly mesh splitting.

Takes a parsed LDraw `Document`, `PartCache`, and `PartIndex` and extracts:

  - Wheel rims & tires (`LDraw.Connectivity` + `LDraw.Catalog` + geometry bounds),
  - Concentric rim+tire wheel assemblies paired across front and rear axles,
  - Real turntable / knuckle steering bogies vs fixed-axle fallback,
  - Optional `3829c01` cockpit steering wheel indicator,
  - Heading normalization (`+Z` forward, `+Y` up, bottom tires resting at `y = 0`),
  - Mass, Center of Mass (CoM), diagonal inertia, frontal drag area, ground clearance,
  - Physical connectivity graph analysis to identify disconnected loose parts,
  - Split `Render.Scene.Scene` sub-assemblies (`chassis`, `steeringBogie`, per-wheel meshes)
    ready for animated WebGL rendering.

-}

import Dict exposing (Dict)
import LDraw.Catalog as Catalog exposing (PartIndex)
import LDraw.Colors as Colors
import LDraw.Connectivity as Connectivity exposing (BoundingBox3D, ConnectionPoint, ConnectionType(..), PartConnectivity)
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Types exposing (SteeringMechanism(..), ValidationCode(..), ValidationIssue, ValidationSeverity(..), VehicleSpec, WheelSpec, lduToMeters, vec3LduToMeters)
import Render.Scene as Scene exposing (Scene)
import Set exposing (Set)


{-| Controls whether missing/failed parts synthesize fallback box/cylinder meshes.
-}
type FallbackCacheMode
    = UseProceduralFallbacks
    | StrictCacheOnly


{-| Combined vehicle specification and WebGL-ready sub-assembly meshes.
-}
type alias AnalyzedVehicle =
    { spec : VehicleSpec
    , meshes : CarMeshBundle
    }


{-| Pre-baked sub-assembly WebGL scenes centered in Car Render Space (`+Y` up, `+Z` forward, LDU).
-}
type alias DynamicSubMesh =
    { pivotLdu : Vec3
    , scene : Scene
    , colorHexSamples : List { r : Float, g : Float, b : Float }
    }


{-| Sub-assembly meshes for rendering chassis, articulated bogies, spinning wheels,
and cockpit steering wheel independently.
-}
type alias CarMeshBundle =
    { chassis : Scene
    , steeringBogie : Maybe DynamicSubMesh
    , steeringLeftKnuckle : Maybe DynamicSubMesh
    , steeringRightKnuckle : Maybe DynamicSubMesh
    , wheelMeshes : List { wheel : WheelSpec, scene : Scene }
    , steeringWheel : Maybe DynamicSubMesh
    , debrisColors : List { r : Float, g : Float, b : Float }
    , carBoundsRadiusLdu : Float
    }


{-| Converts LDraw `-Y` up space to WebGL `+Y` up (determinant `+1`).
-}
yUpFlip : Mat4
yUpFlip =
    Mat4.makeScale3 1 -1 -1


{-| Scale LEGO brick mass (grams) smoothly to realistic soapbox cart mass (kg).
-}
carWeightKgFromGrams : Float -> Float
carWeightKgFromGrams grams =
    clamp 12.0 160.0 (grams * 0.55)


{-| Seed missing or failed parts with clean procedural LDraw boxes/cylinders
from `LDraw.Connectivity` / `LDraw.Catalog` so cars render even offline.
-}
buildFallbackCache : Document -> PartCache -> PartCache
buildFallbackCache doc initialCache =
    let
        referencedParts =
            Document.flattenedPlacements doc
                |> List.map .part
                |> Set.fromList
                |> Set.toList
    in
    List.foldl
        (\partName cache ->
            case Dict.get partName cache of
                Just (Loaded _) ->
                    cache

                _ ->
                    case synthesizePartLines partName of
                        Just lines ->
                            Dict.insert partName (Loaded lines) cache

                        Nothing ->
                            cache
        )
        initialCache
        referencedParts


{-| Analyze a vehicle `Document` using procedural fallback meshes for any unloaded catalog parts.
-}
analyzeDocument : String -> Document -> PartCache -> PartIndex -> AnalyzedVehicle
analyzeDocument =
    analyzeDocumentWithMode UseProceduralFallbacks


{-| Analyze a vehicle `Document` with configurable fallback cache behavior.
-}
analyzeDocumentWithMode : FallbackCacheMode -> String -> Document -> PartCache -> PartIndex -> AnalyzedVehicle
analyzeDocumentWithMode mode title doc rawCache partIndex =
    let
        placements =
            Document.flattenedPlacements doc

        mpdSubfiles =
            Document.subfiles doc

        cacheWithSubfiles =
            List.foldl
                (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc)
                rawCache
                mpdSubfiles

        cache =
            case mode of
                UseProceduralFallbacks ->
                    buildFallbackCache doc cacheWithSubfiles

                StrictCacheOnly ->
                    cacheWithSubfiles

        missingCount =
            placements
                |> List.filter
                    (\p ->
                        case Dict.get p.part cache of
                            Just (Loaded _) ->
                                False

                            _ ->
                                True
                    )
                |> List.length

        partRecords =
            List.map (inspectPlacement cache partIndex) placements

        rawWheels =
            List.filterMap identity (List.map (classifyWheelPart partIndex) partRecords)

        clusteredWheels =
            pairConcentricWheels rawWheels

        ( modelToCarMatrix, wheelContactYLdu ) =
            computeNormalizationTransform placements partRecords clusteredWheels

        carRecords =
            List.map (retransformRecord modelToCarMatrix) partRecords

        carWheelsBeforeFront =
            List.map (transformWheelCluster modelToCarMatrix) clusteredWheels

        carWheels =
            assignFrontWheels carWheelsBeforeFront

        looseIds =
            findLoosePlacements carRecords carWheels

        steeringMech =
            detectSteeringMechanism carRecords carWheels

        ( wheelMeshesData, carWheelsWithSteer ) =
            tagSteerableWheels steeringMech carWheels

        wheelPlacementIdSet =
            carWheelsWithSteer
                |> List.concatMap .placementIds
                |> Set.fromList

        chassisNonWheelRecords =
            List.filter (\r -> not (Set.member r.placement.id wheelPlacementIdSet)) carRecords

        chassisMinYLdu =
            chassisNonWheelRecords
                |> List.map (\r -> Vec3.getY r.carMinCorner)
                |> List.minimum
                |> Maybe.withDefault 0.0

        boundsMinLdu =
            combinedMin carRecords

        boundsMaxLdu =
            combinedMax carRecords

        totalMassGrams =
            List.sum (List.map .massGrams carRecords)

        totalMassKg =
            carWeightKgFromGrams totalMassGrams

        comLdu =
            if totalMassGrams <= 0.001 then
                vec3 0 0 0

            else
                carRecords
                    |> List.foldl
                        (\r acc -> Vec3.add acc (Vec3.scale r.massGrams r.carCenter))
                        (vec3 0 0 0)
                    |> Vec3.scale (1.0 / totalMassGrams)

        comM =
            vec3LduToMeters comLdu

        ( wheelBaseM, trackWidthM ) =
            computeStanceMetrics carWheelsWithSteer

        ( frontalAreaM2, dragCoeff ) =
            computeAeroMetrics carRecords boundsMinLdu boundsMaxLdu

        inertia =
            computeInertia totalMassKg comLdu carRecords wheelBaseM trackWidthM

        steeringWheelRec =
            carRecords
                |> List.filter
                    (\r ->
                        r.cleanId
                            == "3829c01"
                            || r.cleanId
                            == "3829"
                            || r.cleanId
                            == "30663"
                            || r.cleanId
                            == "2819"
                    )
                |> List.head

        -- Keep combined steering stand+wheel units (3829c01/3829) bolted flush inside
        -- the rigid chassis so their rectangular base never tilts off the plate studs;
        -- only isolated rim parts spin independently.
        isolatedRimRec =
            carRecords
                |> List.filter (\r -> r.cleanId == "30663" || r.cleanId == "2819")
                |> List.head

        issues =
            buildValidationReport
                { placementCount = List.length placements
                , wheelCount = List.length carWheelsWithSteer
                , wheelBaseM = wheelBaseM
                , trackWidthM = trackWidthM
                , groundClearanceLdu = chassisMinYLdu
                , comM = comM
                , looseCount = List.length looseIds
                , missingCount = missingCount
                , steering = steeringMech
                , wheels = carWheelsWithSteer
                , dragCoeff = dragCoeff
                }

        isDrivable =
            not (List.any (\i -> i.severity == Blocker) issues)

        spec =
            { name = title
            , placementCount = List.length placements
            , legoMassGrams = totalMassGrams
            , totalMassKg = totalMassKg
            , centerOfMassLdu = comLdu
            , centerOfMassM = comM
            , boundsMinLdu = boundsMinLdu
            , boundsMaxLdu = boundsMaxLdu
            , wheelBaseM = wheelBaseM
            , trackWidthM = trackWidthM
            , groundClearanceLdu = chassisMinYLdu
            , groundClearanceM = lduToMeters chassisMinYLdu
            , wheelContactYLdu = wheelContactYLdu
            , frontalAreaM2 = frontalAreaM2
            , dragCoeff = dragCoeff
            , inertia = inertia
            , wheels = carWheelsWithSteer
            , steering = steeringMech
            , hasSteeringWheelIndicator = steeringWheelRec /= Nothing
            , steeringWheelPivotLdu = Maybe.map .carCenter isolatedRimRec
            , steeringWheelIds = Maybe.map (\r -> [ r.placement.id ]) isolatedRimRec |> Maybe.withDefault []
            , loosePlacementIds = looseIds
            , issues = issues
            , isDrivable = isDrivable
            , modelToCarMatrix = modelToCarMatrix
            }

        meshes =
            buildVehicleScenes cache spec placements carWheelsWithSteer wheelMeshesData
    in
    { spec = spec
    , meshes = meshes
    }


{-| Rebuild WebGL sub-assembly scenes (chassis, steering bogies/knuckles, wheels) for a validated vehicle.
-}
buildVehicleScenes :
    PartCache
    -> VehicleSpec
    -> List Placement
    -> List WheelSpec
    -> List WheelSpec
    -> CarMeshBundle
buildVehicleScenes cache spec placements wheels _ =
    let
        wheelIds =
            wheels
                |> List.concatMap .placementIds
                |> Set.fromList

        bogieIds =
            case spec.steering of
                TurntableBogie b ->
                    Set.fromList b.bogiePlacementIds

                TwinKnuckle k ->
                    Set.union (Set.fromList k.leftPlacementIds) (Set.fromList k.rightPlacementIds)

                FixedAxleFallback ->
                    Set.empty

        steeringWheelIdSet =
            Set.fromList spec.steeringWheelIds

        dynamicIds =
            wheelIds
                |> Set.union bogieIds
                |> Set.union steeringWheelIdSet

        chassisPlacements =
            List.filter (\p -> not (Set.member p.id dynamicIds)) placements

        chassisScene =
            flattenPlacementsInCarSpace cache spec.modelToCarMatrix chassisPlacements

        steeringBogie =
            case spec.steering of
                TurntableBogie b ->
                    let
                        partSet =
                            Set.fromList b.bogiePlacementIds
                                |> (\s -> Set.diff s wheelIds)

                        bogiePlacements =
                            List.filter (\p -> Set.member p.id partSet) placements
                    in
                    if List.isEmpty bogiePlacements then
                        Nothing

                    else
                        let
                            pivotOffset =
                                Mat4.makeTranslate (Vec3.negate b.pivotPosLdu)
                        in
                        Just
                            { pivotLdu = b.pivotPosLdu
                            , scene = flattenPlacementsWithPostMatrix cache (Mat4.mul pivotOffset spec.modelToCarMatrix) bogiePlacements
                            , colorHexSamples = sampleColors bogiePlacements
                            }

                _ ->
                    Nothing

        buildKnuckleMesh pivot ids =
            let
                idSet =
                    Set.diff (Set.fromList ids) wheelIds

                ps =
                    List.filter (\p -> Set.member p.id idSet) placements
            in
            if List.isEmpty ps then
                Nothing

            else
                Just
                    { pivotLdu = pivot
                    , scene = flattenPlacementsWithPostMatrix cache (Mat4.mul (Mat4.makeTranslate (Vec3.negate pivot)) spec.modelToCarMatrix) ps
                    , colorHexSamples = sampleColors ps
                    }

        ( leftKnuckle, rightKnuckle ) =
            case spec.steering of
                TwinKnuckle k ->
                    ( buildKnuckleMesh k.leftPivotLdu k.leftPlacementIds
                    , buildKnuckleMesh k.rightPivotLdu k.rightPlacementIds
                    )

                _ ->
                    ( Nothing, Nothing )

        steeringWheel =
            case spec.steeringWheelPivotLdu of
                Just pivot ->
                    let
                        swPlacements =
                            List.filter (\p -> Set.member p.id steeringWheelIdSet) placements
                    in
                    if List.isEmpty swPlacements then
                        Nothing

                    else
                        Just
                            { pivotLdu = pivot
                            , scene = flattenPlacementsWithPostMatrix cache (Mat4.mul (Mat4.makeTranslate (Vec3.negate pivot)) spec.modelToCarMatrix) swPlacements
                            , colorHexSamples = sampleColors swPlacements
                            }

                Nothing ->
                    Nothing

        wheelMeshes =
            List.map
                (\w ->
                    let
                        idSet =
                            Set.fromList w.placementIds

                        wPlacements =
                            List.filter (\p -> Set.member p.id idSet) placements

                        centerMat =
                            Mat4.mul (Mat4.makeTranslate (Vec3.negate w.localHubPosLdu)) spec.modelToCarMatrix
                    in
                    { wheel = w
                    , scene = flattenPlacementsWithPostMatrix cache centerMat wPlacements
                    }
                )
                wheels

        debrisColors =
            sampleColors placements

        extent =
            Vec3.sub spec.boundsMaxLdu spec.boundsMinLdu

        radiusLdu =
            max 20.0 (Vec3.length extent * 0.5)
    in
    { chassis = chassisScene
    , steeringBogie = steeringBogie
    , steeringLeftKnuckle = leftKnuckle
    , steeringRightKnuckle = rightKnuckle
    , wheelMeshes = wheelMeshes
    , steeringWheel = steeringWheel
    , debrisColors = debrisColors
    , carBoundsRadiusLdu = radiusLdu
    }



-- ── Internal Types ────────────────────────────────────────────────────────────


type alias PlacementRecord =
    { placement : Placement
    , cleanId : String
    , massGrams : Float
    , yUpMinCorner : Vec3
    , yUpMaxCorner : Vec3
    , yUpCenter : Vec3
    , carMinCorner : Vec3
    , carMaxCorner : Vec3
    , carCenter : Vec3
    , connectivity : Maybe PartConnectivity
    , hasGeometry : Bool
    }


type alias RawWheelHit =
    { placement : Placement
    , hubYUp : Vec3
    , spinAxisYUp : Vec3
    , radiusLdu : Float
    , widthLdu : Float
    , isTire : Bool
    , rimTireDiameter : Float
    }


type alias YUpWheelCluster =
    { id : Int
    , placementIds : List Int
    , hubYUp : Vec3
    , spinAxisYUp : Vec3
    , radiusLdu : Float
    , widthLdu : Float
    , hasRubberTire : Bool
    }


cleanPartName : String -> String
cleanPartName raw =
    Catalog.cleanPartId raw


inspectPlacement : PartCache -> PartIndex -> Placement -> PlacementRecord
inspectPlacement cache partIndex placement =
    let
        cleanId =
            cleanPartName placement.part

        maybeConn =
            Connectivity.knownCatalogPart placement.part

        geom =
            Geometry.flattenWith
                Geometry.worldOptions
                [ SubFileRef
                    { color = placement.color
                    , transform = placement.matrix
                    , file = placement.part
                    }
                ]
                cache
                15
                Mat4.identity

        hasGeom =
            not (List.isEmpty geom.triangles)

        ( yUpMin, yUpMax ) =
            if hasGeom then
                boundsOfTriangles geom.triangles

            else
                case maybeConn of
                    Just conn ->
                        boundsOfConnBoxes placement.matrix conn.boundingBoxes

                    Nothing ->
                        let
                            p =
                                transformPointYUp placement.matrix (vec3 0 0 0)
                        in
                        ( Vec3.sub p (vec3 10 12 10), Vec3.add p (vec3 10 12 10) )

        dims =
            Vec3.sub yUpMax yUpMin

        center =
            Vec3.scale 0.5 (Vec3.add yUpMin yUpMax)

        massGrams =
            case knownWheelOrExtraWeight cleanId of
                Just w ->
                    w

                Nothing ->
                    case Catalog.getPartInfo placement.part partIndex |> Maybe.andThen .unitWeightGrams of
                        Just w ->
                            w

                        Nothing ->
                            case Catalog.defaultPartWeight placement.part of
                                Just w ->
                                    w

                                Nothing ->
                                    Catalog.estimateWeightFromDimensions
                                        ( abs (Vec3.getX dims), abs (Vec3.getY dims), abs (Vec3.getZ dims) )
    in
    { placement = placement
    , cleanId = cleanId
    , massGrams = massGrams
    , yUpMinCorner = yUpMin
    , yUpMaxCorner = yUpMax
    , yUpCenter = center
    , carMinCorner = yUpMin
    , carMaxCorner = yUpMax
    , carCenter = center
    , connectivity = maybeConn
    , hasGeometry = hasGeom
    }


retransformRecord : Mat4 -> PlacementRecord -> PlacementRecord
retransformRecord mat r =
    let
        ( cMin, cMax ) =
            transformBox mat r.yUpMinCorner r.yUpMaxCorner
    in
    { r
        | carMinCorner = cMin
        , carMaxCorner = cMax
        , carCenter = Vec3.scale 0.5 (Vec3.add cMin cMax)
    }


classifyWheelPart : PartIndex -> PlacementRecord -> Maybe RawWheelHit
classifyWheelPart partIndex rec =
    if isNonWheelPartId rec.cleanId then
        Nothing

    else
        case rec.connectivity of
            Just conn ->
                -- Trust LDraw.Connectivity authoritatively so known wheel holders (4600),
                -- steering consoles (3829c01), brackets, and plates never trigger text heuristics.
                findWheelConn conn.connections
                    |> Maybe.map (buildConnWheelHit rec conn.boundingBoxes)

            Nothing ->
                classifyUnknownPartAsWheel partIndex rec


buildConnWheelHit : PlacementRecord -> List BoundingBox3D -> ( Bool, Float ) -> RawWheelHit
buildConnWheelHit rec boundingBoxes ( isTire, diam ) =
    let
        spinAxisLocalZ =
            normalizeSafe (transformDirYUp rec.placement.matrix (vec3 0 0 1)) (vec3 1 0 0)

        localBox =
            unionConnBoxes boundingBoxes

        rx =
            max (abs (Vec3.getX localBox.min)) (abs (Vec3.getX localBox.max))

        ry =
            max (abs (Vec3.getY localBox.min)) (abs (Vec3.getY localBox.max))

        r =
            max 8.0 (max rx ry)

        w =
            max 6.0 (Vec3.getZ localBox.max - Vec3.getZ localBox.min)
    in
    { placement = rec.placement
    , hubYUp = rec.yUpCenter
    , spinAxisYUp = spinAxisLocalZ
    , radiusLdu = r
    , widthLdu = w
    , isTire = isTire
    , rimTireDiameter = diam
    }


classifyUnknownPartAsWheel : PartIndex -> PlacementRecord -> Maybe RawWheelHit
classifyUnknownPartAsWheel partIndex rec =
    if isNonWheelPartId rec.cleanId then
        Nothing

    else
        let
            spinAxisLocalZ =
                normalizeSafe (transformDirYUp rec.placement.matrix (vec3 0 0 1)) (vec3 1 0 0)

            info =
                Catalog.getPartInfo rec.placement.part partIndex

            descText =
                info
                    |> Maybe.map (\i -> String.toLower (i.category ++ " " ++ i.description))
                    |> Maybe.withDefault ""

            idLower =
                String.toLower rec.cleanId

            hasExclusion =
                hasNonWheelDescriptionKeyword descText

            looksLikeTire =
                not hasExclusion
                    && (String.contains "tyre" descText
                            || String.contains "tire" descText
                       )

            looksLikeWheel =
                not hasExclusion
                    && (looksLikeTire
                            || String.contains "wheel" descText
                            || (String.contains "rim" descText
                                    && not (String.contains "trim" descText)
                                    && not (String.contains "primer" descText)
                               )
                       )
        in
        if looksLikeWheel then
            let
                ext =
                    Vec3.sub rec.yUpMaxCorner rec.yUpMinCorner

                ex =
                    abs (Vec3.getX ext)

                ey =
                    abs (Vec3.getY ext)

                ez =
                    abs (Vec3.getZ ext)

                -- Pick narrowest dimension as spin axis
                ( spinAx, rLdu, wLdu ) =
                    if ex <= ey && ex <= ez then
                        ( vec3 1 0 0, max 8.0 ((ey + ez) * 0.25), max 6.0 ex )

                    else if ez <= ex && ez <= ey then
                        ( vec3 0 0 1, max 8.0 ((ex + ey) * 0.25), max 6.0 ez )

                    else
                        ( spinAxisLocalZ, max 8.0 ((ex + ez) * 0.25), max 6.0 ey )
            in
            Just
                { placement = rec.placement
                , hubYUp = rec.yUpCenter
                , spinAxisYUp = spinAx
                , radiusLdu = rLdu
                , widthLdu = wLdu
                , isTire = looksLikeTire || String.contains "tyre" idLower
                , rimTireDiameter = rLdu * 1.4
                }

        else
            Nothing


isNonWheelPartId : String -> Bool
isNonWheelPartId cleanId =
    List.member cleanId
        [ "4600"
        , "4488"
        , "24326"
        , "30027a"
        , "30027b"
        , "3829"
        , "3829c01"
        , "30663"
        , "2819"
        , "73081"
        , "6157"
        , "2926"
        ]


hasNonWheelDescriptionKeyword : String -> Bool
hasNonWheelDescriptionKeyword lowerText =
    List.any (\kw -> String.contains kw lowerText)
        [ "holder"
        , "steering"
        , "mudguard"
        , "arch"
        , "fender"
        , "well"
        , "fork"
        , "fairing"
        , "bracket"
        , "turntable"
        , "suspension"
        , "wheelchair"
        , "wheelbarrow"
        ]


findWheelConn : List ConnectionPoint -> Maybe ( Bool, Float )
findWheelConn pts =
    case pts of
        [] ->
            Nothing

        first :: rest ->
            case first.connType of
                WheelTire { diameter } ->
                    Just ( True, diameter )

                WheelRim { diameter } ->
                    Just ( False, diameter )

                _ ->
                    findWheelConn rest


pairConcentricWheels : List RawWheelHit -> List YUpWheelCluster
pairConcentricWheels hits =
    let
        addHit hit clusters =
            case findMatchingCluster hit clusters of
                Nothing ->
                    { id = List.length clusters
                    , placementIds = [ hit.placement.id ]
                    , hubYUp = hit.hubYUp
                    , spinAxisYUp = hit.spinAxisYUp
                    , radiusLdu = hit.radiusLdu
                    , widthLdu = hit.widthLdu
                    , hasRubberTire = hit.isTire
                    }
                        :: clusters

                Just targetId ->
                    List.map
                        (\c ->
                            if c.id == targetId then
                                { c
                                    | placementIds = hit.placement.id :: c.placementIds
                                    , hubYUp = Vec3.scale 0.5 (Vec3.add c.hubYUp hit.hubYUp)
                                    , radiusLdu = max c.radiusLdu hit.radiusLdu
                                    , widthLdu = max c.widthLdu hit.widthLdu
                                    , hasRubberTire = c.hasRubberTire || hit.isTire
                                }

                            else
                                c
                        )
                        clusters

        findMatchingCluster hit clusters =
            clusters
                |> List.filter
                    (\c ->
                        Vec3.distance hit.hubYUp c.hubYUp
                            <= 8.0
                            && abs (Vec3.dot hit.spinAxisYUp c.spinAxisYUp)
                            >= 0.92
                    )
                |> List.head
                |> Maybe.map .id
    in
    List.foldl addHit [] hits
        |> List.reverse
        |> List.indexedMap (\i c -> { c | id = i })


computeNormalizationTransform : List Placement -> List PlacementRecord -> List YUpWheelCluster -> ( Mat4, Float )
computeNormalizationTransform _ records wheels =
    let
        yawAngle =
            determineForwardYaw records wheels

        rotY =
            Mat4.makeRotate yawAngle (vec3 0 1 0)

        rotatedRecords =
            List.map (retransformRecord rotY) records

        rotatedWheels =
            List.map
                (\w ->
                    { w | hubYUp = Mat4.transform rotY w.hubYUp }
                )
                wheels

        wheelContactY =
            case rotatedWheels of
                [] ->
                    rotatedRecords
                        |> List.map (\r -> Vec3.getY r.carMinCorner)
                        |> List.minimum
                        |> Maybe.withDefault 0.0

                _ ->
                    rotatedWheels
                        |> List.map (\w -> Vec3.getY w.hubYUp - w.radiusLdu)
                        |> List.minimum
                        |> Maybe.withDefault 0.0

        ( centerX, centerZ ) =
            let
                totalMass =
                    List.sum (List.map .massGrams rotatedRecords)
            in
            if totalMass > 0.001 then
                let
                    sumVec =
                        List.foldl
                            (\r acc -> Vec3.add acc (Vec3.scale r.massGrams r.carCenter))
                            (vec3 0 0 0)
                            rotatedRecords
                            |> Vec3.scale (1.0 / totalMass)
                in
                ( Vec3.getX sumVec, Vec3.getZ sumVec )

            else
                ( 0.0, 0.0 )

        translate =
            Mat4.makeTranslate (vec3 -centerX -wheelContactY -centerZ)
    in
    ( Mat4.mul translate rotY, wheelContactY )


determineForwardYaw : List PlacementRecord -> List YUpWheelCluster -> Float
determineForwardYaw records wheels =
    let
        wheelCentroid =
            if List.isEmpty wheels then
                vec3 0 0 0

            else
                wheels
                    |> List.foldl (\w acc -> Vec3.add acc w.hubYUp) (vec3 0 0 0)
                    |> Vec3.scale (1.0 / toFloat (List.length wheels))

        steeringMarkers =
            List.filter
                (\r ->
                    isTurntablePart r.cleanId
                        || r.cleanId
                        == "3829c01"
                        || r.cleanId
                        == "3829"
                )
                records

        allMin =
            List.foldl
                (\r acc ->
                    vec3
                        (min (Vec3.getX acc) (Vec3.getX r.yUpMinCorner))
                        0
                        (min (Vec3.getZ acc) (Vec3.getZ r.yUpMinCorner))
                )
                (vec3 1.0e9 0 1.0e9)
                records

        allMax =
            List.foldl
                (\r acc ->
                    vec3
                        (max (Vec3.getX acc) (Vec3.getX r.yUpMaxCorner))
                        0
                        (max (Vec3.getZ acc) (Vec3.getZ r.yUpMaxCorner))
                )
                (vec3 -1.0e9 0 -1.0e9)
                records

        spanX =
            Vec3.getX allMax - Vec3.getX allMin

        spanZ =
            Vec3.getZ allMax - Vec3.getZ allMin
    in
    case steeringMarkers of
        m :: _ ->
            let
                delta =
                    Vec3.sub m.yUpCenter wheelCentroid

                dx =
                    Vec3.getX delta

                dz =
                    Vec3.getZ delta
            in
            if abs dz >= abs dx then
                if dz >= -1.0 then
                    0.0

                else
                    pi

            else if dx >= 0 then
                -pi / 2

            else
                pi / 2

        [] ->
            let
                seatMarker =
                    List.filter (\r -> r.cleanId == "4079") records |> List.head
            in
            case seatMarker of
                Just seat ->
                    let
                        localForwardYUp =
                            normalizeSafe (transformDirYUp seat.placement.matrix (vec3 0 0 -1)) (vec3 0 0 1)

                        fx =
                            Vec3.getX localForwardYUp

                        fz =
                            Vec3.getZ localForwardYUp
                    in
                    if abs fz >= abs fx then
                        if fz >= 0 then
                            0.0

                        else
                            pi

                    else if fx > 0 then
                        -pi / 2

                    else
                        pi / 2

                Nothing ->
                    if spanX > spanZ * 1.15 then
                        pi / 2

                    else
                        0.0


transformWheelCluster : Mat4 -> YUpWheelCluster -> WheelSpec
transformWheelCluster modelToCar c =
    let
        hubCar =
            Mat4.transform modelToCar c.hubYUp

        spinCar =
            normalizeSafe (transformVecOnly modelToCar c.spinAxisYUp) (vec3 1 0 0)
    in
    { id = c.id
    , placementIds = c.placementIds
    , localHubPosLdu = hubCar
    , localHubPosM = vec3LduToMeters hubCar
    , spinAxis = spinCar
    , radiusLdu = c.radiusLdu
    , radiusM = lduToMeters c.radiusLdu
    , widthLdu = c.widthLdu
    , widthM = lduToMeters c.widthLdu
    , isFront = False
    , isSteerable = False
    , hasRubberTire = c.hasRubberTire
    }


assignFrontWheels : List WheelSpec -> List WheelSpec
assignFrontWheels wheels =
    case wheels of
        [] ->
            []

        _ ->
            let
                zValues =
                    List.map (\w -> Vec3.getZ w.localHubPosLdu) wheels

                minZ =
                    List.minimum zValues |> Maybe.withDefault 0

                maxZ =
                    List.maximum zValues |> Maybe.withDefault 0

                midZ =
                    (minZ + maxZ) * 0.5
            in
            List.map
                (\w ->
                    { w | isFront = Vec3.getZ w.localHubPosLdu >= midZ - 4.0 && maxZ - minZ > 8.0 }
                )
                wheels


detectSteeringMechanism : List PlacementRecord -> List WheelSpec -> SteeringMechanism
detectSteeringMechanism records wheels =
    let
        turntables =
            List.filter (\r -> isTurntablePart r.cleanId) records

        frontWheelCenterZ =
            wheels
                |> List.filter .isFront
                |> List.map (\w -> Vec3.getZ w.localHubPosLdu)
                |> List.maximum
                |> Maybe.withDefault 20.0

        frontTurntable =
            turntables
                |> List.filter (\r -> Vec3.getZ r.carCenter >= 0.0 || abs (Vec3.getZ r.carCenter - frontWheelCenterZ) < 60.0)
                |> List.sortBy (\r -> abs (Vec3.getZ r.carCenter - frontWheelCenterZ))
                |> List.head
    in
    case frontTurntable of
        Just tt ->
            let
                bogieIds =
                    collectBogieIdsForTurntable tt records wheels
            in
            TurntableBogie
                { pivotPosLdu = tt.carCenter
                , pivotPosM = vec3LduToMeters tt.carCenter
                , bogiePlacementIds = bogieIds
                }

        Nothing ->
            let
                hinges =
                    List.filter (\r -> isKnuckleHingePart r.cleanId && Vec3.getZ r.carCenter > 0.0) records
                        |> List.sortBy (\r -> Vec3.getX r.carCenter)
            in
            case hinges of
                left :: rest ->
                    case List.reverse rest |> List.head of
                        Just right ->
                            if Vec3.getX right.carCenter - Vec3.getX left.carCenter > 15.0 then
                                TwinKnuckle
                                    { leftPivotLdu = left.carCenter
                                    , rightPivotLdu = right.carCenter
                                    , leftPlacementIds = collectNearbyIds left.carCenter records wheels
                                    , rightPlacementIds = collectNearbyIds right.carCenter records wheels
                                    }

                            else
                                FixedAxleFallback

                        Nothing ->
                            FixedAxleFallback

                [] ->
                    FixedAxleFallback


collectBogieIdsForTurntable : PlacementRecord -> List PlacementRecord -> List WheelSpec -> List Int
collectBogieIdsForTurntable tt records wheels =
    let
        turntableZ =
            Vec3.getZ tt.carCenter

        ttBottomY =
            Vec3.getY tt.carMinCorner

        frontWheelsIds =
            wheels
                |> List.filter (\w -> w.isFront || abs (Vec3.getZ w.localHubPosLdu - turntableZ) < 40.0)
                |> List.concatMap .placementIds

        attachedAxleParts =
            records
                |> List.filter
                    (\r ->
                        r.placement.id
                            /= tt.placement.id
                            && not (isTurntablePart r.cleanId)
                            && abs (Vec3.getZ r.carCenter - turntableZ)
                            < 36.0
                            && Vec3.getY r.carCenter
                            <= ttBottomY
                            + 16.0
                            && (r.cleanId == "4600" || r.cleanId == "4488" || r.cleanId == "24326" || List.member r.placement.id frontWheelsIds)
                    )
                |> List.map .placement
                |> List.map .id

        ttBaseIds =
            records
                |> List.filter (\r -> (r.cleanId == "3680" || r.cleanId == "99009") && Vec3.distance r.carCenter tt.carCenter < 20.0)
                |> List.map (.placement >> .id)
    in
    Set.fromList (ttBaseIds ++ attachedAxleParts ++ frontWheelsIds)
        |> Set.toList


collectNearbyIds : Vec3 -> List PlacementRecord -> List WheelSpec -> List Int
collectNearbyIds pivot records wheels =
    let
        nearRecords =
            records
                |> List.filter (\r -> Vec3.distance r.carCenter pivot < 45.0)
                |> List.map (.placement >> .id)

        nearWheels =
            wheels
                |> List.filter (\w -> Vec3.distance w.localHubPosLdu pivot < 55.0)
                |> List.concatMap .placementIds
    in
    Set.fromList (nearRecords ++ nearWheels)
        |> Set.toList


tagSteerableWheels : SteeringMechanism -> List WheelSpec -> ( List WheelSpec, List WheelSpec )
tagSteerableWheels mech wheels =
    let
        steerableIds =
            case mech of
                TurntableBogie b ->
                    Set.fromList b.bogiePlacementIds

                TwinKnuckle k ->
                    Set.union (Set.fromList k.leftPlacementIds) (Set.fromList k.rightPlacementIds)

                FixedAxleFallback ->
                    Set.empty

        updated =
            List.map
                (\w ->
                    let
                        isPivotMember =
                            List.any (\pid -> Set.member pid steerableIds) w.placementIds

                        steer =
                            case mech of
                                FixedAxleFallback ->
                                    False

                                _ ->
                                    w.isFront || isPivotMember
                    in
                    { w | isSteerable = steer }
                )
                wheels
    in
    ( updated, updated )


findLoosePlacements : List PlacementRecord -> List WheelSpec -> List Int
findLoosePlacements records _ =
    case records of
        [] ->
            []

        _ :: [] ->
            []

        _ ->
            let
                adj : Dict Int (List Int)
                adj =
                    buildConnectivityGraph records

                anchorId =
                    records
                        |> List.sortBy (\r -> -(Vec3.getX (Vec3.sub r.carMaxCorner r.carMinCorner) * Vec3.getZ (Vec3.sub r.carMaxCorner r.carMinCorner)))
                        |> List.head
                        |> Maybe.map (.placement >> .id)
                        |> Maybe.withDefault 0

                visited =
                    bfsConnected anchorId adj Set.empty [ anchorId ]
            in
            records
                |> List.filter (\r -> not (Set.member r.placement.id visited))
                |> List.map (.placement >> .id)


buildConnectivityGraph : List PlacementRecord -> Dict Int (List Int)
buildConnectivityGraph records =
    let
        pairs =
            collectClosePairs records []
    in
    List.foldl
        (\( a, b ) dict ->
            dict
                |> Dict.update a (\v -> Just (b :: Maybe.withDefault [] v))
                |> Dict.update b (\v -> Just (a :: Maybe.withDefault [] v))
        )
        Dict.empty
        pairs


collectClosePairs : List PlacementRecord -> List ( Int, Int ) -> List ( Int, Int )
collectClosePairs remaining acc =
    case remaining of
        [] ->
            acc

        first :: rest ->
            let
                matches =
                    List.filterMap
                        (\other ->
                            if boxesTouchOrOverlap 9.5 first.carMinCorner first.carMaxCorner other.carMinCorner other.carMaxCorner then
                                Just ( first.placement.id, other.placement.id )

                            else
                                Nothing
                        )
                        rest
            in
            collectClosePairs rest (matches ++ acc)


boxesTouchOrOverlap : Float -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Bool
boxesTouchOrOverlap tol minA maxA minB maxB =
    (Vec3.getX minA <= Vec3.getX maxB + tol && Vec3.getX maxA + tol >= Vec3.getX minB)
        && (Vec3.getY minA <= Vec3.getY maxB + tol && Vec3.getY maxA + tol >= Vec3.getY minB)
        && (Vec3.getZ minA <= Vec3.getZ maxB + tol && Vec3.getZ maxA + tol >= Vec3.getZ minB)


bfsConnected : Int -> Dict Int (List Int) -> Set Int -> List Int -> Set Int
bfsConnected _ adj visited queue =
    case queue of
        [] ->
            visited

        curr :: rest ->
            if Set.member curr visited then
                bfsConnected curr adj visited rest

            else
                let
                    neighbors =
                        Dict.get curr adj |> Maybe.withDefault []

                    nextVisited =
                        Set.insert curr visited

                    nextQueue =
                        rest ++ List.filter (\n -> not (Set.member n nextVisited)) neighbors
                in
                bfsConnected curr adj nextVisited nextQueue


computeStanceMetrics : List WheelSpec -> ( Float, Float )
computeStanceMetrics wheels =
    case wheels of
        [] ->
            ( 0.0, 0.0 )

        _ :: [] ->
            ( 0.0, 0.0 )

        _ ->
            let
                xs =
                    List.map (\w -> Vec3.getX w.localHubPosM) wheels

                zs =
                    List.map (\w -> Vec3.getZ w.localHubPosM) wheels

                minX =
                    List.minimum xs |> Maybe.withDefault 0

                maxX =
                    List.maximum xs |> Maybe.withDefault 0

                minZ =
                    List.minimum zs |> Maybe.withDefault 0

                maxZ =
                    List.maximum zs |> Maybe.withDefault 0
            in
            ( maxZ - minZ, maxX - minX )


computeAeroMetrics : List PlacementRecord -> Vec3 -> Vec3 -> ( Float, Float )
computeAeroMetrics records boundsMinLdu boundsMaxLdu =
    let
        widthM =
            max 0.2 (lduToMeters (Vec3.getX boundsMaxLdu - Vec3.getX boundsMinLdu))

        heightM =
            max 0.15 (lduToMeters (Vec3.getY boundsMaxLdu - Vec3.getY boundsMinLdu))

        projectedArea =
            clamp 0.12 1.8 (widthM * heightM * 0.72)

        slopeCount =
            records
                |> List.filter (\r -> isAeroSlopePart r.cleanId)
                |> List.length

        totalCount =
            max 1 (List.length records)

        slopeRatio =
            toFloat slopeCount / toFloat totalCount

        dragCoeff =
            clamp 0.52 0.96 (0.86 - slopeRatio * 0.65)
    in
    ( projectedArea, dragCoeff )


computeInertia : Float -> Vec3 -> List PlacementRecord -> Float -> Float -> { yaw : Float, roll : Float, pitch : Float }
computeInertia totalMassKg comLdu records wheelBaseM trackWidthM =
    let
        totalGrams =
            max 1.0 (List.sum (List.map .massGrams records))

        scaleFactor =
            totalMassKg / totalGrams

        sumMoments =
            List.foldl
                (\r acc ->
                    let
                        dx =
                            lduToMeters (Vec3.getX r.carCenter - Vec3.getX comLdu)

                        dy =
                            lduToMeters (Vec3.getY r.carCenter - Vec3.getY comLdu)

                        dz =
                            lduToMeters (Vec3.getZ r.carCenter - Vec3.getZ comLdu)

                        m =
                            r.massGrams * scaleFactor
                    in
                    { yaw = acc.yaw + m * (dx * dx + dz * dz)
                    , roll = acc.roll + m * (dx * dx + dy * dy)
                    , pitch = acc.pitch + m * (dz * dz + dy * dy)
                    }
                )
                { yaw = 0, roll = 0, pitch = 0 }
                records

        minYaw =
            totalMassKg * (max 0.35 wheelBaseM ^ 2 + max 0.35 trackWidthM ^ 2) * 0.16

        minRoll =
            totalMassKg * (max 0.35 trackWidthM ^ 2) * 0.25

        minPitch =
            totalMassKg * (max 0.35 wheelBaseM ^ 2) * 0.25
    in
    { yaw = max minYaw sumMoments.yaw
    , roll = max minRoll sumMoments.roll
    , pitch = max minPitch sumMoments.pitch
    }


buildValidationReport :
    { placementCount : Int
    , wheelCount : Int
    , wheelBaseM : Float
    , trackWidthM : Float
    , groundClearanceLdu : Float
    , comM : Vec3
    , looseCount : Int
    , missingCount : Int
    , steering : SteeringMechanism
    , wheels : List WheelSpec
    , dragCoeff : Float
    }
    -> List ValidationIssue
buildValidationReport ctx =
    let
        blockerEmpty =
            if ctx.placementCount == 0 then
                [ { severity = Blocker
                  , code = EmptyModel
                  , detail = "Model has no placed bricks."
                  }
                ]

            else
                []

        blockerWheels =
            if ctx.placementCount > 0 && ctx.wheelCount < 3 then
                [ { severity = Blocker
                  , code = TooFewWheels ctx.wheelCount
                  , detail = "Need at least 3 wheels to balance and steer down the hill."
                  }
                ]

            else
                []

        blockerStance =
            if ctx.wheelCount >= 3 && (ctx.wheelBaseM < 0.1 || ctx.trackWidthM < 0.1) then
                [ { severity = Blocker
                  , code = CollinearStance
                  , detail = "Wheels are aligned on one line; add front/rear axle spread."
                  }
                ]

            else
                []

        warningClearance =
            if ctx.wheelCount >= 3 && ctx.groundClearanceLdu < -0.5 then
                [ { severity = Warning
                  , code = BellyScrape ctx.groundClearanceLdu
                  , detail = "Chassis drags below tires (-" ++ String.fromFloat (abs ctx.groundClearanceLdu) ++ " LDU)."
                  }
                ]

            else
                []

        tippyRatio =
            Vec3.getY ctx.comM / (0.5 * max 0.25 ctx.trackWidthM)

        warningCoM =
            if ctx.wheelCount >= 3 && tippyRatio > 1.35 then
                [ { severity = Warning
                  , code = HighCenterOfMass tippyRatio
                  , detail = "High center of gravity — shift weight into corners to avoid tipping."
                  }
                ]

            else
                []

        warningLoose =
            if ctx.looseCount > 0 then
                [ { severity = Warning
                  , code = DisconnectedBricks ctx.looseCount
                  , detail = String.fromInt ctx.looseCount ++ " loose brick(s) not attached to main chassis."
                  }
                ]

            else
                []

        warningMissing =
            if ctx.missingCount > 0 then
                [ { severity = Warning
                  , code = MissingPartGeometry ctx.missingCount
                  , detail = String.fromInt ctx.missingCount ++ " part file(s) using placeholder boxes."
                  }
                ]

            else
                []

        steeringNote =
            case ctx.steering of
                FixedAxleFallback ->
                    [ { severity = Info
                      , code = FixedAxleTip
                      , detail = "Fixed axles detected — using Soapbox Weight-Shift virtual steering."
                      }
                    ]

                TurntableBogie _ ->
                    [ { severity = Bonus
                      , code = RealSteeringBonus
                      , detail = "Turntable steering bogie detected (+25% turn-in agility)."
                      }
                    ]

                TwinKnuckle _ ->
                    [ { severity = Bonus
                      , code = RealSteeringBonus
                      , detail = "Dual-knuckle front steering detected (+25% turn-in agility)."
                      }
                    ]

        tireBonus =
            if List.any .hasRubberTire ctx.wheels then
                [ { severity = Bonus
                  , code = RubberGripBonus
                  , detail = "Rubber tires equipped for full lateral cornering grip."
                  }
                ]

            else
                []

        aeroBonus =
            if ctx.dragCoeff < 0.78 then
                [ { severity = Bonus
                  , code = AeroWedgeBonus
                  , detail = "Sloped bodywork lowers aerodynamic drag."
                  }
                ]

            else
                []
    in
    blockerEmpty
        ++ blockerWheels
        ++ blockerStance
        ++ warningClearance
        ++ warningCoM
        ++ warningLoose
        ++ warningMissing
        ++ steeringNote
        ++ tireBonus
        ++ aeroBonus



-- ── Flattening & Geometry Helpers ─────────────────────────────────────────────


flattenPlacementsInCarSpace : PartCache -> Mat4 -> List Placement -> Scene
flattenPlacementsInCarSpace cache carFromYUp placements =
    flattenPlacementsWithPostMatrix cache carFromYUp placements


flattenPlacementsWithPostMatrix : PartCache -> Mat4 -> List Placement -> Scene
flattenPlacementsWithPostMatrix cache carFromYUp placements =
    let
        lines =
            List.map
                (\p ->
                    SubFileRef
                        { color = p.color
                        , transform = p.matrix
                        , file = p.part
                        }
                )
                placements

        rawGeom =
            Geometry.flattenWith Geometry.worldOptions lines cache 15 Mat4.identity

        transformVertex v =
            { position = Mat4.transform carFromYUp v.position
            , normal = normalizeSafe (transformVecOnly carFromYUp v.normal) (vec3 0 1 0)
            , color = v.color
            , inherit = v.inherit
            }

        transformedTris =
            List.map
                (\( a, b, c ) -> ( transformVertex a, transformVertex b, transformVertex c ))
                rawGeom.triangles

        transformedLines =
            List.map
                (\( p1, p2 ) -> ( Mat4.transform carFromYUp p1, Mat4.transform carFromYUp p2 ))
                rawGeom.lines

        transformedCond =
            List.map
                (\e ->
                    { p1 = Mat4.transform carFromYUp e.p1
                    , p2 = Mat4.transform carFromYUp e.p2
                    , c1 = Mat4.transform carFromYUp e.c1
                    , c2 = Mat4.transform carFromYUp e.c2
                    }
                )
                rawGeom.conditionalLines
    in
    Scene.buildScene
        { triangles = transformedTris
        , lines = transformedLines
        , conditionalLines = transformedCond
        }
        False


sampleColors : List Placement -> List { r : Float, g : Float, b : Float }
sampleColors placements =
    placements
        |> List.filterMap
            (\p ->
                if p.color == 16 || p.color == 24 then
                    Nothing

                else
                    let
                        c =
                            Colors.resolveColor 15 p.color
                    in
                    Just { r = c.r, g = c.g, b = c.b }
            )
        |> (\list ->
                if List.isEmpty list then
                    [ { r = 0.85, g = 0.16, b = 0.16 }
                    , { r = 0.96, g = 0.78, b = 0.1 }
                    ]

                else
                    list
           )


boundsOfTriangles : List ( { b | position : Vec3 }, { b | position : Vec3 }, { b | position : Vec3 } ) -> ( Vec3, Vec3 )
boundsOfTriangles tris =
    let
        step v acc =
            let
                x =
                    Vec3.getX v.position

                y =
                    Vec3.getY v.position

                z =
                    Vec3.getZ v.position
            in
            { minX = min acc.minX x
            , minY = min acc.minY y
            , minZ = min acc.minZ z
            , maxX = max acc.maxX x
            , maxY = max acc.maxY y
            , maxZ = max acc.maxZ z
            }

        initAcc =
            { minX = 1.0e9
            , minY = 1.0e9
            , minZ = 1.0e9
            , maxX = -1.0e9
            , maxY = -1.0e9
            , maxZ = -1.0e9
            }

        finalAcc =
            List.foldl (\( a, b, c ) acc -> step c (step b (step a acc))) initAcc tris
    in
    if finalAcc.minX > finalAcc.maxX then
        ( vec3 0 0 0, vec3 0 0 0 )

    else
        ( vec3 finalAcc.minX finalAcc.minY finalAcc.minZ
        , vec3 finalAcc.maxX finalAcc.maxY finalAcc.maxZ
        )


boundsOfConnBoxes : Mat4 -> List BoundingBox3D -> ( Vec3, Vec3 )
boundsOfConnBoxes ldrawMat boxes =
    let
        unionB =
            unionConnBoxes boxes

        pts =
            boxCorners unionB.min unionB.max
                |> List.map (transformPointYUp ldrawMat)
    in
    case pts of
        [] ->
            ( vec3 0 0 0, vec3 0 0 0 )

        p0 :: rest ->
            List.foldl
                (\p ( lo, hi ) ->
                    ( vec3 (min (Vec3.getX lo) (Vec3.getX p)) (min (Vec3.getY lo) (Vec3.getY p)) (min (Vec3.getZ lo) (Vec3.getZ p))
                    , vec3 (max (Vec3.getX hi) (Vec3.getX p)) (max (Vec3.getY hi) (Vec3.getY p)) (max (Vec3.getZ hi) (Vec3.getZ p))
                    )
                )
                ( p0, p0 )
                rest


unionConnBoxes : List BoundingBox3D -> BoundingBox3D
unionConnBoxes boxes =
    case boxes of
        [] ->
            { min = vec3 -10 -12 -10, max = vec3 10 12 10 }

        b0 :: rest ->
            List.foldl
                (\b acc ->
                    { min =
                        vec3
                            (min (Vec3.getX acc.min) (Vec3.getX b.min))
                            (min (Vec3.getY acc.min) (Vec3.getY b.min))
                            (min (Vec3.getZ acc.min) (Vec3.getZ b.min))
                    , max =
                        vec3
                            (max (Vec3.getX acc.max) (Vec3.getX b.max))
                            (max (Vec3.getY acc.max) (Vec3.getY b.max))
                            (max (Vec3.getZ acc.max) (Vec3.getZ b.max))
                    }
                )
                b0
                rest


transformPointYUp : Mat4 -> Vec3 -> Vec3
transformPointYUp ldrawMat pt =
    let
        tp =
            Mat4.transform ldrawMat pt
    in
    vec3 (Vec3.getX tp) -(Vec3.getY tp) -(Vec3.getZ tp)


transformDirYUp : Mat4 -> Vec3 -> Vec3
transformDirYUp ldrawMat dir =
    let
        origin =
            transformPointYUp ldrawMat (vec3 0 0 0)

        tip =
            transformPointYUp ldrawMat dir
    in
    Vec3.sub tip origin


transformVecOnly : Mat4 -> Vec3 -> Vec3
transformVecOnly mat dir =
    let
        origin =
            Mat4.transform mat (vec3 0 0 0)

        tip =
            Mat4.transform mat dir
    in
    Vec3.sub tip origin


transformBox : Mat4 -> Vec3 -> Vec3 -> ( Vec3, Vec3 )
transformBox mat lo hi =
    let
        pts =
            boxCorners lo hi |> List.map (Mat4.transform mat)
    in
    case pts of
        [] ->
            ( lo, hi )

        p0 :: rest ->
            List.foldl
                (\p ( mn, mx ) ->
                    ( vec3 (min (Vec3.getX mn) (Vec3.getX p)) (min (Vec3.getY mn) (Vec3.getY p)) (min (Vec3.getZ mn) (Vec3.getZ p))
                    , vec3 (max (Vec3.getX mx) (Vec3.getX p)) (max (Vec3.getY mx) (Vec3.getY p)) (max (Vec3.getZ mx) (Vec3.getZ p))
                    )
                )
                ( p0, p0 )
                rest


boxCorners : Vec3 -> Vec3 -> List Vec3
boxCorners lo hi =
    let
        ( x0, y0, z0 ) =
            ( Vec3.getX lo, Vec3.getY lo, Vec3.getZ lo )

        ( x1, y1, z1 ) =
            ( Vec3.getX hi, Vec3.getY hi, Vec3.getZ hi )
    in
    [ vec3 x0 y0 z0
    , vec3 x1 y0 z0
    , vec3 x0 y1 z0
    , vec3 x1 y1 z0
    , vec3 x0 y0 z1
    , vec3 x1 y0 z1
    , vec3 x0 y1 z1
    , vec3 x1 y1 z1
    ]


combinedMin : List PlacementRecord -> Vec3
combinedMin records =
    case records of
        [] ->
            vec3 0 0 0

        r0 :: rest ->
            List.foldl
                (\r acc ->
                    vec3
                        (min (Vec3.getX acc) (Vec3.getX r.carMinCorner))
                        (min (Vec3.getY acc) (Vec3.getY r.carMinCorner))
                        (min (Vec3.getZ acc) (Vec3.getZ r.carMinCorner))
                )
                r0.carMinCorner
                rest


combinedMax : List PlacementRecord -> Vec3
combinedMax records =
    case records of
        [] ->
            vec3 0 0 0

        r0 :: rest ->
            List.foldl
                (\r acc ->
                    vec3
                        (max (Vec3.getX acc) (Vec3.getX r.carMaxCorner))
                        (max (Vec3.getY acc) (Vec3.getY r.carMaxCorner))
                        (max (Vec3.getZ acc) (Vec3.getZ r.carMaxCorner))
                )
                r0.carMaxCorner
                rest


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


isTurntablePart : String -> Bool
isTurntablePart cleanId =
    List.member cleanId [ "3679", "3680", "99009", "99010", "18938", "48452", "48168" ]


isKnuckleHingePart : String -> Bool
isKnuckleHingePart cleanId =
    List.member cleanId [ "2429", "2430", "4275b", "4276b", "14417", "14418", "2736" ]


isAeroSlopePart : String -> Bool
isAeroSlopePart cleanId =
    List.member cleanId [ "3040", "3040a", "3040b", "3039", "3038", "3037", "3298", "54200", "50950", "11477", "24201" ]


knownWheelOrExtraWeight : String -> Maybe Float
knownWheelOrExtraWeight cleanId =
    case cleanId of
        "4624" ->
            Just 1.2

        "3641" ->
            Just 2.8

        "6014" ->
            Just 2.4

        "6014b" ->
            Just 2.4

        "6015" ->
            Just 4.5

        "56890" ->
            Just 6.4

        "55981" ->
            Just 5.2

        "55982" ->
            Just 5.2

        "56891" ->
            Just 11.5

        "56145" ->
            Just 14.0

        "44309" ->
            Just 28.0

        "4600" ->
            Just 1.4

        "4488" ->
            Just 1.1

        "24326" ->
            Just 1.2

        "3679" ->
            Just 0.6

        "3680" ->
            Just 0.7

        "99009" ->
            Just 2.6

        "99010" ->
            Just 2.4

        "3829c01" ->
            Just 1.3

        "4079" ->
            Just 1.5

        "3034" ->
            Just 3.1

        "3039" ->
            Just 1.1

        "3040" ->
            Just 0.6

        _ ->
            Nothing



-- ── Offline Procedural Fallback Meshes ────────────────────────────────────────


synthesizePartLines : String -> Maybe (List LDrawLine)
synthesizePartLines rawPart =
    let
        cleanId =
            cleanPartName rawPart
    in
    case Connectivity.knownCatalogPart rawPart of
        Just conn ->
            case findWheelConn conn.connections of
                Just ( isTire, _ ) ->
                    let
                        bbox =
                            unionConnBoxes conn.boundingBoxes

                        r =
                            max (abs (Vec3.getX bbox.max)) (abs (Vec3.getY bbox.max))

                        z0 =
                            Vec3.getZ bbox.min

                        z1 =
                            Vec3.getZ bbox.max
                    in
                    Just (buildCylinderLines 16 r z0 z1 isTire)

                Nothing ->
                    if cleanId == "3829c01" || cleanId == "3829" then
                        Just (buildSteeringWheelLines ++ List.concatMap boxLines conn.boundingBoxes)

                    else
                        Just (List.concatMap boxLines conn.boundingBoxes)

        Nothing ->
            Nothing


boxLines : BoundingBox3D -> List LDrawLine
boxLines box =
    let
        x0 =
            Vec3.getX box.min

        y0 =
            Vec3.getY box.min

        z0 =
            Vec3.getZ box.min

        x1 =
            Vec3.getX box.max

        y1 =
            Vec3.getY box.max

        z1 =
            Vec3.getZ box.max

        q a b c d =
            Quad { color = 16, p1 = a, p2 = b, p3 = c, p4 = d }

        e a b =
            LineSegment { color = 24, p1 = a, p2 = b }

        p000 =
            vec3 x0 y0 z0

        p100 =
            vec3 x1 y0 z0

        p010 =
            vec3 x0 y1 z0

        p110 =
            vec3 x1 y1 z0

        p001 =
            vec3 x0 y0 z1

        p101 =
            vec3 x1 y0 z1

        p011 =
            vec3 x0 y1 z1

        p111 =
            vec3 x1 y1 z1
    in
    [ q p001 p101 p111 p011
    , q p100 p000 p010 p110
    , q p000 p100 p101 p001
    , q p110 p010 p011 p111
    , q p100 p110 p111 p101
    , q p010 p000 p001 p011
    , e p000 p100
    , e p100 p110
    , e p110 p010
    , e p010 p000
    , e p001 p101
    , e p101 p111
    , e p111 p011
    , e p011 p001
    , e p000 p001
    , e p100 p101
    , e p110 p111
    , e p010 p011
    ]


buildCylinderLines : Int -> Float -> Float -> Float -> Bool -> List LDrawLine
buildCylinderLines segments outerR z0 z1 isTire =
    let
        innerR =
            if isTire then
                outerR * 0.58

            else
                outerR * 0.25

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

                ox0 =
                    outerR * c0

                oy0 =
                    outerR * s0

                ox1 =
                    outerR * c1

                oy1 =
                    outerR * s1

                ix0 =
                    innerR * c0

                iy0 =
                    innerR * s0

                ix1 =
                    innerR * c1

                iy1 =
                    innerR * s1

                tread =
                    Quad { color = 16, p1 = vec3 ox0 oy0 z0, p2 = vec3 ox1 oy1 z0, p3 = vec3 ox1 oy1 z1, p4 = vec3 ox0 oy0 z1 }

                capFront =
                    Quad { color = 16, p1 = vec3 ix0 iy0 z1, p2 = vec3 ox0 oy0 z1, p3 = vec3 ox1 oy1 z1, p4 = vec3 ix1 iy1 z1 }

                capBack =
                    Quad { color = 16, p1 = vec3 ox0 oy0 z0, p2 = vec3 ix0 iy0 z0, p3 = vec3 ix1 iy1 z0, p4 = vec3 ox1 oy1 z0 }

                eOuter0 =
                    LineSegment { color = 24, p1 = vec3 ox0 oy0 z0, p2 = vec3 ox1 oy1 z0 }

                eOuter1 =
                    LineSegment { color = 24, p1 = vec3 ox0 oy0 z1, p2 = vec3 ox1 oy1 z1 }

                spokeEdge =
                    LineSegment { color = 24, p1 = vec3 ox0 oy0 z0, p2 = vec3 ox0 oy0 z1 }
            in
            [ tread, capFront, capBack, eOuter0, eOuter1 ]
                ++ (if modBy 2 i == 0 then
                        [ spokeEdge ]

                    else
                        []
                   )
    in
    List.concatMap step (List.range 0 (segments - 1))


buildSteeringWheelLines : List LDrawLine
buildSteeringWheelLines =
    [ LineSegment { color = 24, p1 = vec3 0 -18 -6, p2 = vec3 0 -30 6 }
    ]
