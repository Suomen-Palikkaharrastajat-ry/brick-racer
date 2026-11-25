module Racer.Render exposing
    ( RenderCarSlot
    , buildCarWorldMatrix
    , carStyleFor
    , renderCarWithStyleAndSpins
    , renderGarageViewport
    , renderRaceViewport
    , renderRaceViewportMulti
    , renderVehicle
    )

{-| WebGL rendering compositor for Brick Racer's Garage Inspector and 3D Race View.

Combines the brick sky, chunked countryside terrain in the brick look
(`Racer.TerrainMesh`), frustum culling (`Render.Instanced.frustumFromMatrix`),
brick-built scenery, arches and debris from catalog models (`Racer.Scenery`),
articulated brick sub-assemblies (`Render.Scene.renderSceneWithTransform`) and
ground contact shadows.

-}

import Array
import Game.MapParams exposing (MapParams)
import Html exposing (Html)
import Html.Attributes as Attr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Analyze exposing (CarMeshBundle)
import Racer.Scenery as Scenery exposing (Props)
import Racer.TerrainMesh exposing (TrackChunk, TrackRenderBundle)
import Racer.TerrainShader as TerrainShader exposing (DebrisPiece, TerrainVertex)
import Racer.Track as Track
import Racer.Types exposing (GeneratedTrack, PhysicsState, SteeringMechanism(..), TrackId(..), VehicleSpec, lduPerMeter, lduToMeters)
import Render.Camera as Camera exposing (Camera)
import Render.Instanced as Instanced
import Render.Scene as Scene
import Render.Style as Style
import WebGL


{-| Scale matrix converting normalized Car Space (`1 unit = 1 LDU`) into world meters (`160 LDU = 1 m`).
-}
lduToMetersMatrix : Mat4
lduToMetersMatrix =
    let
        s =
            1.0 / lduPerMeter
    in
    Mat4.makeScale3 s s s


{-| Match brick plastic lighting direction and vibrance to each course's sky palette.
-}
carStyleFor : MapParams -> Style.Style
carStyleFor map =
    let
        pal =
            TerrainShader.paletteFor map

        base =
            Style.defaultStyle
    in
    { base
        | lightDirection = pal.sunDir
        , ambientStrength = 0.72
        , lightStrength = 0.42
        , specularStrength = 0.16
        , specularPower = 24.0
        , rimStrength = 0.12
        , vibrance = 0.25
        , edgeWidth = 1.4
    }


{-| Compose world transform matrix for the soapbox rigid body (`T * Ry * Rz * Rx`).
-}
buildCarWorldMatrix : PhysicsState -> Mat4
buildCarWorldMatrix physics =
    let
        pitchMat =
            Mat4.makeRotate physics.pitch (vec3 1 0 0)

        rollMat =
            Mat4.makeRotate physics.roll (vec3 0 0 1)

        yawMat =
            Mat4.makeRotate physics.yaw (vec3 0 1 0)

        transMat =
            Mat4.makeTranslate physics.position
    in
    transMat
        |> (\m -> Mat4.mul m yawMat)
        |> (\m -> Mat4.mul m rollMat)
        |> (\m -> Mat4.mul m pitchMat)


{-| Render the static and articulated brick sub-assemblies of a vehicle in world space.
-}
renderVehicle :
    Mat4
    -> Float
    -> List Float
    -> Camera
    -> Style.Style
    -> CarMeshBundle
    -> Int
    -> Int
    -> List WebGL.Entity
renderVehicle worldMat steerRad wheelSpins camera style meshes width height =
    let
        carInMeters =
            Mat4.mul worldMat lduToMetersMatrix

        visualYaw =
            -steerRad

        chassisEntities =
            Scene.renderSceneWithTransform carInMeters meshes.chassis camera style width height

        renderDynamic maybeSub rotMat =
            case maybeSub of
                Nothing ->
                    []

                Just sub ->
                    let
                        pivotWorld =
                            carInMeters
                                |> (\m -> Mat4.mul m (Mat4.makeTranslate sub.pivotLdu))
                                |> (\m -> Mat4.mul m rotMat)
                    in
                    Scene.renderSceneWithTransform pivotWorld sub.scene camera style width height

        bogieEntities =
            renderDynamic meshes.steeringBogie (Mat4.makeRotate visualYaw (vec3 0 1 0))

        leftKnuckleEntities =
            renderDynamic meshes.steeringLeftKnuckle (Mat4.makeRotate visualYaw (vec3 0 1 0))

        rightKnuckleEntities =
            renderDynamic meshes.steeringRightKnuckle (Mat4.makeRotate visualYaw (vec3 0 1 0))

        steeringWheelEntities =
            renderDynamic meshes.steeringWheel (Mat4.makeRotate (visualYaw * 2.2) (Vec3.normalize (vec3 0 0.65 0.75)))

        paddedSpins =
            padSpins (List.length meshes.wheelMeshes) wheelSpins

        wheelEntities =
            List.map2
                (\{ wheel, scene } spinAngle ->
                    let
                        steerTransform =
                            case wheel.isSteerable of
                                True ->
                                    Mat4.makeRotate visualYaw (vec3 0 1 0)

                                False ->
                                    Mat4.identity

                        -- Roll each wheel about its own axle. The mesh is centred on
                        -- its hub in car space, so the spin is applied before the
                        -- steering and hub transforms. A fixed X axis made wheels whose
                        -- axle is not along X wobble and shake as they turned.
                        spinTransform =
                            Mat4.makeRotate spinAngle wheel.spinAxis

                        wheelWorld =
                            case ( wheel.isSteerable, meshes.steeringBogie ) of
                                ( True, Just bogie ) ->
                                    carInMeters
                                        |> (\m -> Mat4.mul m (Mat4.makeTranslate bogie.pivotLdu))
                                        |> (\m -> Mat4.mul m steerTransform)
                                        |> (\m -> Mat4.mul m (Mat4.makeTranslate (Vec3.sub wheel.localHubPosLdu bogie.pivotLdu)))
                                        |> (\m -> Mat4.mul m spinTransform)

                                _ ->
                                    carInMeters
                                        |> (\m -> Mat4.mul m (Mat4.makeTranslate wheel.localHubPosLdu))
                                        |> (\m -> Mat4.mul m steerTransform)
                                        |> (\m -> Mat4.mul m spinTransform)
                    in
                    Scene.renderSceneWithTransform wheelWorld scene camera style width height
                )
                meshes.wheelMeshes
                paddedSpins
                |> List.concat
    in
    chassisEntities
        ++ bogieEntities
        ++ leftKnuckleEntities
        ++ rightKnuckleEntities
        ++ steeringWheelEntities
        ++ wheelEntities


{-| Render the vehicle with style and wheel spins.
Synonym for `renderVehicle`.
-}
renderCarWithStyleAndSpins :
    Mat4
    -> Float
    -> List Float
    -> Camera
    -> Style.Style
    -> CarMeshBundle
    -> Int
    -> Int
    -> List WebGL.Entity
renderCarWithStyleAndSpins =
    renderVehicle


{-| Render the interactive Garage 3D plinth inspection viewport.
-}
renderGarageViewport :
    TrackId
    -> WebGL.Mesh TerrainVertex
    -> WebGL.Mesh TerrainVertex
    -> Float
    -> VehicleSpec
    -> CarMeshBundle
    -> Camera
    -> Int
    -> Int
    -> List (Html.Attribute msg)
    -> Html msg
renderGarageViewport trackId plinthMesh shadowMesh wheelPreviewSpin spec meshes camera w h attrs =
    let
        map =
            Track.mapForTrack trackId 1

        safeW =
            max 1 w

        safeH =
            max 1 h

        widthF =
            max 1.0 (lduToMeters (Vec3.getX spec.boundsMaxLdu - Vec3.getX spec.boundsMinLdu))

        lengthF =
            max 1.0 (lduToMeters (Vec3.getZ spec.boundsMaxLdu - Vec3.getZ spec.boundsMinLdu))

        shadowMat =
            Mat4.mul
                (Mat4.makeTranslate (vec3 0 0.005 0))
                (Mat4.makeScale3 (widthF * 0.68) 1.0 (lengthF * 0.68))

        steerPreview =
            sin (wheelPreviewSpin * 0.45) * 0.34

        style =
            carStyleFor map

        entities =
            [ TerrainShader.skyEntity camera map safeW safeH
            , TerrainShader.terrainEntity Mat4.identity plinthMesh map camera safeW safeH
            , TerrainShader.shadowEntity shadowMat shadowMesh camera safeW safeH 0.0
            ]
                ++ renderVehicle
                    Mat4.identity
                    steerPreview
                    (List.repeat (List.length meshes.wheelMeshes) wheelPreviewSpin)
                    camera
                    style
                    meshes
                    safeW
                    safeH
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.depth 1
        , WebGL.antialias
        , WebGL.clearColor 0.78 0.89 0.96 1.0
        ]
        ([ Attr.width safeW
         , Attr.height safeH
         , Attr.style "display" "block"
         , Attr.style "width" "100%"
         , Attr.style "height" "100%"
         , Attr.style "touch-action" "pan-y"
         ]
            ++ attrs
        )
        entities


{-| Vehicle state and geometry slot rendered in the 3D race view.
-}
type alias RenderCarSlot =
    { spec : VehicleSpec
    , meshes : CarMeshBundle
    , physics : PhysicsState
    }


{-| Render the full-viewport WebGL racing scene for one vehicle.
-}
renderRaceViewport :
    TrackRenderBundle
    -> GeneratedTrack
    -> VehicleSpec
    -> CarMeshBundle
    -> Props
    -> WebGL.Mesh TerrainVertex
    -> PhysicsState
    -> List DebrisPiece
    -> Camera
    -> Int
    -> Int
    -> List (Html.Attribute msg)
    -> Html msg
renderRaceViewport bundle track spec meshes props shadowMesh physics debris camera w h attrs =
    renderRaceViewportMulti
        bundle
        track
        [ { spec = spec, meshes = meshes, physics = physics } ]
        props
        shadowMesh
        debris
        camera
        w
        h
        attrs


{-| Render the 3D racing viewport with support for multiple simultaneous vehicles
(e.g. Player 1 + Player 2 in split-screen racing).
-}
renderRaceViewportMulti :
    TrackRenderBundle
    -> GeneratedTrack
    -> List RenderCarSlot
    -> Props
    -> WebGL.Mesh TerrainVertex
    -> List DebrisPiece
    -> Camera
    -> Int
    -> Int
    -> List (Html.Attribute msg)
    -> Html msg
renderRaceViewportMulti bundle track cars props shadowMesh debris camera w h attrs =
    let
        safeW =
            max 1 w

        safeH =
            max 1 h

        aspect =
            toFloat safeW / toFloat safeH

        viewProj =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
                (Camera.viewMatrix camera)

        frustum =
            Instanced.frustumFromMatrix viewProj

        primaryProgress =
            cars
                |> List.head
                |> Maybe.map (.physics >> .progressS)
                |> Maybe.withDefault 0.0

        currentChunk =
            max 0 (floor (primaryProgress / bundle.chunkSizeM))

        chunkIndices =
            List.range (max 0 (currentChunk - 1)) (min (Array.length bundle.chunks - 1) (currentChunk + 4))

        propView =
            TerrainShader.propView camera bundle.map safeW safeH

        inView center radius =
            Instanced.sphereInFrustum frustum center radius

        visibleChunks =
            chunkIndices
                |> List.filterMap (\idx -> Array.get idx bundle.chunks)
                |> List.filter (\c -> Instanced.sphereInFrustum frustum c.boundsCenter c.boundsRadius)

        groundEntities =
            List.map (\c -> TerrainShader.groundEntity c.mesh bundle.map camera safeW safeH) visibleChunks

        propEntities =
            List.concatMap
                (\c ->
                    Scenery.archEntities propView inView props c.arches
                        ++ Scenery.obstacleEntities propView inView props c.scenery
                )
                visibleChunks

        style =
            carStyleFor bundle.map

        renderOneCar { spec, meshes, physics } =
            let
                groundBelowCar =
                    Track.sampleAtHinted physics.progressS track (Vec3.getX physics.position) (Vec3.getZ physics.position)

                clearance =
                    max 0.0 (Vec3.getY physics.position - groundBelowCar.height)

                carWidthM =
                    max 0.55 (lduToMeters (Vec3.getX spec.boundsMaxLdu - Vec3.getX spec.boundsMinLdu))

                carLenM =
                    max 0.8 (lduToMeters (Vec3.getZ spec.boundsMaxLdu - Vec3.getZ spec.boundsMinLdu))

                shadowPos =
                    vec3 (Vec3.getX physics.position) (groundBelowCar.height + 0.03) (Vec3.getZ physics.position)

                shadowScale =
                    max 0.6 (1.0 - clearance * 0.08)

                shadowMat =
                    Mat4.makeTranslate shadowPos
                        |> (\m -> Mat4.mul m (Mat4.makeRotate physics.yaw (vec3 0 1 0)))
                        |> (\m -> Mat4.mul m (Mat4.makeScale3 (carWidthM * 0.72 * shadowScale) 1.0 (carLenM * 0.65 * shadowScale)))

                carWorldMat =
                    buildCarWorldMatrix physics

                carEntities =
                    renderVehicle
                        carWorldMat
                        physics.steerAngle
                        physics.wheelSpinAngles
                        camera
                        style
                        meshes
                        safeW
                        safeH

                speed =
                    Vec3.length physics.velocity

                driftIntensity =
                    clamp 0.0 1.0 ((abs physics.steerAngle - 0.08) * 3.2 * clamp 0.0 1.0 (speed / 4.0))

                skidEntities =
                    if driftIntensity > 0.05 then
                        let
                            skidMat sideX =
                                Mat4.makeTranslate (vec3 (Vec3.getX physics.position) (groundBelowCar.height + 0.02) (Vec3.getZ physics.position))
                                    |> (\m -> Mat4.mul m (Mat4.makeRotate physics.yaw (vec3 0 1 0)))
                                    |> (\m -> Mat4.mul m (Mat4.makeTranslate3 sideX 0.0 (carLenM * 0.25)))
                                    |> (\m -> Mat4.mul m (Mat4.makeScale3 (carWidthM * 0.16) 1.0 (carLenM * 0.45 * (1.0 + driftIntensity))))

                            fade =
                                1.0 - driftIntensity * 0.4
                        in
                        [ TerrainShader.shadowEntity (skidMat (-carWidthM * 0.38)) shadowMesh camera safeW safeH fade
                        , TerrainShader.shadowEntity (skidMat (carWidthM * 0.38)) shadowMesh camera safeW safeH fade
                        ]

                    else
                        []
            in
            TerrainShader.shadowEntity shadowMat shadowMesh camera safeW safeH clearance :: skidEntities ++ carEntities

        -- A car out of health has broken apart into its bricks (now debris):
        -- draw nothing for it until it is rebuilt.
        carsEntities =
            cars
                |> List.filter (\car -> car.physics.integrity > 0.0)
                |> List.concatMap renderOneCar

        debrisEntities =
            Scenery.debrisEntities propView props debris

        allEntities =
            [ TerrainShader.skyEntity camera bundle.map safeW safeH ]
                ++ groundEntities
                ++ carsEntities
                ++ propEntities
                ++ debrisEntities
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.depth 1
        , WebGL.antialias
        , WebGL.clearColor 0.78 0.89 0.96 1.0
        ]
        ([ Attr.width safeW
         , Attr.height safeH
         , Attr.style "display" "block"
         , Attr.style "width" "100%"
         , Attr.style "height" "100%"
         , Attr.style "touch-action" "pan-y"
         ]
            ++ attrs
        )
        allEntities


padSpins : Int -> List Float -> List Float
padSpins needed existing =
    let
        len =
            List.length existing
    in
    if len >= needed then
        List.take needed existing

    else
        existing ++ List.repeat (needed - len) 0.0
