module Racer.Camera exposing
    ( cycleMode
    , initGarageCamera
    , initRaceCamera
    , updateGarageCamera
    , updateRaceCamera
    )

{-| Camera controllers bridging `Racer.Types` and `Render.Camera.Camera`.

Maintains a single `Render.Camera.Camera` record (`sceneRadius = 450.0 m`) so
`Render.Shader`, `Render.EdgeShader`, `Render.Scene.renderSceneWithTransform`,
and custom terrain shaders share identical view & projection matrices.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Physics as Physics
import Racer.Types exposing (CameraMode(..), PhysicsState, VehicleSpec, lduToMeters)
import Render.Camera as Camera exposing (Camera)


{-| Initialize an orbital inspection camera centered on the soapbox in Garage view (in meters).
-}
initGarageCamera : Int -> VehicleSpec -> Camera
initGarageCamera viewportHeight spec =
    let
        extent =
            Vec3.sub spec.boundsMaxLdu spec.boundsMinLdu

        radiusM =
            max 0.7 (lduToMeters (Vec3.length extent * 0.5))

        centerM =
            Vec3.scale 0.5
                (Vec3.add
                    (Vec3.scale (1.0 / 160.0) spec.boundsMinLdu)
                    (Vec3.scale (1.0 / 160.0) spec.boundsMaxLdu)
                )
    in
    Camera.init
        |> Camera.setViewportHeight (toFloat (max 1 viewportHeight))
        |> Camera.setSceneRadius 220.0
        |> Camera.setZoomRange (max 1.2 (radiusM * 1.3)) (max 8.0 (radiusM * 7.5))
        |> (\c ->
                { c
                    | azimuth = 0.65
                    , elevation = 0.36
                    , distance = clamp 2.2 6.8 (radiusM * 3.1)
                    , target = centerM
                }
           )


{-| Gently auto-rotate Garage inspection camera when the user is not dragging.
-}
updateGarageCamera : Float -> Camera -> Camera
updateGarageCamera dt cam =
    if cam.dragging then
        cam

    else
        Camera.orbitBy (dt * 0.28) 0.0 cam


{-| Initialize race camera positioned cleanly behind the car on the starting ramp.
-}
initRaceCamera : Int -> PhysicsState -> VehicleSpec -> Camera
initRaceCamera viewportHeight physics spec =
    let
        extent =
            Vec3.sub spec.boundsMaxLdu spec.boundsMinLdu

        radiusM =
            max 0.7 (lduToMeters (Vec3.length extent * 0.5))

        fwd =
            Physics.forwardVector physics.yaw

        target =
            Vec3.add physics.position (Vec3.add (vec3 0 0.55 0) (Vec3.scale 0.65 fwd))
    in
    Camera.init
        |> Camera.setViewportHeight (toFloat (max 1 viewportHeight))
        |> Camera.setSceneRadius 450.0
        |> Camera.setZoomRange 1.5 18.0
        |> (\c ->
                { c
                    | azimuth = physics.yaw + pi
                    , elevation = 0.28
                    , distance = clamp 3.4 6.0 (2.7 + radiusM * 2.1)
                    , target = target
                }
           )


{-| Cycle between `ChaseCam`, `HoodCam`, and `OrbitCam`.
-}
cycleMode : CameraMode -> CameraMode
cycleMode mode =
    case mode of
        ChaseCam ->
            HoodCam

        HoodCam ->
            OrbitCam

        OrbitCam ->
            ChaseCam


{-| Update race camera according to active `CameraMode`, car velocity, and terrain slope.
-}
updateRaceCamera : Float -> CameraMode -> PhysicsState -> VehicleSpec -> Camera -> Camera
updateRaceCamera dt mode physics spec cam =
    let
        extent =
            Vec3.sub spec.boundsMaxLdu spec.boundsMinLdu

        radiusM =
            max 0.7 (lduToMeters (Vec3.length extent * 0.5))

        speed =
            Vec3.length physics.velocity

        fwd =
            Physics.forwardVector physics.yaw

        carCenterHeight =
            max 0.35 (lduToMeters (Vec3.getY spec.boundsMaxLdu))

        baseCam =
            Camera.setSceneRadius 450.0 cam
    in
    case mode of
        ChaseCam ->
            let
                desiredAz =
                    physics.yaw + pi - physics.yawRate * 0.09

                -- Look slightly downward as the slope steepens so roadbed fills lower 2/3
                desiredEl =
                    clamp 0.16 0.45 (0.25 + physics.gradeRatio * 0.38)

                desiredDist =
                    clamp 3.2 7.2 (2.6 + radiusM * 2.0 + speed * 0.05)

                lookAheadTarget =
                    physics.position
                        |> Vec3.add (vec3 0 (carCenterHeight * 0.65 + 0.18) 0)
                        |> Vec3.add (Vec3.scale 0.85 fwd)

                alpha =
                    clamp 0.0 1.0 (dt * 9.0)

                nextAz =
                    lerpAngle baseCam.azimuth desiredAz alpha

                nextEl =
                    lerp baseCam.elevation desiredEl (clamp 0.0 1.0 (dt * 6.0))

                nextDist =
                    lerp baseCam.distance desiredDist (clamp 0.0 1.0 (dt * 5.0))

                nextTarget =
                    vec3Lerp (clamp 0.0 1.0 (dt * 8.5)) baseCam.target lookAheadTarget
            in
            { baseCam
                | azimuth = nextAz
                , elevation = nextEl
                , distance = nextDist
                , target = nextTarget
            }

        HoodCam ->
            let
                hoodHeight =
                    clamp 0.32 1.2 (carCenterHeight * 0.82 + 0.18)

                eyeOrigin =
                    physics.position
                        |> Vec3.add (vec3 0 hoodHeight 0)
                        |> Vec3.add (Vec3.scale 0.08 fwd)

                lookDistance =
                    3.0

                slopeDown =
                    clamp -0.25 0.1 -(physics.gradeRatio * 0.35)

                targetPt =
                    eyeOrigin
                        |> Vec3.add (Vec3.scale lookDistance fwd)
                        |> Vec3.add (vec3 0 (lookDistance * slopeDown) 0)
            in
            { baseCam
                | azimuth = physics.yaw + pi
                , elevation = -slopeDown * 0.8
                , distance = lookDistance
                , target = targetPt
            }

        OrbitCam ->
            let
                targetPt =
                    Vec3.add physics.position (vec3 0 (carCenterHeight * 0.5) 0)

                deltaMove =
                    Vec3.sub targetPt baseCam.target
            in
            { baseCam
                | target = Vec3.add baseCam.target deltaMove
            }


lerp : Float -> Float -> Float -> Float
lerp a b t =
    a + (b - a) * t


vec3Lerp : Float -> Vec3 -> Vec3 -> Vec3
vec3Lerp t a b =
    vec3
        (lerp (Vec3.getX a) (Vec3.getX b) t)
        (lerp (Vec3.getY a) (Vec3.getY b) t)
        (lerp (Vec3.getZ a) (Vec3.getZ b) t)


lerpAngle : Float -> Float -> Float -> Float
lerpAngle from to t =
    let
        diff =
            wrapPi (to - from)
    in
    wrapPi (from + diff * t)


wrapPi : Float -> Float
wrapPi a =
    let
        twoPi =
            2.0 * pi
    in
    if a > pi then
        wrapPi (a - twoPi)

    else if a < -pi then
        wrapPi (a + twoPi)

    else
        a
