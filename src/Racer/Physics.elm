module Racer.Physics exposing
    ( breakSpeedFor
    , forwardVector
    , initPhysics
    , initPhysicsWithOffset
    , landingDamage
    , maxHealth
    , obstacleImpactDamage
    , recoveryTowSpeed
    , remoteSmashes
    , respawnAtCheckpoint
    , rightVector
    , stepFixed
    , stepFrame
    , surfaceGripMultiplier
    , surfaceRollingResist
    )

{-| Deterministic 60 Hz downhill soapbox physics engine.

Integrates gravity projected onto continuous 3D countryside terrain, 4-wheel suspension
loads and pitch/roll chassis tilt, surface-dependent rolling resistance and lateral
tire slip, articulated steering vs fixed-axle weight-shift authority, aerodynamic drag
with aero tuck, obstacle impulses, structural brick pop-off events, and checkpoint tracking.

Brick launch ramps (`Track.brickRampLift`) and rounded side ramps
(`Track.sideRampLift`, which also tilt the ground normal) are added to the
ground under the wheel probes. Climbing one gives the car its upward speed, and where the ground
falls away under the car faster than it can fall (the ramp's lip, the back of a
hump), the car flies on a ballistic arc until the normal landing code catches it.

The brick loops (`GeneratedTrack.loops`) cannot be a heightfield: their lane
goes over the top of itself. A car that crosses a loop's entry on the road
rides its lane instead (`PhysicsState.loopRide`): a point on the lane surface
(`Track.loopFrame`) moving along it under gravity, drag and rolling resistance,
held on by the lane's support force. It leaves the lane at the exit, back onto
the road; if it is too slow, it rolls back out of the entry or, where the
support force would have to pull, falls off and flies like any airborne car.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Track as Track
import Racer.Types
    exposing
        ( CrashReason(..)
        , GeneratedTrack
        , InputState
        , LoopRide
        , LoopSection
        , ObstacleKind(..)
        , PhysicsEvent(..)
        , PhysicsState
        , SteeringMechanism(..)
        , SurfaceType(..)
        , TrackObstacle
        , VehicleSpec
        , WheelSpec
        , fixedDt
        )


gravityAccel : Float
gravityAccel =
    9.81


airDensity : Float
airDensity =
    1.225


maxFrameDelta : Float
maxFrameDelta =
    0.05


maxSubstepsPerFrame : Int
maxSubstepsPerFrame =
    4


{-| Initialize a `PhysicsState` parked squarely on the track starting ramp.
-}
initPhysics : GeneratedTrack -> VehicleSpec -> PhysicsState
initPhysics track spec =
    let
        wheelCount =
            max 1 (List.length spec.wheels)

        terrain =
            Track.sampleAt track (Vec3.getX track.startPosition) (Vec3.getZ track.startPosition)

        tanY =
            Vec3.getY terrain.nearestTangent

        initPitch =
            -(atan2 tanY (max 0.1 (sqrt (1.0 - min 0.99 (tanY * tanY)))))

        startPos =
            vec3 (Vec3.getX track.startPosition) terrain.height (Vec3.getZ track.startPosition)
    in
    { position = startPos
    , velocity = vec3 0 0 0
    , yaw = track.startYaw
    , yawRate = 0.0
    , pitch = initPitch
    , pitchRate = 0.0
    , roll = 0.0
    , rollRate = 0.0
    , steerAngle = 0.0
    , leanInput = { x = 0.0, y = 0.0 }
    , wheelSpinAngles = List.repeat wheelCount 0.0
    , suspensionCompressions = List.repeat 4 0.0
    , groundedWheels = wheelCount
    , airborneTime = 0.0
    , surfaceUnderCar = terrain.surface
    , integrity = maxHealth
    , chassisScraping = spec.groundClearanceLdu < -0.5
    , offTrack = False
    , highSpeedBonus = False
    , rolloverTimer = 0.0
    , progressS = 0.0
    , checkpointIndex = 0
    , checkpointAnchor = { s = 0.0, position = startPos, yaw = track.startYaw }
    , lastImpactCooldown = 0.0
    , nextPopThreshold = 80.0
    , gradeRatio = max 0.0 -(Vec3.getY terrain.nearestTangent)
    , loopRide = Nothing
    }


{-| Initialize downhill soapbox dynamics with a lateral starting grid offset in meters
(e.g. -1.8 m for Player 1, +1.8 m for Player 2 in split-screen matches).
-}
initPhysicsWithOffset : Float -> GeneratedTrack -> VehicleSpec -> PhysicsState
initPhysicsWithOffset lateralOffsetM track spec =
    let
        p0 =
            initPhysics track spec

        cosYaw =
            cos p0.yaw

        sinYaw =
            sin p0.yaw

        posX =
            Vec3.getX p0.position + cosYaw * lateralOffsetM

        posZ =
            Vec3.getZ p0.position - sinYaw * lateralOffsetM

        terrain =
            Track.sampleAt track posX posZ

        offsetPos =
            vec3 posX terrain.height posZ

        anchor =
            { s = 0.0, position = offsetPos, yaw = track.startYaw }
    in
    { p0
        | position = offsetPos
        , checkpointAnchor = anchor
        , surfaceUnderCar = terrain.surface
    }


{-| Reset car upright and stationary onto the most recently passed checkpoint anchor
with restored structural integrity.
-}
respawnAtCheckpoint : PhysicsState -> PhysicsState
respawnAtCheckpoint state =
    { state
        | position = Vec3.add state.checkpointAnchor.position (vec3 0 0.08 0)
        , velocity = vec3 0 0 0
        , yaw = state.checkpointAnchor.yaw
        , yawRate = 0.0
        , pitch = 0.0
        , pitchRate = 0.0
        , roll = 0.0
        , rollRate = 0.0
        , steerAngle = 0.0
        , suspensionCompressions = List.map (\_ -> 0.0) state.suspensionCompressions
        , groundedWheels = max 1 (List.length state.wheelSpinAngles)
        , airborneTime = 0.0
        , integrity = maxHealth
        , nextPopThreshold = 80.0
        , rolloverTimer = 0.0
        , progressS = state.checkpointAnchor.s
        , lastImpactCooldown = 0.5
        , highSpeedBonus = False
        , loopRide = Nothing
    }


{-| Accumulate raw frame delta (`dtSeconds`) and advance physics in fixed `1/60 s` steps,
bounded to at most 4 substeps per frame to prevent spiral-of-death slowdowns.
-}
stepFrame :
    Float
    -> Float
    -> GeneratedTrack
    -> VehicleSpec
    -> InputState
    -> PhysicsState
    -> ( Float, PhysicsState, List PhysicsEvent )
stepFrame rawFrameDt accumulator track spec input state =
    let
        clampedDt =
            clamp 0.0 maxFrameDelta rawFrameDt

        totalAcc =
            accumulator + clampedDt
    in
    runSubsteps 0 totalAcc track spec input state []


runSubsteps :
    Int
    -> Float
    -> GeneratedTrack
    -> VehicleSpec
    -> InputState
    -> PhysicsState
    -> List PhysicsEvent
    -> ( Float, PhysicsState, List PhysicsEvent )
runSubsteps steps acc track spec input state eventsAcc =
    if steps >= maxSubstepsPerFrame || acc < fixedDt then
        ( min acc (fixedDt * 2.0), state, List.reverse eventsAcc )

    else
        let
            ( nextState, stepEvents ) =
                stepFixed fixedDt track spec input state

            -- Smashed scenery is gone for the rest of this frame's substeps too;
            -- the caller removes it from the race track for later frames.
            remainingTrack =
                Track.removeObstacles (smashedIds stepEvents) track
        in
        runSubsteps (steps + 1) (acc - fixedDt) remainingTrack spec input nextState (List.reverse stepEvents ++ eventsAcc)


smashedIds : List PhysicsEvent -> List Int
smashedIds events =
    List.filterMap
        (\ev ->
            case ev of
                ScenerySmashed { obstacleId } ->
                    Just obstacleId

                _ ->
                    Nothing
        )
        events


{-| Advance vehicle simulation by one deterministic fixed timestep `dt` seconds.
-}
stepFixed :
    Float
    -> GeneratedTrack
    -> VehicleSpec
    -> InputState
    -> PhysicsState
    -> ( PhysicsState, List PhysicsEvent )
stepFixed dt track spec input state =
    let
        riding =
            state.loopRide
                |> Maybe.andThen
                    (\ride ->
                        List.drop ride.loopIndex track.loops
                            |> List.head
                            |> Maybe.map (Tuple.pair ride)
                    )
    in
    case riding of
        Just ( ride, loop ) ->
            stepOnLoop dt track spec input loop ride state

        Nothing ->
            stepOnGround dt track spec input { state | loopRide = Nothing }
                |> enterLoop track state


stepOnGround :
    Float
    -> GeneratedTrack
    -> VehicleSpec
    -> InputState
    -> PhysicsState
    -> ( PhysicsState, List PhysicsEvent )
stepOnGround dt track spec input state =
    let
        mass =
            max 12.0 spec.totalMassKg

        fwd =
            forwardVector state.yaw

        right =
            rightVector state.yaw

        halfTrack =
            max 0.25 (spec.trackWidthM * 0.5)

        halfBase =
            max 0.32 (spec.wheelBaseM * 0.5)

        centerTerrain =
            Track.sampleAtHinted state.progressS track (Vec3.getX state.position) (Vec3.getZ state.position)
                |> withBrickRamps track

        centerRamp =
            Track.brickRampLift track centerTerrain.nearestS centerTerrain.roadOffsetM

        centerSideRamp =
            Track.sideRampLift track centerTerrain.nearestS centerTerrain.roadOffsetM

        speed =
            Vec3.length state.velocity

        horizSpeed =
            sqrt (Vec3.getX state.velocity ^ 2 + Vec3.getZ state.velocity ^ 2)

        vFwd =
            Vec3.dot state.velocity fwd

        vRight =
            Vec3.dot state.velocity right

        -- Steering responsiveness and speed-dependent max angle (in radians; positive = right turn)
        maxSteer =
            clamp 0.08 0.44 (0.44 / (1.0 + 0.085 * horizSpeed))

        targetSteer =
            clamp -1.0 1.0 input.steer * maxSteer

        steerAuthority =
            case spec.steering of
                TurntableBogie _ ->
                    1.2

                TwinKnuckle _ ->
                    1.18

                FixedAxleFallback ->
                    0.85

        steerLerpRate =
            clamp 0.0 1.0 (dt * (7.0 + 3.0 * steerAuthority))

        nextSteerAngle =
            state.steerAngle + (targetSteer - state.steerAngle) * steerLerpRate

        leanLat =
            clamp -1.0 1.0 (input.leanX + input.steer * 0.35)

        -- 4-corner terrain height & suspension probes
        cornerOffsets =
            [ ( -halfTrack, halfBase )
            , ( halfTrack, halfBase )
            , ( -halfTrack, -halfBase )
            , ( halfTrack, -halfBase )
            ]

        probeCorner ( lx, lz ) =
            let
                worldPt =
                    Vec3.add state.position
                        (Vec3.add (Vec3.scale lx right) (Vec3.scale lz fwd))

                smp =
                    Track.sampleAtHinted centerTerrain.nearestS track (Vec3.getX worldPt) (Vec3.getZ worldPt)
                        |> withBrickRamps track

                -- Vertical offset of chassis corner due to pitch/roll
                cornerY =
                    Vec3.getY state.position - lz * sin state.pitch + lx * sin state.roll
            in
            { lx = lx
            , lz = lz
            , terrainHeight = smp.height
            , compression = smp.height - cornerY
            }

        probes =
            List.map probeCorner cornerOffsets

        minAvgHeight =
            (List.sum (List.map .terrainHeight probes) / 4.0)
                |> max (centerTerrain.height - 0.12)

        -- Nominal ride height keeps vehicle flush with terrain on zero slope at rest
        maxCompression =
            probes
                |> List.map .compression
                |> List.maximum
                |> Maybe.withDefault 0.0

        isGroundedNow =
            Vec3.getY state.position <= centerTerrain.height + 0.16 || maxCompression > -0.08

        prevWasAirborne =
            state.groundedWheels == 0 && state.airborneTime > 0.12

        groundedCornersCount =
            if isGroundedNow then
                probes
                    |> List.filter (\p -> p.compression >= -0.14)
                    |> List.length
                    |> max 1

            else
                0

        actualWheelCount =
            max 1 (List.length spec.wheels)

        groundedWheels =
            if groundedCornersCount == 0 then
                0

            else
                clamp 1 actualWheelCount (round (toFloat groundedCornersCount * toFloat actualWheelCount / 4.0))

        groundedRatio =
            toFloat groundedCornersCount / 4.0

        -- Tire rubber & road surface parameters
        hasAnyRubber =
            List.any .hasRubberTire spec.wheels

        tireGripFactor =
            if hasAnyRubber then
                1.0

            else
                0.62

        surfaceGrip =
            surfaceGripMultiplier centerTerrain.surface

        surfaceRoll =
            surfaceRollingResist centerTerrain.surface

        bellyScrape =
            spec.groundClearanceLdu < -0.5 || maxCompression > 0.22

        scrapeResist =
            if bellyScrape && isGroundedNow then
                0.18

            else
                0.0

        -- Gravity along the terrain surface normal
        normalUpDot =
            clamp 0.25 1.0 (Vec3.getY centerTerrain.normal)

        downForce =
            vec3 0 (-mass * gravityAccel) 0

        tangentGravity =
            Vec3.sub downForce (Vec3.scale (Vec3.dot downForce centerTerrain.normal) centerTerrain.normal)

        -- Normal force load on grounded wheels
        normalLoad =
            if isGroundedNow then
                mass * gravityAccel * normalUpDot

            else
                0.0

        -- Rolling resistance opposing velocity tangent to road
        vSpeedMag =
            max 1.0e-5 speed

        rollingForceMag =
            if isGroundedNow && speed > 0.04 then
                (surfaceRoll + scrapeResist) * normalLoad

            else
                0.0

        rollingForce =
            if speed > 0.04 then
                Vec3.scale (-rollingForceMag / vSpeedMag) state.velocity

            else
                vec3 0 0 0

        -- Aerodynamic drag opposing velocity (tuck lowers CdA by 18%)
        tuckMult =
            1.0 - 0.18 * clamp 0.0 1.0 input.aeroTuck

        aeroDragMag =
            0.5 * airDensity * spec.dragCoeff * spec.frontalAreaM2 * tuckMult * speed * speed

        aeroForce =
            if speed > 0.05 then
                Vec3.scale (-aeroDragMag / vSpeedMag) state.velocity

            else
                vec3 0 0 0

        -- Service brakes & handbrake deceleration
        brakeDecel =
            let
                service =
                    6.8 * clamp 0.0 1.0 input.brake

                hand =
                    if input.handbrake then
                        2.6

                    else
                        0.0
            in
            (service + hand) * mass * groundedRatio

        brakeForce =
            if isGroundedNow && horizSpeed > 0.08 then
                let
                    horizVel =
                        vec3 (Vec3.getX state.velocity) 0 (Vec3.getZ state.velocity)

                    dir =
                        Vec3.scale (1.0 / horizSpeed) horizVel

                    maxStopMag =
                        mass * horizSpeed / dt
                in
                Vec3.scale -(min brakeDecel maxStopMag) dir

            else
                vec3 0 0 0

        -- Bicycle lateral tire slip + steering forces
        turnRad =
            nextSteerAngle

        frontAxleX =
            vec3 (Vec3.getX right * cos turnRad - Vec3.getX fwd * sin turnRad)
                0
                (Vec3.getZ right * cos turnRad - Vec3.getZ fwd * sin turnRad)
                |> (\v -> normalizeSafe v right)

        -- Front & rear velocity along each axle's right direction
        vFrontRight =
            Vec3.dot
                (vec3 (Vec3.getX state.velocity + state.yawRate * halfBase * -(Vec3.getX right))
                    0
                    (Vec3.getZ state.velocity + state.yawRate * halfBase * -(Vec3.getZ right))
                )
                frontAxleX

        vRearRight =
            vRight + state.yawRate * halfBase

        tuckFrontLoadBonus =
            1.0 + 0.15 * clamp 0.0 1.0 input.aeroTuck

        frontGripForceMag =
            if isGroundedNow then
                let
                    stiffness =
                        4.8 * surfaceGrip * tireGripFactor * steerAuthority * tuckFrontLoadBonus * groundedRatio
                in
                clamp -(normalLoad * 0.74 * surfaceGrip) (normalLoad * 0.74 * surfaceGrip) (-vFrontRight * mass * stiffness)

            else
                0.0

        rearGripScale =
            if input.handbrake then
                0.38

            else
                1.0

        rearGripForceMag =
            if isGroundedNow then
                let
                    stiffness =
                        6.8 * surfaceGrip * tireGripFactor * rearGripScale * groundedRatio
                in
                clamp -(normalLoad * 0.88 * surfaceGrip * rearGripScale) (normalLoad * 0.88 * surfaceGrip * rearGripScale) (-vRearRight * mass * stiffness)

            else
                0.0

        lateralForceVec =
            Vec3.add
                (Vec3.scale frontGripForceMag frontAxleX)
                (Vec3.scale rearGripForceMag right)

        -- Yaw torque from front & rear lateral forces:
        -- (+halfBase * fwd) x (frontForce * frontRight) has Y = -(halfBase * frontForce * dot(frontAxleX, right))
        alignDot =
            Vec3.dot frontAxleX right

        yawDamping =
            max 2.0 spec.inertia.yaw * 3.0

        yawTorque =
            (-halfBase * frontGripForceMag * alignDot) + (halfBase * rearGripForceMag) - state.yawRate * yawDamping

        nextYawRate =
            clamp -3.8 3.8 (state.yawRate + (yawTorque / max 2.0 spec.inertia.yaw) * dt)

        nextYaw =
            normalizeAngle (state.yaw + nextYawRate * dt)

        -- Combine linear horizontal & tangent gravity acceleration
        slopeComponent =
            if isGroundedNow then
                tangentGravity

            else
                vec3 0 0 0

        totalForce3D =
            slopeComponent
                |> Vec3.add rollingForce
                |> Vec3.add aeroForce
                |> Vec3.add brakeForce
                |> Vec3.add lateralForceVec

        velAfterForcesXZ =
            Vec3.add state.velocity (Vec3.scale (dt / mass) totalForce3D)

        -- Keep velocity clean when nearly stopped on very gentle slopes or braking
        dampedHorizVel =
            let
                slopeMag =
                    Vec3.length tangentGravity

                stillSpeed =
                    Vec3.length velAfterForcesXZ
            in
            if isGroundedNow && input.brake > 0.5 && horizSpeed < 0.25 then
                vec3 0 (Vec3.getY velAfterForcesXZ) 0

            else if isGroundedNow && stillSpeed < 0.03 && slopeMag < mass * gravityAccel * surfaceRoll * 0.85 then
                vec3 0 (Vec3.getY velAfterForcesXZ) 0

            else
                velAfterForcesXZ

        -- Vertical integration with smooth spring-damper ground following vs ballistic airtime
        rawNextYVel =
            Vec3.getY state.velocity - gravityAccel * dt

        -- Vertical speed the ground under the car adds while climbing a brick
        -- ramp: its rise per meter of arc length times the speed along the road.
        rampSlopeVy =
            if centerRamp.grade > 0.0 then
                let
                    tanX =
                        Vec3.getX centerTerrain.nearestTangent

                    tanZ =
                        Vec3.getZ centerTerrain.nearestTangent

                    horizTanLen =
                        max 0.25 (sqrt (tanX * tanX + tanZ * tanZ))

                    vAlong =
                        (Vec3.getX state.velocity * tanX + Vec3.getZ state.velocity * tanZ) / horizTanLen
                in
                max 0.0 vAlong * centerRamp.grade / horizTanLen

            else
                0.0

        -- Likewise on a rounded side ramp, climbing it sideways or along the road.
        sideSlopeVy =
            if centerSideRamp.lift > 0.0 then
                let
                    tanFlat =
                        normalizeSafe (vec3 (Vec3.getX centerTerrain.nearestTangent) 0 (Vec3.getZ centerTerrain.nearestTangent)) (vec3 0 0 1)

                    rightFlat =
                        vec3 -(Vec3.getZ tanFlat) 0 (Vec3.getX tanFlat)
                in
                Vec3.dot state.velocity rightFlat * centerSideRamp.sideGrade + Vec3.dot state.velocity tanFlat * centerSideRamp.grade

            else
                0.0

        vertical =
            let
                predictedY =
                    Vec3.getY state.position + rawNextYVel * dt
            in
            if predictedY <= minAvgHeight then
                let
                    terrainSlopeVy =
                        vFwd * Vec3.getY centerTerrain.nearestTangent + rampSlopeVy + sideSlopeVy

                    penetration =
                        minAvgHeight - predictedY

                    impactVel =
                        max 0.0 -(rawNextYVel - terrainSlopeVy)

                    bounceVy =
                        if prevWasAirborne && impactVel > 2.6 then
                            terrainSlopeVy + impactVel * 0.05

                        else
                            terrainSlopeVy

                    landEv =
                        if prevWasAirborne && impactVel > 2.0 then
                            [ Landing { impulse = clamp 0.1 1.0 (impactVel / 9.0) } ]

                        else
                            []

                    targetY =
                        minAvgHeight + min 0.015 (penetration * 0.03)

                    smoothY =
                        Vec3.getY state.position + (targetY - Vec3.getY state.position) * min 1.0 (dt * 36.0)
                in
                { y = smoothY
                , vy = bounceVy
                , landing = landEv
                , landingDamage =
                    if prevWasAirborne then
                        landingDamage impactVel

                    else
                        0.0
                }

            else if isGroundedNow && maxCompression > -0.05 && rawNextYVel <= 0.6 && predictedY - minAvgHeight <= groundFallAwayM then
                -- Stay planted smoothly along continuous downward curved hills unless cresting hard.
                -- Where the ground drops away faster than the car can fall (a ramp lip), it
                -- takes off instead: see the ballistic branch below.
                let
                    terrainFollowY =
                        Vec3.getY state.position + (minAvgHeight - Vec3.getY state.position) * min 1.0 (dt * 32.0)

                    terrainSlopeVy =
                        vFwd * Vec3.getY centerTerrain.nearestTangent + rampSlopeVy + sideSlopeVy
                in
                { y = max minAvgHeight terrainFollowY, vy = terrainSlopeVy, landing = [], landingDamage = 0.0 }

            else
                { y = predictedY, vy = rawNextYVel, landing = [], landingDamage = 0.0 }

        finalPosY =
            vertical.y

        finalVelY =
            vertical.vy

        landingImpact =
            vertical.landing

        -- Recovery tow: a slow car stranded out in the field is eased back
        -- toward the nearest point of the road. Off on the road itself.
        towSpeed =
            recoveryTowSpeed
                { roadOffsetM = centerTerrain.roadOffsetM
                , halfWidthM = centerTerrain.halfWidthM
                , speed = horizSpeed
                , grounded = isGroundedNow
                }

        towStep =
            if towSpeed > 0.0 then
                let
                    toRoad =
                        vec3
                            (Vec3.getX centerTerrain.nearestCenter - Vec3.getX state.position)
                            0
                            (Vec3.getZ centerTerrain.nearestCenter - Vec3.getZ state.position)
                in
                Vec3.scale (towSpeed * dt) (normalizeSafe toRoad (vec3 0 0 0))

            else
                vec3 0 0 0

        prelimPos =
            vec3
                (Vec3.getX state.position + Vec3.getX dampedHorizVel * dt + Vec3.getX towStep)
                finalPosY
                (Vec3.getZ state.position + Vec3.getZ dampedHorizVel * dt + Vec3.getZ towStep)

        prelimVel =
            vec3 (Vec3.getX dampedHorizVel) finalVelY (Vec3.getZ dampedHorizVel)

        -- Suspension pitch & roll spring simulation driven by terrain slope, cornering lateral accel & driver lean
        targetRoadPitch =
            let
                tanY =
                    Vec3.getY centerTerrain.nearestTangent

                horizTangentLen =
                    sqrt (Vec3.getX centerTerrain.nearestTangent ^ 2 + Vec3.getZ centerTerrain.nearestTangent ^ 2)

                headingDot =
                    if horizTangentLen > 1.0e-3 then
                        clamp -1.0 1.0 ((Vec3.getX fwd * Vec3.getX centerTerrain.nearestTangent + Vec3.getZ fwd * Vec3.getZ centerTerrain.nearestTangent) / horizTangentLen)

                    else
                        1.0

                effectiveTanY =
                    headingDot * tanY
            in
            -(atan2 effectiveTanY (max 0.1 (sqrt (1.0 - min 0.99 (effectiveTanY * effectiveTanY)))))

        latAccel =
            vFwd * -nextYawRate

        comHeight =
            max 0.18 (Vec3.getY spec.centerOfMassM)

        rollTorqueInput =
            (latAccel * comHeight * mass * 0.38) - (leanLat * mass * 2.1 * halfTrack)

        rollSpring =
            -mass * halfTrack * 28.0 * state.roll - mass * 9.5 * state.rollRate

        nextRollRate =
            clamp -4.5 4.5 (state.rollRate + ((rollTorqueInput + rollSpring) / max 1.5 spec.inertia.roll) * dt)

        nextRoll =
            clamp -1.35 1.35 (state.roll + nextRollRate * dt)

        pitchDiff =
            targetRoadPitch - state.pitch

        -- A car that fell off a loop can still be nose up or upside down:
        -- it turns back over towards the road at a steady rate.
        tumblingBack =
            abs state.pitch > 0.76

        nextPitchRate =
            if tumblingBack then
                0.0

            else
                clamp -4.0 4.0 (state.pitchRate + (pitchDiff * 42.0 - state.pitchRate * 13.5 + input.brake * 2.4) * dt)

        nextPitch =
            if tumblingBack then
                state.pitch - clamp -(loopTumbleRate * dt) (loopTumbleRate * dt) state.pitch

            else
                clamp -0.75 0.75 (state.pitch + nextPitchRate * dt)

        -- Spin each wheel cleanly from longitudinal wheel speed
        wheelSpinDelta w =
            (vFwd / max 0.08 w.radiusM) * dt

        nextWheelSpins =
            List.map2 (\ang w -> ang + wheelSpinDelta w) state.wheelSpinAngles spec.wheels

        -- Re-sample terrain and resolve obstacles & containment walls
        postTerrain =
            Track.sampleAtHinted centerTerrain.nearestS track (Vec3.getX prelimPos) (Vec3.getZ prelimPos)

        nextCooldown =
            max 0.0 (state.lastImpactCooldown - dt)

        obsRes =
            resolveObstacles
                (Track.obstaclesNear track postTerrain.nearestS 24.0)
                halfTrack
                spec
                mass
                nextCooldown
                prelimPos
                prelimVel

        -- Valley cliff containment walls outside shoulder runoff
        maxCorridor =
            postTerrain.halfWidthM + Track.recoveryCorridorM

        wallRes =
            resolveValleyContainment maxCorridor postTerrain mass obsRes.pos obsRes.vel

        finalPos =
            wallRes.pos

        finalVel =
            wallRes.vel

        obsCooldown =
            obsRes.cooldown

        totalDamage =
            (obsRes.damage + wallRes.damage + vertical.landingDamage) * damageScale

        nextIntegrity =
            max 0.0 (state.integrity - totalDamage)

        ( nextPopThreshold, popEvents ) =
            if nextIntegrity <= state.nextPopThreshold && totalDamage > 0.0 then
                ( max 0.0 (state.nextPopThreshold - 20.0)
                , [ BrickDetached { remainingIntegrity = nextIntegrity } ]
                )

            else
                ( state.nextPopThreshold, [] )

        -- Checkpoints & finish gate progression
        prevS =
            state.progressS

        newS =
            max prevS postTerrain.nearestS

        ( nextCpIndex, nextAnchor, cpEvents ) =
            checkCheckpoints track state.checkpointIndex state.checkpointAnchor prevS newS finalPos nextYaw

        -- Airborne and rollover tracking
        nextAirborneTime =
            if groundedWheels == 0 then
                state.airborneTime + dt

            else
                0.0

        nextRolloverTimer =
            if abs nextRoll > 1.15 then
                state.rolloverTimer + dt

            else
                max 0.0 (state.rolloverTimer - dt * 2.0)

        crashEvents =
            if nextIntegrity <= 0.0 then
                [ Crashed StructuralCollapse ]

            else if nextRolloverTimer > 0.75 then
                [ Crashed RolloverTumble ]

            else if Vec3.getY finalPos < postTerrain.height - 28.0 then
                [ Crashed OffMountainDrop ]

            else
                []

        scrapeEvents =
            if bellyScrape && isGroundedNow && speed > 4.5 && obsCooldown <= 0.0 && abs postTerrain.roadOffsetM <= postTerrain.halfWidthM then
                []

            else
                []

        allEvents =
            landingImpact ++ obsRes.events ++ wallRes.events ++ scrapeEvents ++ popEvents ++ cpEvents ++ crashEvents

        nextState =
            { state
                | position = finalPos
                , velocity = finalVel
                , yaw = nextYaw
                , yawRate = nextYawRate
                , pitch = nextPitch
                , pitchRate = nextPitchRate
                , roll = nextRoll
                , rollRate = nextRollRate
                , steerAngle = nextSteerAngle
                , leanInput = { x = leanLat, y = input.aeroTuck - input.brake }
                , wheelSpinAngles = nextWheelSpins
                , suspensionCompressions = List.map (\p -> clamp 0.0 0.35 (p.compression + 0.08)) probes
                , groundedWheels = groundedWheels
                , airborneTime = nextAirborneTime
                , surfaceUnderCar = postTerrain.surface
                , integrity = nextIntegrity
                , chassisScraping = bellyScrape && isGroundedNow
                , offTrack = abs postTerrain.roadOffsetM > postTerrain.halfWidthM + 0.5
                , highSpeedBonus = prevWasAirborne && List.length landingImpact > 0
                , rolloverTimer = nextRolloverTimer
                , progressS = newS
                , checkpointIndex = nextCpIndex
                , checkpointAnchor = nextAnchor
                , lastImpactCooldown = obsCooldown
                , nextPopThreshold = nextPopThreshold
                , gradeRatio = max 0.0 -(Vec3.getY postTerrain.nearestTangent)
            }
    in
    ( nextState, allEvents )


{-| Speed (m/s) at which a car off the road is eased back toward it.

Zero on the road and its verge, while airborne, and at speed: only a car that is
slow and well out in the field gets the gentle tow, so it never drives the car
along the road. Grows smoothly with the distance from the road edge and fades
out as the car picks up speed. Pure and deterministic.

-}
recoveryTowSpeed : { roadOffsetM : Float, halfWidthM : Float, speed : Float, grounded : Bool } -> Float
recoveryTowSpeed { roadOffsetM, halfWidthM, speed, grounded } =
    let
        beyondVerge =
            abs roadOffsetM - halfWidthM - Track.vergeWidthM

        farFactor =
            clamp 0.0 1.0 (beyondVerge / 4.0)

        slowFactor =
            clamp 0.0 1.0 ((recoveryTowMaxCarSpeed - speed) / 3.0)
    in
    if not grounded || beyondVerge <= 0.0 then
        0.0

    else
        recoveryTowTopSpeed * farFactor * slowFactor


{-| Fastest tow toward the road (m/s).
-}
recoveryTowTopSpeed : Float
recoveryTowTopSpeed =
    2.0


{-| Car speed (m/s) above which the tow is off.
-}
recoveryTowMaxCarSpeed : Float
recoveryTowMaxCarSpeed =
    6.0


{-| How far (m) the ground may drop below the car's free-fall path in one step
before the car stops following it and flies. Smooth hills never drop this fast;
the vertical back face of a brick ramp does.
-}
groundFallAwayM : Float
groundFallAwayM =
    0.1


{-| Add the brick ramps standing on the road to a ground sample's height, and
the rounded side ramps to its height and its slope (so gravity turns a car on
one back towards the road).
-}
withBrickRamps : GeneratedTrack -> Racer.Types.TerrainSample -> Racer.Types.TerrainSample
withBrickRamps track smp =
    let
        ramp =
            Track.brickRampLift track smp.nearestS smp.roadOffsetM

        side =
            Track.sideRampLift track smp.nearestS smp.roadOffsetM

        withSide =
            if side.lift > 0.0 then
                let
                    tanFlat =
                        normalizeSafe (vec3 (Vec3.getX smp.nearestTangent) 0 (Vec3.getZ smp.nearestTangent)) (vec3 0 0 1)

                    rightFlat =
                        vec3 -(Vec3.getZ tanFlat) 0 (Vec3.getX tanFlat)

                    -- The normal as (-dh/dx, 1, -dh/dz), less the side ramp's gradient.
                    slopeForm =
                        Vec3.scale (1.0 / max 0.05 (Vec3.getY smp.normal)) smp.normal
                            |> Vec3.add (Vec3.scale -side.sideGrade rightFlat)
                            |> Vec3.add (Vec3.scale -side.grade tanFlat)
                in
                { smp
                    | height = smp.height + side.lift
                    , normal = normalizeSafe slopeForm smp.normal
                }

            else
                smp
    in
    if ramp.lift > 0.0 then
        { withSide | height = withSide.height + ramp.lift }

    else
        withSide



-- ── Brick loops ──────────────────────────────────────────────────────────────


{-| Rolling resistance of a loop's smooth brick lane, per unit support force.
-}
loopRollingResist : Float
loopRollingResist =
    0.02


{-| How fast (rad/s) a car that fell off a loop upside down turns back over.
-}
loopTumbleRate : Float
loopTumbleRate =
    5.0


{-| After a step on the ground: a car that crossed a loop's entry on its wheels,
on the road and rolling forward, starts riding the loop's lane.
-}
enterLoop : GeneratedTrack -> PhysicsState -> ( PhysicsState, List PhysicsEvent ) -> ( PhysicsState, List PhysicsEvent )
enterLoop track before ( after, events ) =
    let
        crossed =
            List.indexedMap Tuple.pair track.loops
                |> List.filter (\( _, lp ) -> before.progressS < lp.entryS && after.progressS >= lp.entryS)
                |> List.head
    in
    case crossed of
        Just ( idx, loop ) ->
            let
                ground =
                    Track.sampleAtHinted after.progressS track (Vec3.getX after.position) (Vec3.getZ after.position)

                entryFrame =
                    Track.loopFrame track loop 0.0 ground.roadOffsetM

                speedAlong =
                    Vec3.dot after.velocity entryFrame.tangent

                onRoad =
                    abs ground.roadOffsetM <= loop.apronHalfWidthM + 0.3

                -- On its wheels, or only skipping over a bump.
                nearGround =
                    Vec3.getY after.position - ground.height < 0.35
            in
            if nearGround && onRoad && speedAlong > 2.0 then
                let
                    ride =
                        { loopIndex = idx
                        , theta = clamp 0.0 0.3 ((ground.nearestS - loop.entryS) / entryFrame.arcPerRad)
                        , lateralM = ground.roadOffsetM
                        , speed = speedAlong
                        }
                in
                ( poseOnLoop track loop ride after, events )

            else
                ( after, events )

        Nothing ->
            ( after, events )


{-| One fixed step riding round a loop's lane.

The car is a point on the lane surface moving along it: gravity along the lane,
aero drag and rolling resistance on the support force change its speed. The
support force the lane must give (per unit mass) is `v^2 * curvature + g * up_y`;
where that would have to be negative (too slow over the top) the car falls off.
The lane narrows from the full road at the entry, so the car is squeezed into
it. Steering and brakes do nothing on the lane.

-}
stepOnLoop :
    Float
    -> GeneratedTrack
    -> VehicleSpec
    -> InputState
    -> LoopSection
    -> LoopRide
    -> PhysicsState
    -> ( PhysicsState, List PhysicsEvent )
stepOnLoop dt track spec input loop ride state =
    let
        mass =
            max 12.0 spec.totalMassKg

        halfTrack =
            max 0.25 (spec.trackWidthM * 0.5)

        frame =
            Track.loopFrame track loop ride.theta ride.lateralM

        v =
            ride.speed

        support =
            v * v * frame.curvature + gravityAccel * Vec3.getY frame.up

        tuckMult =
            1.0 - 0.18 * clamp 0.0 1.0 input.aeroTuck

        dragAccel =
            0.5 * airDensity * spec.dragCoeff * spec.frontalAreaM2 * tuckMult * v * v / mass

        resist =
            if abs v > 0.05 then
                (dragAccel + loopRollingResist * max 0.0 support)
                    * (if v > 0.0 then
                        1.0

                       else
                        -1.0
                      )

            else
                0.0

        nextSpeed =
            v + (-gravityAccel * Vec3.getY frame.tangent - resist) * dt

        nextTheta =
            ride.theta + nextSpeed * dt / frame.arcPerRad

        maxLat =
            max 0.0 (Track.loopLaneHalfWidth loop (clamp 0.0 (2.0 * pi) nextTheta) - halfTrack)

        nextRide =
            { ride
                | theta = clamp 0.0 (2.0 * pi) nextTheta
                , lateralM = clamp -maxLat maxLat ride.lateralM
                , speed = nextSpeed
            }

        nextFrame =
            Track.loopFrame track loop nextRide.theta nextRide.lateralM

        nextSupport =
            nextSpeed * nextSpeed * nextFrame.curvature + gravityAccel * Vec3.getY nextFrame.up

        spun =
            { state
                | wheelSpinAngles =
                    List.map2 (\ang w -> ang + (nextSpeed / max 0.08 w.radiusM) * dt) state.wheelSpinAngles spec.wheels
                , steerAngle = state.steerAngle * 0.9
                , leanInput = { x = 0.0, y = input.aeroTuck - input.brake }
            }

        posed =
            poseOnLoop track loop nextRide spun

        newS =
            max state.progressS (loop.entryS + (loop.exitS - loop.entryS) * nextRide.theta / (2.0 * pi))

        ( nextCpIndex, nextAnchor, cpEvents ) =
            checkCheckpoints track state.checkpointIndex state.checkpointAnchor state.progressS newS posed.position posed.yaw

        progressed =
            { posed | progressS = newS, checkpointIndex = nextCpIndex, checkpointAnchor = nextAnchor }

        wheelCount =
            max 1 (List.length spec.wheels)
    in
    if nextTheta >= 2.0 * pi || nextTheta <= 0.0 then
        -- Out of the exit onto the road, or rolled back out of the entry.
        ( { progressed | loopRide = Nothing, groundedWheels = wheelCount }, cpEvents )

    else if nextSupport < 0.0 then
        -- Too slow to be held on: off the lane, into the air.
        ( { progressed
            | loopRide = Nothing
            , position = Vec3.add progressed.position (Vec3.scale 0.05 nextFrame.up)
            , groundedWheels = 0
            , airborneTime = dt
          }
        , cpEvents
        )

    else
        ( progressed, cpEvents )


{-| Put the car on a loop's lane where `ride` says, facing along it, with the
lane under its wheels.
-}
poseOnLoop : GeneratedTrack -> LoopSection -> LoopRide -> PhysicsState -> PhysicsState
poseOnLoop track loop ride state =
    let
        frame =
            Track.loopFrame track loop ride.theta ride.lateralM

        -- The car's world matrix is T * Ry(yaw) * Rz(roll) * Rx(pitch), with
        -- its left along local +X, up along +Y and forward along +Z.
        left =
            Vec3.negate frame.right
    in
    { state
        | position = frame.position
        , velocity = Vec3.scale ride.speed frame.tangent
        , yaw = atan2 -(Vec3.getZ left) (Vec3.getX left)
        , yawRate = 0.0
        , roll = asin (clamp -1.0 1.0 (Vec3.getY left))
        , rollRate = 0.0
        , pitch = atan2 -(Vec3.getY frame.tangent) (Vec3.getY frame.up)
        , pitchRate = 0.0
        , groundedWheels = max 1 (List.length state.wheelSpinAngles)
        , airborneTime = 0.0
        , chassisScraping = False
        , offTrack = False
        , highSpeedBonus = False
        , rolloverTimer = 0.0
        , gradeRatio = 0.0
        , loopRide = Just ride
    }


{-| Horizontal forward unit vector `(sin yaw, 0, cos yaw)` at vehicle heading `yaw` radians.
-}
forwardVector : Float -> Vec3
forwardVector yaw =
    vec3 (sin yaw) 0.0 (cos yaw)


{-| Horizontal driver-right unit vector `(-cos yaw, 0, sin yaw)` orthogonal to `forwardVector yaw`.
-}
rightVector : Float -> Vec3
rightVector yaw =
    vec3 -(cos yaw) 0.0 (sin yaw)


{-| Lateral tire grip multiplier for each unpaved surface type.

Loose countryside surfaces grip less than a paved road would, so steep descents
slide and feel a bit out of control.

-}
surfaceGripMultiplier : SurfaceType -> Float
surfaceGripMultiplier surface =
    case surface of
        DirtRoad ->
            0.95

        ForestTrack ->
            0.88

        GravelRoad ->
            0.78

        GrassPath ->
            0.72

        RoughVerge ->
            0.6

        MeadowGrass ->
            0.5


{-| Rolling resistance coefficient for each unpaved surface type.
-}
surfaceRollingResist : SurfaceType -> Float
surfaceRollingResist surface =
    case surface of
        DirtRoad ->
            0.022

        ForestTrack ->
            0.045

        GravelRoad ->
            0.04

        GrassPath ->
            0.055

        RoughVerge ->
            0.09

        MeadowGrass ->
            0.14



-- ── Internal Collision, Containment & Checkpoint Helpers ──────────────────────


type alias ObstacleRes =
    { pos : Vec3
    , vel : Vec3
    , damage : Float
    , events : List PhysicsEvent
    , cooldown : Float
    }


type alias WallRes =
    { pos : Vec3
    , vel : Vec3
    , damage : Float
    , events : List PhysicsEvent
    }


{-| The scenery another racer smashes, seen only through what the room shares
of their car: its position, heading (`yaw`), speed and distance down the
course (`progressS`). Their own physics smashes the piece on their screen; this
finds the same pieces here, so a smash shows on every screen.

A piece counts when the car is inside it while going at least its break speed:
a car that only bumps a piece is pushed back out of it by its own physics, so
only a car that ploughs through one is seen inside it. The car's radius is the
one `resolveObstacles` uses, a little smaller, since `spec` is only the look
this page has for that car.

-}
remoteSmashes :
    GeneratedTrack
    -> VehicleSpec
    -> { position : Vec3, yaw : Float, speed : Float, progressS : Float }
    -> List PhysicsEvent
remoteSmashes track spec car =
    let
        halfTrack =
            max 0.25 (spec.trackWidthM * 0.5)

        carRadius =
            0.85 * clamp 0.52 1.15 (max halfTrack (spec.wheelBaseM * 0.35))

        carY =
            Vec3.getY car.position

        carTopY =
            carY + max 0.45 (Vec3.getY spec.boundsMaxLdu / 160.0)

        direction =
            forwardVector car.yaw

        inside obs =
            let
                dx =
                    Vec3.getX car.position - Vec3.getX obs.position

                dz =
                    Vec3.getZ car.position - Vec3.getZ obs.position

                minDist =
                    carRadius + obs.radiusM
            in
            (carY <= Vec3.getY obs.position + obs.heightM)
                && (carTopY >= Vec3.getY obs.position - 0.5)
                && (dx * dx + dz * dz < minDist * minDist)
                && (car.speed >= breakSpeedFor obs.kind)
    in
    Track.obstaclesNear track car.progressS 24.0
        |> List.filter inside
        |> List.map
            (\obs ->
                ScenerySmashed
                    { obstacleId = obs.id
                    , impactSpeed = car.speed
                    , direction = direction
                    }
            )


{-| Minimum impact speed (m/s) at which a scenery piece shatters instead of
bouncing the racer off. Light pieces break on any real knock; trees, rocks and
buildings need a fast hit.
-}
breakSpeedFor : ObstacleKind -> Float
breakSpeedFor kind =
    case kind of
        Bush ->
            1.0

        FenceSection ->
            1.5

        Signpost ->
            1.5

        HayBale ->
            2.0

        LeafyTree ->
            4.0

        PineTree ->
            4.0

        Rock ->
            5.5

        Cottage ->
            7.0

        Barn ->
            7.5

        Crate ->
            1.2

        Barrel ->
            2.5

        GardenGnome ->
            0.8

        BrickWall ->
            6.0

        BrickPallet ->
            2.0

        TileStack ->
            1.0

        BrickGate ->
            3.0

        SlopeBlock ->
            3.5

        CountdownBoard ->
            1.5

        Chalet ->
            7.0

        PitGarage ->
            6.0


{-| Fraction of the racer's speed soaked up when ploughing through a piece.
-}
sceneryMassFactor : ObstacleKind -> Float
sceneryMassFactor kind =
    case kind of
        Bush ->
            0.06

        FenceSection ->
            0.12

        Signpost ->
            0.06

        HayBale ->
            0.2

        LeafyTree ->
            0.3

        PineTree ->
            0.3

        Rock ->
            0.4

        Cottage ->
            0.45

        Barn ->
            0.5

        Crate ->
            0.14

        Barrel ->
            0.24

        GardenGnome ->
            0.04

        BrickWall ->
            0.36

        BrickPallet ->
            0.2

        TileStack ->
            0.1

        BrickGate ->
            0.22

        SlopeBlock ->
            0.28

        CountdownBoard ->
            0.08

        Chalet ->
            0.45

        PitGarage ->
            0.4


{-| Full health (structural integrity) of a fresh or rebuilt car. At 0 the car
breaks apart into its bricks (`Crashed StructuralCollapse`).
-}
maxHealth : Float
maxHealth =
    100.0


{-| Every point of damage taken is multiplied by this, so a few hard hits
break the car apart rather than wearing it down forever.
-}
damageScale : Float
damageScale =
    2.2


{-| Health lost bouncing off scenery that does not break (a tree, a cottage)
at `impactSpeed` m/s into it. Bumps up to 5 m/s are free, so brushing past
things never wears the car down; a full-speed head-on hit costs about a third.
-}
obstacleImpactDamage : Float -> Float
obstacleImpactDamage impactSpeed =
    if impactSpeed > 2.5 then
        clamp 5.0 48.0 ((impactSpeed - 2.5) * 4.2)

    else
        0.0


{-| Health lost landing from the air at `impactVel` m/s into the ground. Small
hops and the brick ramps' normal landings at racing speed (up to about 5 m/s
into the ground) are free; dropping in from high up, such as off the top of a
loop, costs up to 36.
-}
landingDamage : Float -> Float
landingDamage impactVel =
    if impactVel > 5.5 then
        clamp 4.0 36.0 ((impactVel - 5.5) * 4.0)

    else
        0.0


{-| Structural integrity a smash always costs, on top of the speed-dependent
damage of the heavy pieces: an oil drum dents the bodywork, a ceramic gnome
chips it, a brick wall, pallet, gate or slope block knocks bricks off.
-}
smashChipDamage : ObstacleKind -> Float
smashChipDamage kind =
    case kind of
        Barrel ->
            8.0

        GardenGnome ->
            4.0

        BrickWall ->
            16.0

        BrickPallet ->
            6.0

        BrickGate ->
            6.0

        SlopeBlock ->
            10.0

        CountdownBoard ->
            3.0

        Chalet ->
            16.0

        PitGarage ->
            14.0

        _ ->
            0.0


resolveObstacles :
    List TrackObstacle
    -> Float
    -> VehicleSpec
    -> Float
    -> Float
    -> Vec3
    -> Vec3
    -> ObstacleRes
resolveObstacles obsList halfTrack spec _ cooldown pos vel =
    let
        carRadius =
            clamp 0.52 1.15 (max halfTrack (spec.wheelBaseM * 0.35))

        carTopY =
            Vec3.getY pos + max 0.45 (Vec3.getY spec.boundsMaxLdu / 160.0)

        step obs acc =
            let
                obsTop =
                    Vec3.getY obs.position + obs.heightM

                obsBottom =
                    Vec3.getY obs.position - 0.5
            in
            if Vec3.getY acc.pos > obsTop || carTopY < obsBottom then
                acc

            else
                let
                    dx =
                        Vec3.getX acc.pos - Vec3.getX obs.position

                    dz =
                        Vec3.getZ acc.pos - Vec3.getZ obs.position

                    minDist =
                        carRadius + obs.radiusM

                    distSq =
                        dx * dx + dz * dz
                in
                if distSq >= minDist * minDist || distSq < 1.0e-8 then
                    acc

                else
                    let
                        dist =
                            sqrt distSq

                        nx =
                            dx / dist

                        nz =
                            dz / dist

                        penetration =
                            minDist - dist + 0.02

                        pushedPos =
                            vec3 (Vec3.getX acc.pos + nx * penetration) (Vec3.getY acc.pos) (Vec3.getZ acc.pos + nz * penetration)

                        vn =
                            Vec3.getX acc.vel * nx + Vec3.getZ acc.vel * nz

                        impactSpeed =
                            -vn

                        impactEvent =
                            if impactSpeed > 1.8 && acc.cooldown <= 0.0 then
                                [ Impact { impulse = clamp 0.1 1.0 (impactSpeed / 16.0), scrape = False } ]

                            else
                                []

                        nextCd =
                            if List.isEmpty impactEvent then
                                acc.cooldown

                            else
                                0.22
                    in
                    if vn < 0.0 && impactSpeed >= breakSpeedFor obs.kind then
                        -- Smash: the piece shatters, the racer ploughs on, slowed by its mass.
                        let
                            massFactor =
                                sceneryMassFactor obs.kind

                            keep =
                                1.0 - massFactor

                            newVel =
                                vec3 (Vec3.getX acc.vel * keep) (Vec3.getY acc.vel) (Vec3.getZ acc.vel * keep)

                            dmg =
                                smashChipDamage obs.kind
                                    + (if massFactor >= 0.3 && impactSpeed > 8.0 then
                                        clamp 0.0 25.0 ((impactSpeed - 8.0) * 2.5)

                                       else
                                        0.0
                                      )

                            horizDir =
                                normalizeSafe (vec3 (Vec3.getX acc.vel) 0 (Vec3.getZ acc.vel)) (vec3 -nx 0 -nz)
                        in
                        { pos = acc.pos
                        , vel = newVel
                        , damage = acc.damage + dmg
                        , events =
                            acc.events
                                ++ impactEvent
                                ++ [ ScenerySmashed { obstacleId = obs.id, impactSpeed = impactSpeed, direction = horizDir } ]
                        , cooldown = nextCd
                        }

                    else if vn < 0.0 then
                        let
                            restitution =
                                0.32

                            deltaVn =
                                -(1.0 + restitution) * vn

                            nextVx =
                                Vec3.getX acc.vel + nx * deltaVn * 0.92

                            nextVz =
                                Vec3.getZ acc.vel + nz * deltaVn * 0.92

                            newVel =
                                vec3 nextVx (Vec3.getY acc.vel * 0.85) nextVz

                            dmg =
                                if acc.cooldown <= 0.0 then
                                    obstacleImpactDamage impactSpeed

                                else
                                    0.0
                        in
                        { pos = pushedPos
                        , vel = newVel
                        , damage = acc.damage + dmg
                        , events = acc.events ++ impactEvent
                        , cooldown = nextCd
                        }

                    else
                        { acc | pos = pushedPos }
    in
    List.foldl step { pos = pos, vel = vel, damage = 0.0, events = [], cooldown = cooldown } obsList


resolveValleyContainment :
    Float
    -> Racer.Types.TerrainSample
    -> Float
    -> Vec3
    -> Vec3
    -> WallRes
resolveValleyContainment maxCorridor terrain _ pos vel =
    if abs terrain.roadOffsetM <= maxCorridor then
        { pos = pos, vel = vel, damage = 0.0, events = [] }

    else
        let
            tanXZ =
                normalizeSafe (vec3 (Vec3.getX terrain.nearestTangent) 0 (Vec3.getZ terrain.nearestTangent)) (vec3 0 0 1)

            rightXZ =
                vec3 -(Vec3.getZ tanXZ) 0 (Vec3.getX tanXZ)

            excess =
                abs terrain.roadOffsetM - maxCorridor

            signD =
                if terrain.roadOffsetM >= 0.0 then
                    1.0

                else
                    -1.0

            inwardNormal =
                Vec3.scale -signD rightXZ

            clampedPos =
                Vec3.add pos (Vec3.scale excess inwardNormal)

            outwardVel =
                -(Vec3.getX vel * Vec3.getX inwardNormal + Vec3.getZ vel * Vec3.getZ inwardNormal)
        in
        if outwardVel > 0.0 then
            let
                bouncedVel =
                    Vec3.add vel (Vec3.scale (outwardVel * 1.25) inwardNormal)

                dmg =
                    if outwardVel > 4.0 then
                        clamp 4.0 25.0 ((outwardVel - 4.0) * 2.5)

                    else
                        0.0

                evs =
                    if outwardVel > 2.0 then
                        [ Impact { impulse = clamp 0.1 1.0 (outwardVel / 15.0), scrape = True } ]

                    else
                        []
            in
            { pos = clampedPos, vel = bouncedVel, damage = dmg, events = evs }

        else
            { pos = clampedPos, vel = vel, damage = 0.0, events = [] }


checkCheckpoints :
    GeneratedTrack
    -> Int
    -> { s : Float, position : Vec3, yaw : Float }
    -> Float
    -> Float
    -> Vec3
    -> Float
    -> ( Int, { s : Float, position : Vec3, yaw : Float }, List PhysicsEvent )
checkCheckpoints track currIdx currAnchor prevS newS pos _ =
    let
        process cp ( idxAcc, anchAcc, evAcc ) =
            if cp.index >= idxAcc && prevS < cp.s && newS >= cp.s then
                if cp.isFinish then
                    ( cp.index + 1
                    , anchAcc
                    , evAcc ++ [ CheckpointCrossed { index = cp.index, splitTimeS = 0.0 }, FinishedRace ]
                    )

                else
                    let
                        anchorYaw =
                            atan2 (Vec3.getX cp.tangent) (Vec3.getZ cp.tangent)

                        cpAnchor =
                            { s = cp.s
                            , position = vec3 (Vec3.getX cp.center) (Vec3.getY pos + 0.05) (Vec3.getZ cp.center)
                            , yaw = anchorYaw
                            }
                    in
                    ( cp.index + 1
                    , cpAnchor
                    , evAcc ++ [ CheckpointCrossed { index = cp.index, splitTimeS = 0.0 } ]
                    )

            else
                ( idxAcc, anchAcc, evAcc )

        ( nextIdx, nextAnch, evs ) =
            List.foldl process ( currIdx, currAnchor, [] ) track.checkpoints
    in
    if not (List.member FinishedRace evs) && prevS < track.finishS && newS >= track.finishS then
        ( nextIdx, nextAnch, evs ++ [ FinishedRace ] )

    else
        ( nextIdx, nextAnch, evs )


normalizeAngle : Float -> Float
normalizeAngle a =
    let
        twoPi =
            2.0 * pi
    in
    if a > pi then
        normalizeAngle (a - twoPi)

    else if a < -pi then
        normalizeAngle (a + twoPi)

    else
        a


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
