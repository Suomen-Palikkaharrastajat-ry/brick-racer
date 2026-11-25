module PlayFeelTest exposing (suite)

{-| Tests for the phone play fixes: tilt steering clears the touch pads, the
chase camera keeps the car big and whole in view, the ground leads back to the
road, and the car's health wears down fairly until it breaks into bricks.
-}

import Dict
import Expect
import LDraw.Catalog as Catalog
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Analyze as Analyze
import Racer.Camera as RacerCamera
import Racer.HUD as HUD
import Racer.Ingest as Ingest
import Racer.Physics as Physics
import Racer.Render as Render
import Racer.Track as Track
import Racer.Types exposing (CameraMode(..), CrashReason(..), GeneratedTrack, PhysicsEvent(..), PhysicsState, TrackId(..), VehicleSpec, lduToMeters)
import Render.Camera as Camera
import Test exposing (Test, describe, test)


presetSpec : String -> VehicleSpec
presetSpec presetId =
    let
        ldr =
            Ingest.findPresetById presetId
                |> Maybe.map .ldrText
                |> Maybe.withDefault "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
    in
    Analyze.analyzeDocument presetId (Document.parse ldr) Dict.empty Catalog.emptyPartIndex
        |> .spec


farmLane : GeneratedTrack
farmLane =
    Track.generate (Track.trackConfigFor FarmLane 1)


{-| Let the chase camera settle behind a car moving at `speed` on a slope of
`grade`, then return each corner of the car's bounding box in clip space
(x, y in -1 .. 1 when on screen) for a phone-landscape aspect.
-}
carCornersOnScreen : Float -> VehicleSpec -> Float -> Float -> List ( Float, Float )
carCornersOnScreen aspect spec speed grade =
    let
        base =
            Physics.initPhysics farmLane spec

        fwd =
            Physics.forwardVector base.yaw

        physics =
            { base
                | velocity = Vec3.scale speed fwd
                , gradeRatio = grade
                , pitch = -(atan grade)
            }

        settle n c =
            if n <= 0 then
                c

            else
                settle (n - 1) (RacerCamera.updateRaceCamera (1 / 60) ChaseCam physics spec c)

        cam =
            settle 600 (RacerCamera.initRaceCamera 390 physics spec)

        viewProj =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane cam) (Camera.farPlane cam))
                (Camera.viewMatrix cam)

        world =
            Render.buildCarWorldMatrix physics

        lo =
            spec.boundsMinLdu

        hi =
            spec.boundsMaxLdu

        corners =
            List.concatMap
                (\x ->
                    List.concatMap
                        (\y -> List.map (\z -> vec3 (lduToMeters x) (lduToMeters y) (lduToMeters z)) [ Vec3.getZ lo, Vec3.getZ hi ])
                        [ Vec3.getY lo, Vec3.getY hi ]
                )
                [ Vec3.getX lo, Vec3.getX hi ]

        project p =
            let
                w =
                    Mat4.transform world p

                clip =
                    Mat4.transform viewProj w
            in
            ( Vec3.getX clip, Vec3.getY clip )
    in
    List.map project corners


{-| Off-road points up to 12 m beyond the road edge where the ground does not
slope back down toward the road: (s, offset) pairs. Further out, on the inside
of tight bends the road's own grade can win; the recovery tow covers that.
-}
pointsSlopingAway : GeneratedTrack -> List ( ( Float, Float ), ( Float, Float, Float ) )
pointsSlopingAway track =
    let
        stations =
            List.range 1 (floor (track.totalLengthM / 8.0) - 1)
                |> List.map (\i -> toFloat i * 8.0)

        check s side extra =
            let
                smp =
                    Track.splineSampleAtS track s

                offset =
                    side * (smp.halfWidth + extra)

                p =
                    Vec3.add smp.center (Vec3.scale offset smp.right)

                t =
                    Track.sampleAtHinted s track (Vec3.getX p) (Vec3.getZ p)

                toRoad =
                    vec3 (Vec3.getX t.nearestCenter - Vec3.getX p) 0 (Vec3.getZ t.nearestCenter - Vec3.getZ p)

                downhillToRoad =
                    Vec3.getX t.normal * Vec3.getX toRoad + Vec3.getZ t.normal * Vec3.getZ toRoad
            in
            if abs t.roadOffsetM > t.halfWidthM + 0.5 && downhillToRoad <= 0.0 then
                -- Where, how much it slopes away, and which road point is nearest.
                Just ( ( s, offset ), ( downhillToRoad, t.nearestS - s, t.roadOffsetM ) )

            else
                Nothing
    in
    List.concatMap
        (\s ->
            List.concatMap (\side -> List.filterMap (check s side) [ 1.0, 3.0, 6.0, 9.0, 12.0 ]) [ -1.0, 1.0 ]
        )
        stations


{-| The car dropped onto the start of the farm lane from the air, falling at
`fallSpeed` m/s, with `health` left. Returns the state and events after one step.
-}
landFrom : Float -> Float -> ( PhysicsState, List PhysicsEvent )
landFrom fallSpeed health =
    let
        spec =
            presetSpec "turntable-kart"

        start =
            Physics.initPhysics farmLane spec

        airborne =
            { start
                | position = Vec3.add start.position (vec3 0 0.05 0)
                , velocity = vec3 0 -fallSpeed 0
                , groundedWheels = 0
                , airborneTime = 0.8
                , integrity = health
            }
    in
    Physics.stepFixed (1 / 60) farmLane spec { steer = 0, brake = 0, handbrake = False, aeroTuck = 0, leanX = 0 } airborne


screenHeightShare : List ( Float, Float ) -> Float
screenHeightShare pts =
    let
        ys =
            List.map Tuple.second pts
    in
    (Maybe.withDefault 0 (List.maximum ys) - Maybe.withDefault 0 (List.minimum ys)) / 2.0


suite : Test
suite =
    describe "Phone play"
        [ describe "Touch pads and tilt"
            [ test "pads show on a phone steering with buttons" <|
                \_ ->
                    HUD.touchPadsVisible { showTouchControls = True, tiltEnabled = False, tiltAvailable = True }
                        |> Expect.equal True
            , test "pads hide while tilt steering is on" <|
                \_ ->
                    HUD.touchPadsVisible { showTouchControls = True, tiltEnabled = True, tiltAvailable = True }
                        |> Expect.equal False
            , test "pads come back when tilt cannot be used" <|
                \_ ->
                    HUD.touchPadsVisible { showTouchControls = True, tiltEnabled = True, tiltAvailable = False }
                        |> Expect.equal True
            , test "pads stay hidden on desktop" <|
                \_ ->
                    HUD.touchPadsVisible { showTouchControls = False, tiltEnabled = False, tiltAvailable = False }
                        |> Expect.equal False
            ]
        , describe "Chase camera"
            [ test "speed pulls the camera back only a little" <|
                \_ ->
                    (RacerCamera.chaseDistance 1.0 45.0 - RacerCamera.chaseDistance 1.0 0.0)
                        |> Expect.atMost 0.5
            , test "distance never shrinks as speed grows" <|
                \_ ->
                    List.map (\v -> RacerCamera.chaseDistance 0.9 (toFloat v)) (List.range 0 50)
                        |> (\ds -> List.map2 (<=) ds (List.drop 1 ds))
                        |> List.all identity
                        |> Expect.equal True
            , test "the camera sits much closer than before at speed" <|
                \_ ->
                    -- The old rule was clamp 3.2 7.2 (2.6 + 2 r + 0.05 v).
                    RacerCamera.chaseDistance 0.9 25.0
                        |> Expect.lessThan (0.8 * clamp 3.2 7.2 (2.6 + 0.9 * 2.0 + 25.0 * 0.05))
            , describe "the whole car stays in view on a phone"
                (List.concatMap
                    (\presetId ->
                        List.map
                            (\( speed, grade ) ->
                                test (presetId ++ " at " ++ String.fromFloat speed ++ " m/s, grade " ++ String.fromFloat grade) <|
                                    \_ ->
                                        carCornersOnScreen (844 / 390) (presetSpec presetId) speed grade
                                            |> List.filter (\( x, y ) -> abs x > 0.97 || abs y > 0.97)
                                            |> Expect.equal []
                            )
                            [ ( 0, 0 ), ( 12, 0.3 ), ( 30, 0.45 ), ( 30, 0 ) ]
                    )
                    (List.map .id Ingest.presets)
                )
            , test "the car fills a good share of the view height" <|
                \_ ->
                    carCornersOnScreen (844 / 390) (presetSpec "turntable-kart") 15 0.3
                        |> screenHeightShare
                        |> Expect.atLeast 0.2
            ]
        , describe "Getting back onto the road"
            [ describe "the ground beside the road slopes back toward it"
                (List.map
                    (\( name, trackId ) ->
                        test name <|
                            \_ ->
                                List.range 1 3
                                    |> List.concatMap (\seed -> pointsSlopingAway (Track.generate (Track.trackConfigFor trackId seed)))
                                    |> Expect.equal []
                    )
                    [ ( "Farm lane", FarmLane ), ( "Forest trail", ForestTrail ), ( "Meadow path", MeadowPath ) ]
                )
            , test "no tow on the road or its verge" <|
                \_ ->
                    Physics.recoveryTowSpeed { roadOffsetM = 6.5, halfWidthM = 4.4, speed = 0, grounded = True }
                        |> Expect.equal 0
            , test "no tow at speed, so the car never drives itself" <|
                \_ ->
                    Physics.recoveryTowSpeed { roadOffsetM = 20, halfWidthM = 4.4, speed = 9, grounded = True }
                        |> Expect.equal 0
            , test "no tow in the air" <|
                \_ ->
                    Physics.recoveryTowSpeed { roadOffsetM = 20, halfWidthM = 4.4, speed = 0, grounded = False }
                        |> Expect.equal 0
            , test "a slow car out in the field gets a gentle tow" <|
                \_ ->
                    Physics.recoveryTowSpeed { roadOffsetM = -18, halfWidthM = 4.4, speed = 1, grounded = True }
                        |> Expect.all [ Expect.greaterThan 1.0, Expect.atMost 2.0 ]
            , test "the tow grows with the distance from the road" <|
                \_ ->
                    List.map (\d -> Physics.recoveryTowSpeed { roadOffsetM = toFloat d, halfWidthM = 4.4, speed = 0, grounded = True }) (List.range 0 30)
                        |> (\ts -> List.map2 (<=) ts (List.drop 1 ts))
                        |> List.all identity
                        |> Expect.equal True
            , test "a car stopped far out in the field comes back near the road, the same way every time" <|
                \_ ->
                    let
                        spec =
                            presetSpec "turntable-kart"

                        start =
                            Physics.initPhysics farmLane spec

                        smp =
                            Track.splineSampleAtS farmLane 120.0

                        offPos =
                            Vec3.add smp.center (Vec3.scale (smp.halfWidth + 16.0) smp.right)

                        ground =
                            Track.sampleAtHinted 120.0 farmLane (Vec3.getX offPos) (Vec3.getZ offPos)

                        stranded =
                            { start
                                | position = vec3 (Vec3.getX offPos) ground.height (Vec3.getZ offPos)
                                , progressS = 120.0

                                -- Facing across the slope, so gravity alone does not bring it back.
                                , yaw = start.yaw + pi / 2
                            }

                        idle =
                            { steer = 0, brake = 1, handbrake = False, aeroTuck = 0, leanX = 0 }

                        run n st =
                            if n <= 0 then
                                st

                            else
                                run (n - 1) (Tuple.first (Physics.stepFixed (1 / 60) farmLane spec idle st))

                        after =
                            run (60 * 12) stranded

                        offsetAfter =
                            Track.sampleAtHinted after.progressS farmLane (Vec3.getX after.position) (Vec3.getZ after.position)
                                |> (\t -> abs t.roadOffsetM - t.halfWidthM)
                    in
                    Expect.all
                        [ \_ -> Expect.atMost (Track.vergeWidthM + 1.0) offsetAfter
                        , \_ -> Expect.equal after.position (run (60 * 12) stranded).position
                        ]
                        ()
            ]
        , describe "Car health"
            [ test "small bumps cost nothing" <|
                \_ ->
                    ( Physics.obstacleImpactDamage 2.0, Physics.landingDamage 4.0 )
                        |> Expect.equal ( 0, 0 )
            , test "a handful of moderate knocks leaves most of the health" <|
                \_ ->
                    (Physics.maxHealth - 5 * Physics.obstacleImpactDamage 4.0)
                        |> Expect.atLeast (Physics.maxHealth * 0.4)
            , test "one hard crash does not wreck a healthy car" <|
                \_ ->
                    Physics.obstacleImpactDamage 30
                        |> Expect.lessThan (Physics.maxHealth / 2)
            , test "three hard crashes break the car apart" <|
                \_ ->
                    (3 * Physics.obstacleImpactDamage 16)
                        |> Expect.atLeast Physics.maxHealth
            , test "damage grows with the impact speed" <|
                \_ ->
                    List.map (\v -> ( Physics.obstacleImpactDamage (toFloat v), Physics.landingDamage (toFloat v) )) (List.range 0 30)
                        |> (\ds -> List.map2 (\( a, b ) ( c, d ) -> a <= c && b <= d) ds (List.drop 1 ds))
                        |> List.all identity
                        |> Expect.equal True
            , test "a soft landing keeps full health" <|
                \_ ->
                    landFrom 3.0 Physics.maxHealth
                        |> Tuple.first
                        |> .integrity
                        |> Expect.within (Expect.Absolute 1.0e-9) Physics.maxHealth
            , test "a hard landing costs health" <|
                \_ ->
                    landFrom 13.0 Physics.maxHealth
                        |> Tuple.first
                        |> .integrity
                        |> Expect.all [ Expect.lessThan 40.0, Expect.greaterThan 10.0 ]
            , test "a hard landing on the last bit of health wrecks the car" <|
                \_ ->
                    landFrom 13.0 10.0
                        |> Tuple.second
                        |> List.member (Crashed StructuralCollapse)
                        |> Expect.equal True
            , test "the rebuilt car is back at full health" <|
                \_ ->
                    landFrom 13.0 10.0
                        |> Tuple.first
                        |> Physics.respawnAtCheckpoint
                        |> .integrity
                        |> Expect.within (Expect.Absolute 1.0e-9) Physics.maxHealth
            ]
        ]
