module PlayFeelTest exposing (suite)

{-| Tests for the phone play fixes: tilt steering clears the touch pads, the
chase camera keeps the car big and whole in view.
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
import Racer.Types exposing (CameraMode(..), PhysicsState, TrackId(..), VehicleSpec, lduToMeters)
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


farmLane : Racer.Types.GeneratedTrack
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
        ]
