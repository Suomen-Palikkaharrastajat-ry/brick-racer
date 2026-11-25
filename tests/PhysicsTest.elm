module PhysicsTest exposing (suite)

{-| Unit tests for `Racer.Physics` invariants: static equilibrium on flat ground,
downhill gravity acceleration, belly-scrape resistance, braking, turntable vs fixed
steering authority, and checkpoint recovery.
-}

import Array
import EmbeddedParts
import Expect
import LDraw.Catalog as Catalog
import LDraw.Document as Document
import Math.Vector3 as Vec3 exposing (vec3)
import Racer.Analyze as Analyze
import Racer.Ingest as Ingest
import Racer.Physics as Physics
import Racer.Track as Track
import Racer.Types
    exposing
        ( GeneratedTrack
        , InputState
        , PhysicsState
        , SurfaceType(..)
        , TrackId(..)
        , TrackSample
        , VehicleSpec
        , fixedDt
        )
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Racer.Physics invariants"
        [ test "Stationary soapbox on completely flat track remains stationary" <|
            \_ ->
                let
                    flatTrack =
                        buildFlatTestTrack 0.0 300.0

                    spec =
                        loadSpec "turntable-kart"

                    s0 =
                        Physics.initPhysics flatTrack spec

                    sAfter =
                        simulateSteps 120 flatTrack spec neutralInput s0
                in
                Expect.all
                    [ \_ -> Expect.lessThan 0.05 (Vec3.length sAfter.velocity)
                    , \_ -> Expect.lessThan 0.05 (Vec3.distance s0.position sAfter.position)
                    , \_ -> Expect.equal 100.0 sAfter.integrity
                    ]
                    ()
        , test "Steeper downhill track accelerates car to higher speed over equal time" <|
            \_ ->
                let
                    gentleTrack =
                        buildFlatTestTrack -0.06 500.0

                    steepTrack =
                        buildFlatTestTrack -0.22 500.0

                    spec =
                        loadSpec "turntable-kart"

                    sGentle =
                        simulateSteps 240 gentleTrack spec neutralInput (Physics.initPhysics gentleTrack spec)

                    sSteep =
                        simulateSteps 240 steepTrack spec neutralInput (Physics.initPhysics steepTrack spec)
                in
                Expect.all
                    [ \_ -> Expect.greaterThan 1.4 (Vec3.length sGentle.velocity)
                    , \_ -> Expect.greaterThan 5.0 (Vec3.length sSteep.velocity)
                    , \_ -> Expect.greaterThan (Vec3.length sGentle.velocity) (Vec3.length sSteep.velocity)
                    ]
                    ()
        , test "Aero tuck achieves higher downhill speed than upright posture" <|
            \_ ->
                let
                    track =
                        buildFlatTestTrack -0.15 800.0

                    spec =
                        loadSpec "turntable-kart"

                    s0 =
                        Physics.initPhysics track spec

                    tuckInput =
                        { neutralInput | aeroTuck = 1.0 }

                    sNormal =
                        simulateSteps 420 track spec neutralInput s0

                    sTucked =
                        simulateSteps 420 track spec tuckInput s0
                in
                Expect.greaterThan (Vec3.length sNormal.velocity) (Vec3.length sTucked.velocity)
        , test "Belly-scraping car accelerates noticeably slower than clean-clearance car" <|
            \_ ->
                let
                    track =
                        buildFlatTestTrack -0.12 500.0

                    cleanSpec =
                        loadSpec "turntable-kart"

                    scrapingSpec =
                        { cleanSpec | groundClearanceLdu = -12.0, groundClearanceM = -0.075 }

                    sClean =
                        simulateSteps 240 track cleanSpec neutralInput (Physics.initPhysics track cleanSpec)

                    sScrape =
                        simulateSteps 240 track scrapingSpec neutralInput (Physics.initPhysics track scrapingSpec)
                in
                Expect.greaterThan (Vec3.length sScrape.velocity) (Vec3.length sClean.velocity)
        , test "Holding brake decelerates moving car cleanly down toward zero" <|
            \_ ->
                let
                    track =
                        buildFlatTestTrack 0.0 400.0

                    spec =
                        loadSpec "pinewood-classic"

                    s0 =
                        Physics.initPhysics track spec

                    sFast =
                        { s0 | velocity = vec3 0 0 12.0 }

                    sBraked =
                        simulateSteps 120 track spec { neutralInput | brake = 1.0 } sFast
                in
                Expect.lessThan 2.5 (Vec3.length sBraked.velocity)
        , test "User soapbox b/HYux... descends downhill smoothly without bouncing or losing integrity" <|
            \_ ->
                let
                    bumpyHash =
                        "#b/HYuxFYAwCETvIJUW1m4RTYzP0hSu5AC6WxxLAu9x8OGuXkQIucRosuaQSl5C2o9NiQnAaA3frD6SWodmQx4CRi_96igiZnJtFOk_I-05hZ7z_QM"

                    track =
                        Track.generate (Track.trackConfigFor FarmLane 42)
                in
                case Ingest.decodeRaceInput Nothing bumpyHash of
                    Ok decoded ->
                        let
                            analyzed =
                                Analyze.analyzeDocument "Bumpy" decoded.document EmbeddedParts.cache Catalog.emptyPartIndex

                            s0 =
                                Physics.initPhysics track analyzed.spec

                            sEnd =
                                simulateSteps 180 track analyzed.spec neutralInput s0
                        in
                        Expect.all
                            [ \_ -> Expect.greaterThan 3.0 (Vec3.length sEnd.velocity)
                            , \_ -> Expect.within (Expect.Absolute 0.001) 0.0 sEnd.airborneTime
                            , \_ -> Expect.within (Expect.Absolute 0.01) 100.0 sEnd.integrity
                            ]
                            ()

                    Err _ ->
                        Expect.fail "Failed to decode bumpy hash"
        , test "Steering right creates positive lateral +right motion on Turntable car stronger than FixedAxle" <|
            \_ ->
                let
                    track =
                        buildFlatTestTrack 0.0 400.0

                    ttSpec =
                        loadSpec "turntable-kart"

                    fixedSpec =
                        loadSpec "pinewood-classic"

                    steerRightInput =
                        { neutralInput | steer = 1.0 }

                    startFast sp =
                        let
                            s0 =
                                Physics.initPhysics track sp
                        in
                        { s0 | velocity = vec3 0 0 14.0 }

                    sTt =
                        simulateSteps 60 track ttSpec steerRightInput (startFast ttSpec)

                    sFixed =
                        simulateSteps 60 track fixedSpec steerRightInput (startFast fixedSpec)
                in
                Expect.all
                    [ \_ -> Expect.lessThan -0.05 (Vec3.getX sTt.position)
                    , \_ -> Expect.greaterThan (abs (Vec3.getX sFixed.position)) (abs (Vec3.getX sTt.position))
                    ]
                    ()
        , test "Turntable Bogie steering pulse settles cleanly without oversteer or spinout" <|
            \_ ->
                let
                    track =
                        buildFlatTestTrack 0.0 500.0

                    spec =
                        loadSpec "turntable-kart"

                    s0 =
                        Physics.initPhysics track spec

                    sStart =
                        { s0 | velocity = vec3 0 0 15.0 }

                    steerRightInput =
                        { neutralInput | steer = 1.0 }

                    -- 18 frames (~0.3s) of steering input
                    sSteered =
                        simulateSteps 18 track spec steerRightInput sStart

                    -- 42 frames (~0.7s) of neutral steering recovery
                    sRecovered =
                        simulateSteps 42 track spec neutralInput sSteered
                in
                Expect.all
                    [ -- Settles back into directional stability: yaw rate drops close to zero
                      \_ -> Expect.lessThan 0.05 (abs sRecovered.yawRate)

                    -- Holds line without runaway over-rotation beyond initial turn
                    , \_ -> Expect.lessThan 0.7 (abs sRecovered.yaw)
                    ]
                    ()
        , test "User car hash #b/fcsx... with turntable bogie navigates corner and stabilizes without oversteer" <|
            \_ ->
                let
                    userHash =
                        "#b/fcsxEgFBEAXQ_6ebRWC2SiQSKlmbGbs7md1A6hCOQugOHNERdDmA5FV3_f-nC7HU0pk5qWjuylFzX09NHlK9mT-Wq5OSZ4M5fVUiAlijdfFm_JCUaQUy3AmEEH7Ig6BAxsNVvRg3Mw93-7kA56efQPv6s2wE4xYLbwNf"

                    track =
                        buildFlatTestTrack 0.0 500.0
                in
                case Ingest.decodeRaceInput Nothing userHash of
                    Ok decoded ->
                        let
                            analyzed =
                                Analyze.analyzeDocument "UserCar" decoded.document EmbeddedParts.cache Catalog.emptyPartIndex

                            s0 =
                                Physics.initPhysics track analyzed.spec

                            sStart =
                                { s0 | velocity = vec3 0 0 14.0 }

                            sSteered =
                                simulateSteps 18 track analyzed.spec { neutralInput | steer = 1.0 } sStart

                            sEnd =
                                simulateSteps 42 track analyzed.spec neutralInput sSteered
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan 0.05 (abs sEnd.yawRate)
                            , \_ -> Expect.greaterThan 0.0 (Vec3.length sEnd.velocity)
                            , \_ -> Expect.within (Expect.Absolute 0.01) 100.0 sEnd.integrity
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed to decode user car hash: " ++ Debug.toString err)
        , test "respawnAtCheckpoint restores full integrity and resets velocity to zero" <|
            \_ ->
                let
                    track =
                        Track.generate (Track.trackConfigFor FarmLane 7)

                    spec =
                        loadSpec "turntable-kart"

                    s0 =
                        Physics.initPhysics track spec

                    damaged =
                        { s0
                            | velocity = vec3 4 2 18
                            , integrity = 0.0
                            , roll = 1.2
                        }

                    recovered =
                        Physics.respawnAtCheckpoint damaged
                in
                Expect.all
                    [ \_ -> Expect.equal 100.0 recovered.integrity
                    , \_ -> Expect.equal 0.0 recovered.roll
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) 0.0 (Vec3.length recovered.velocity)
                    ]
                    ()
        , test "High-speed rock collision smashes it, reduces structural integrity and emits Impact" <|
            \_ ->
                let
                    baseTrack =
                        buildFlatTestTrack 0.0 200.0

                    boulder =
                        { id = 1
                        , kind = Racer.Types.Rock
                        , s = 20.0
                        , position = vec3 0.0 0.0 20.0
                        , radiusM = 1.2
                        , heightM = 1.8
                        , yaw = 0.0
                        }

                    trackWithBoulder =
                        { baseTrack
                            | obstacles = [ boulder ]
                            , obstacleBuckets = Array.fromList [ [ boulder ], [], [], [], [], [], [], [] ]
                        }

                    spec =
                        loadSpec "turntable-kart"

                    s0 =
                        Physics.initPhysics trackWithBoulder spec

                    sFast =
                        { s0
                            | position = vec3 0.0 0.0 18.2
                            , velocity = vec3 0.0 0.0 16.0
                            , progressS = 18.2
                        }

                    ( sAfter, events ) =
                        Physics.stepFixed fixedDt trackWithBoulder spec neutralInput sFast

                    hasImpact =
                        List.any
                            (\e ->
                                case e of
                                    Racer.Types.Impact _ ->
                                        True

                                    _ ->
                                        False
                            )
                            events
                in
                Expect.all
                    [ \_ -> Expect.equal True hasImpact
                    , \_ -> Expect.lessThan 100.0 sAfter.integrity
                    ]
                    ()
        , test "Ploughing into a bush smashes it, keeps most speed and costs no integrity" <|
            \_ ->
                let
                    bush =
                        sceneryAt Racer.Types.Bush 20.0

                    track =
                        trackWithScenery bush

                    spec =
                        loadSpec "turntable-kart"

                    s0 =
                        Physics.initPhysics track spec

                    sFast =
                        { s0 | position = vec3 0.0 0.0 18.6, velocity = vec3 0.0 0.0 9.0, progressS = 18.6 }

                    ( sAfter, events ) =
                        Physics.stepFrame (fixedDt * 4.0) 0.0 track spec neutralInput sFast
                            |> (\( _, st, evs ) -> ( st, evs ))
                in
                Expect.all
                    [ \_ -> Expect.equal [ 7 ] (smashedIds events)
                    , \_ -> Expect.greaterThan 7.5 (Vec3.getZ sAfter.velocity)
                    , \_ -> Expect.within (Expect.Absolute 0.01) 100.0 sAfter.integrity
                    ]
                    ()
        , test "A slow nudge against a rock bounces off without breaking it" <|
            \_ ->
                let
                    rock =
                        sceneryAt Racer.Types.Rock 20.0

                    track =
                        trackWithScenery rock

                    spec =
                        loadSpec "turntable-kart"

                    s0 =
                        Physics.initPhysics track spec

                    sSlow =
                        { s0 | position = vec3 0.0 0.0 18.9, velocity = vec3 0.0 0.0 3.0, progressS = 18.9 }

                    ( sAfter, events ) =
                        Physics.stepFixed fixedDt track spec neutralInput sSlow
                in
                Expect.all
                    [ \_ -> Expect.equal [] (smashedIds events)
                    , \_ -> Expect.lessThan 0.0 (Vec3.getZ sAfter.velocity)
                    , \_ -> Expect.lessThan 5.5 (Physics.breakSpeedFor Racer.Types.Bush)
                    ]
                    ()
        , test "A brick ramp taken at speed launches the car on an arc and the landing settles it" <|
            \_ ->
                let
                    track =
                        rampTestTrack

                    spec =
                        loadSpec "turntable-kart"

                    run =
                        runRecorded 150 track spec (rampApproach track spec 15.0)

                    heights =
                        List.map (heightAboveRamp track) run.states

                    airborneSteps =
                        List.length (List.filter (\st -> st.groundedWheels == 0) run.states)

                    landed =
                        List.any isLanding run.events

                    crashed =
                        List.any isCrash run.events

                    final =
                        List.reverse run.states |> List.head |> Maybe.withDefault (rampApproach track spec 15.0)
                in
                Expect.all
                    [ \_ -> Expect.greaterThan 0.6 (List.maximum heights |> Maybe.withDefault 0.0)
                    , \_ -> Expect.atLeast 18 airborneSteps
                    , \_ -> Expect.atMost 90 airborneSteps
                    , \_ -> Expect.equal True landed
                    , \_ -> Expect.equal False crashed
                    , \_ -> Expect.atLeast 90.0 final.integrity
                    , \_ -> Expect.equal 4 final.groundedWheels
                    , \_ -> Expect.atMost 0.3 (heightAboveRamp track final)
                    , \_ -> Expect.greaterThan 55.0 final.progressS
                    , \_ -> Expect.greaterThan 8.0 (Vec3.length final.velocity)
                    ]
                    ()
        , test "A brick ramp flight is deterministic at the fixed timestep" <|
            \_ ->
                let
                    spec =
                        loadSpec "turntable-kart"

                    a =
                        simulateSteps 90 rampTestTrack spec neutralInput (rampApproach rampTestTrack spec 15.0)

                    b =
                        simulateSteps 90 rampTestTrack spec neutralInput (rampApproach rampTestTrack spec 15.0)
                in
                Expect.equal a b
        , test "Driving beside a brick ramp in the free lane stays on the ground" <|
            \_ ->
                let
                    spec =
                        loadSpec "turntable-kart"

                    beside =
                        rampApproach rampTestTrack spec 15.0
                            |> (\st -> { st | position = Vec3.add st.position (vec3 -4.5 0 0) })

                    run =
                        runRecorded 90 rampTestTrack spec beside
                in
                Expect.equal [] (List.filter (\st -> st.groundedWheels == 0) run.states |> List.map .progressS)
        , test "Smashing a crate costs a little speed and no integrity" <|
            \_ ->
                let
                    ( speedAfter, integrity, smashed ) =
                        smashAt Racer.Types.Crate 12.0
                in
                Expect.all
                    [ \_ -> Expect.equal [ 7 ] smashed
                    , \_ -> Expect.within (Expect.Absolute 0.01) 100.0 integrity
                    , \_ -> Expect.lessThan 12.0 speedAfter
                    , \_ -> Expect.greaterThan 9.5 speedAfter
                    ]
                    ()
        , test "Smashing a barrel, a gnome or a brick wall chips integrity; the wall costs most" <|
            \_ ->
                let
                    ( barrelSpeed, barrelIntegrity, barrelSmashed ) =
                        smashAt Racer.Types.Barrel 12.0

                    ( gnomeSpeed, gnomeIntegrity, gnomeSmashed ) =
                        smashAt Racer.Types.GardenGnome 12.0

                    ( wallSpeed, wallIntegrity, wallSmashed ) =
                        smashAt Racer.Types.BrickWall 12.0
                in
                Expect.all
                    [ \_ -> Expect.equal [ [ 7 ], [ 7 ], [ 7 ] ] [ barrelSmashed, gnomeSmashed, wallSmashed ]
                    , \_ -> Expect.lessThan 100.0 gnomeIntegrity
                    , \_ -> Expect.lessThan gnomeIntegrity barrelIntegrity
                    , \_ -> Expect.lessThan barrelIntegrity wallIntegrity
                    , \_ -> Expect.lessThan barrelSpeed wallSpeed
                    , \_ -> Expect.lessThan gnomeSpeed barrelSpeed
                    ]
                    ()
        , test "A slow knock against a brick wall bounces off without breaking it" <|
            \_ ->
                let
                    ( _, _, smashed ) =
                        smashAt Racer.Types.BrickWall 4.0
                in
                Expect.equal [] smashed
        , test "Two-player split-screen starting grid offsets players laterally by 3.6 meters" <|
            \_ ->
                let
                    flatTrack =
                        buildFlatTestTrack 0.0 300.0

                    spec1 =
                        loadSpec "turntable-kart"

                    spec2 =
                        loadSpec "pinewood-classic"

                    p1 =
                        Physics.initPhysicsWithOffset -1.8 flatTrack spec1

                    p2 =
                        Physics.initPhysicsWithOffset 1.8 flatTrack spec2

                    p1X =
                        Vec3.getX p1.position

                    p2X =
                        Vec3.getX p2.position

                    lateralDist =
                        abs (p2X - p1X)
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.1) 3.6 lateralDist
                    , \_ -> Expect.lessThan 0.0 p1X
                    , \_ -> Expect.greaterThan 0.0 p2X
                    , \_ -> Expect.equal (vec3 0 0 0) p1.velocity
                    , \_ -> Expect.equal (vec3 0 0 0) p2.velocity
                    ]
                    ()
        , describe "Brick loops" loopTests
        , describe "Rounded side ramps" sideRampTests
        ]


loopTests : List Test
loopTests =
    [ test "A car at racing speed rides round the loop upside down and comes back down onto the road" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                run =
                    runRecorded 240 loopTestTrack spec (loopApproach loopTestTrack spec 15.0)

                onLoop =
                    List.filter (\st -> st.loopRide /= Nothing) run.states

                final =
                    List.reverse run.states |> List.head |> Maybe.withDefault (loopApproach loopTestTrack spec 15.0)

                roadY =
                    Vec3.getY (Track.splineSampleAtS loopTestTrack 60.0).center

                highest =
                    List.map (.position >> Vec3.getY) onLoop |> List.maximum |> Maybe.withDefault roadY
            in
            Expect.all
                [ \_ -> Expect.atLeast 30 (List.length onLoop)
                , \_ -> Expect.lessThan -0.95 (List.map carUpY onLoop |> List.minimum |> Maybe.withDefault 1.0)
                , \_ -> Expect.atLeast (roadY + 5.0) highest
                , \_ -> Expect.equal Nothing final.loopRide
                , \_ -> Expect.equal 4 final.groundedWheels
                , \_ -> Expect.greaterThan 80.0 final.progressS
                , \_ -> Expect.atMost 0.3 (heightAboveRamp loopTestTrack final)
                , \_ -> Expect.lessThan 0.3 (abs final.pitch - 0.29)
                , \_ -> Expect.greaterThan 12.0 (Vec3.length final.velocity)
                , \_ -> Expect.within (Expect.Absolute 1.0e-9) 100.0 final.integrity
                , \_ -> Expect.equal False (List.any isCrash run.events)
                , \_ -> Expect.equal True (pairwiseNonDecreasing (List.map .progressS run.states))
                ]
                ()
    , test "The car moves smoothly round the loop: no jumps in position or heading" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                run =
                    runRecorded 150 loopTestTrack spec (loopApproach loopTestTrack spec 15.0)

                steps =
                    List.map2 (\a b -> ( Vec3.distance a.position b.position, Vec3.dot (carForward a) (carForward b) )) run.states (List.drop 1 run.states)
            in
            Expect.all
                [ \_ -> Expect.atMost 0.6 (List.map Tuple.first steps |> List.maximum |> Maybe.withDefault 0.0)
                , \_ -> Expect.atLeast 0.9 (List.map Tuple.second steps |> List.minimum |> Maybe.withDefault 1.0)
                ]
                ()
    , test "A loop ride is deterministic at the fixed timestep" <|
        \_ ->
            let
                spec =
                    loadSpec "pinewood-classic"

                a =
                    simulateSteps 120 loopTestTrack spec neutralInput (loopApproach loopTestTrack spec 15.0)

                b =
                    simulateSteps 120 loopTestTrack spec neutralInput (loopApproach loopTestTrack spec 15.0)
            in
            Expect.equal a b
    , test "A car too slow for the loop rolls back out of it onto the road" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                run =
                    runRecorded 420 loopTestTrack spec (loopApproachAt 58.0 loopTestTrack spec 4.0)

                thetas =
                    List.filterMap (.loopRide >> Maybe.map .theta) run.states

                final =
                    List.reverse run.states |> List.head |> Maybe.withDefault (loopApproach loopTestTrack spec 4.0)
            in
            Expect.all
                [ \_ -> Expect.greaterThan 0 (List.length thetas)
                , \_ -> Expect.lessThan (pi / 2.0) (List.maximum thetas |> Maybe.withDefault 0.0)
                , \_ -> Expect.equal Nothing final.loopRide
                , \_ -> Expect.equal 4 final.groundedWheels
                , \_ -> Expect.equal False (List.any isCrash run.events)
                ]
                ()
    , test "A car that is not quite fast enough falls off near the top, lands and turns upright" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                run =
                    runRecorded 300 loopTestTrack spec (loopApproachAt 58.0 loopTestTrack spec 9.5)

                pairs =
                    List.map2 Tuple.pair run.states (List.drop 1 run.states)

                fellOff =
                    List.filter (\( a, b ) -> a.loopRide /= Nothing && b.loopRide == Nothing && b.groundedWheels == 0) pairs

                final =
                    List.reverse run.states |> List.head |> Maybe.withDefault (loopApproach loopTestTrack spec 9.5)
            in
            Expect.all
                [ \_ -> Expect.equal 1 (List.length fellOff)
                , \_ -> Expect.greaterThan (pi / 2.0) (List.filterMap (.loopRide >> Maybe.map .theta) run.states |> List.maximum |> Maybe.withDefault 0.0)
                , \_ -> Expect.lessThan (3.0 * pi / 2.0) (List.filterMap (.loopRide >> Maybe.map .theta) run.states |> List.maximum |> Maybe.withDefault 0.0)
                , \_ -> Expect.equal True (List.any isLanding run.events)
                , \_ -> Expect.equal Nothing final.loopRide
                , \_ -> Expect.atLeast 1 final.groundedWheels
                , \_ -> Expect.atMost 0.76 (abs final.pitch)
                , \_ -> Expect.lessThan 100.0 final.integrity
                , \_ -> Expect.greaterThan 0.0 final.integrity
                ]
                ()
    , test "Driving off the road beside the loop entry does not get on it" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                beside =
                    loopApproach loopTestTrack spec 15.0
                        |> (\st -> { st | position = Vec3.add st.position (vec3 -9.0 0 0) })

                run =
                    runRecorded 120 loopTestTrack spec beside
            in
            Expect.equal [] (List.filter (\st -> st.loopRide /= Nothing) run.states |> List.map .progressS)
    , test "Respawning at a checkpoint takes the car off the loop" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                onLoop =
                    simulateSteps 90 loopTestTrack spec neutralInput (loopApproach loopTestTrack spec 15.0)
            in
            Expect.all
                [ \_ -> Expect.notEqual Nothing onLoop.loopRide
                , \_ -> Expect.equal Nothing (Physics.respawnAtCheckpoint onLoop).loopRide
                ]
                ()
    ]


sideRampTests : List Test
sideRampTests =
    [ test "A car steered into a side ramp rides up its curve and comes back down without crashing" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                start =
                    sideRampApproach spec 4.0

                run =
                    runRecorded 240 sideRampTestTrack spec start

                heights =
                    List.map (heightAboveRamp sideRampTestTrack) run.states

                lifts =
                    List.map
                        (\st ->
                            let
                                g =
                                    Track.sampleAt sideRampTestTrack (Vec3.getX st.position) (Vec3.getZ st.position)
                            in
                            (Track.sideRampLift sideRampTestTrack g.nearestS g.roadOffsetM).lift
                        )
                        run.states

                final =
                    List.reverse run.states |> List.head |> Maybe.withDefault start
            in
            Expect.all
                [ \_ -> Expect.greaterThan 0.4 (List.maximum lifts |> Maybe.withDefault 0.0)
                , \_ -> Expect.lessThan 2.5 (List.maximum heights |> Maybe.withDefault 0.0)
                , \_ -> Expect.equal False (List.any isCrash run.events)
                , \_ -> Expect.atLeast 1 final.groundedWheels
                , \_ -> Expect.greaterThan 70.0 final.progressS
                , \_ -> Expect.atLeast 60.0 final.integrity
                ]
                ()
    , test "On the ramp the ground tilts back towards the road" <|
        \_ ->
            let
                smp =
                    Track.splineSampleAtS sideRampTestTrack 45.0

                onRamp =
                    Vec3.add smp.center (Vec3.scale 8.5 smp.right)

                g =
                    Track.sampleAt sideRampTestTrack (Vec3.getX onRamp) (Vec3.getZ onRamp)

                s =
                    Track.sideRampLift sideRampTestTrack g.nearestS g.roadOffsetM
            in
            Expect.all
                [ \_ -> Expect.greaterThan 0.3 s.lift
                , \_ -> Expect.greaterThan 0.5 s.sideGrade
                ]
                ()
    , test "Driving down the middle of the road past a side ramp stays on the ground" <|
        \_ ->
            let
                spec =
                    loadSpec "turntable-kart"

                run =
                    runRecorded 120 sideRampTestTrack spec (rampApproach sideRampTestTrack spec 15.0)
            in
            Expect.equal [] (List.filter (\st -> st.groundedWheels == 0) run.states |> List.map .progressS)
    ]


{-| The 30 % test road with one rounded side ramp from 30 m to 60 m on its
right edge (the road is 8 m wide each side of its centre).
-}
sideRampTestTrack : GeneratedTrack
sideRampTestTrack =
    let
        base =
            buildFlatTestTrack -0.3 200.0
    in
    { base
        | sideRamps =
            [ { startS = 30.0, endS = 60.0, side = 1.0, innerOffsetM = 7.0, outerOffsetM = 9.4, heightM = 1.2 } ]
    }


{-| The car 5 m right of the road centre at 30 m, rolling down the road at
12 m/s and heading right towards the side ramp at `lateralSpeed`.
-}
sideRampApproach : VehicleSpec -> Float -> PhysicsState
sideRampApproach spec lateralSpeed =
    let
        s0 =
            Physics.initPhysics sideRampTestTrack spec

        smp =
            Track.splineSampleAtS sideRampTestTrack 30.0

        velocity =
            Vec3.add (Vec3.scale 12.0 smp.tangent) (Vec3.scale lateralSpeed smp.right)
    in
    { s0
        | position = Vec3.add smp.center (Vec3.scale 5.0 smp.right)
        , velocity = velocity
        , yaw = atan2 (Vec3.getX velocity) (Vec3.getZ velocity)
        , progressS = 30.0
    }


{-| The 30 % test road with one brick loop from 60 m to 69.8 m, 3.5 m radius.
-}
loopTestTrack : GeneratedTrack
loopTestTrack =
    let
        base =
            buildFlatTestTrack -0.3 200.0
    in
    { base
        | loops =
            [ { entryS = 60.0, exitS = 69.8, radiusM = 3.5, driftM = 2.0, laneHalfWidthM = 1.3, apronHalfWidthM = 8.0 } ]
    }


{-| The car on the road 20 m before the loop, rolling straight at it.
-}
loopApproach : GeneratedTrack -> VehicleSpec -> Float -> PhysicsState
loopApproach =
    loopApproachAt 40.0


loopApproachAt : Float -> GeneratedTrack -> VehicleSpec -> Float -> PhysicsState
loopApproachAt s track spec speed =
    let
        s0 =
            Physics.initPhysics track spec

        smp =
            Track.splineSampleAtS track s
    in
    { s0
        | position = smp.center
        , velocity = Vec3.scale speed smp.tangent
        , progressS = s
    }


{-| World up of the car's roof: below -0.9 means upside down.
-}
carUpY : PhysicsState -> Float
carUpY st =
    cos st.roll * cos st.pitch


{-| World forward of the car's nose, from its yaw, roll and pitch.
-}
carForward : PhysicsState -> Vec3.Vec3
carForward st =
    let
        -- Rx(pitch) then Rz(roll) then Ry(yaw) applied to +Z.
        x1 =
            sin st.roll * sin st.pitch

        y1 =
            -(cos st.roll * sin st.pitch)

        z1 =
            cos st.pitch
    in
    vec3 (cos st.yaw * x1 + sin st.yaw * z1) y1 (-(sin st.yaw) * x1 + cos st.yaw * z1)


pairwiseNonDecreasing : List Float -> Bool
pairwiseNonDecreasing xs =
    case xs of
        a :: b :: rest ->
            b >= a && pairwiseNonDecreasing (b :: rest)

        _ ->
            True


{-| A straight 30 % downhill test road with one brick ramp at 40-46.5 m in its
middle lane, and a 9 m wide free lane beside it.
-}
rampTestTrack : GeneratedTrack
rampTestTrack =
    let
        base =
            buildFlatTestTrack -0.3 200.0
    in
    { base
        | brickRamps =
            [ { startS = 40.0, endS = 46.5, lipHeightM = 0.75, centerOffsetM = 0.0, halfWidthM = 1.8 } ]
    }


{-| The car on the road 20 m before the ramp, rolling straight at it.
-}
rampApproach : GeneratedTrack -> VehicleSpec -> Float -> PhysicsState
rampApproach track spec speed =
    let
        s0 =
            Physics.initPhysics track spec

        smp =
            Track.splineSampleAtS track 20.0
    in
    { s0
        | position = smp.center
        , velocity = Vec3.scale speed smp.tangent
        , progressS = 20.0
    }


heightAboveRamp : GeneratedTrack -> PhysicsState -> Float
heightAboveRamp track st =
    let
        g =
            Track.sampleAt track (Vec3.getX st.position) (Vec3.getZ st.position)
    in
    Vec3.getY st.position - g.height - (Track.brickRampLift track g.nearestS g.roadOffsetM).lift


runRecorded : Int -> GeneratedTrack -> VehicleSpec -> PhysicsState -> { states : List PhysicsState, events : List Racer.Types.PhysicsEvent }
runRecorded count track spec start =
    let
        go n st acc =
            if n <= 0 then
                { states = List.reverse acc.states, events = acc.events }

            else
                let
                    ( next, evs ) =
                        Physics.stepFixed fixedDt track spec neutralInput st
                in
                go (n - 1) next { states = next :: acc.states, events = acc.events ++ evs }
    in
    go count start { states = [], events = [] }


isLanding : Racer.Types.PhysicsEvent -> Bool
isLanding e =
    case e of
        Racer.Types.Landing _ ->
            True

        _ ->
            False


isCrash : Racer.Types.PhysicsEvent -> Bool
isCrash e =
    case e of
        Racer.Types.Crashed _ ->
            True

        _ ->
            False


{-| Drive the kart into a piece of scenery at `speed` on flat ground; returns
the speed and integrity after the hit and the ids of the pieces smashed.
-}
smashAt : Racer.Types.ObstacleKind -> Float -> ( Float, Float, List Int )
smashAt kind speed =
    let
        track =
            trackWithScenery (sceneryAt kind 20.0)

        spec =
            loadSpec "turntable-kart"

        s0 =
            Physics.initPhysics track spec

        sHit =
            { s0
                | position = vec3 0.0 0.0 18.6
                , velocity = vec3 0.0 0.0 speed
                , progressS = 18.6
            }

        ( _, sAfter, events ) =
            Physics.stepFrame (fixedDt * 4.0) 0.0 track spec neutralInput sHit
    in
    ( Vec3.length sAfter.velocity, sAfter.integrity, smashedIds events )


sceneryAt : Racer.Types.ObstacleKind -> Float -> Racer.Types.TrackObstacle
sceneryAt kind z =
    { id = 7
    , kind = kind
    , s = z
    , position = vec3 0.0 0.0 z
    , radiusM = 0.8
    , heightM = 1.0
    , yaw = 0.0
    }


trackWithScenery : Racer.Types.TrackObstacle -> GeneratedTrack
trackWithScenery obs =
    let
        baseTrack =
            buildFlatTestTrack 0.0 200.0
    in
    { baseTrack
        | obstacles = [ obs ]
        , obstacleBuckets = Array.fromList [ [ obs ], [], [], [], [], [], [], [] ]
    }


smashedIds : List Racer.Types.PhysicsEvent -> List Int
smashedIds events =
    List.filterMap
        (\e ->
            case e of
                Racer.Types.ScenerySmashed { obstacleId } ->
                    Just obstacleId

                _ ->
                    Nothing
        )
        events


neutralInput : InputState
neutralInput =
    { steer = 0.0
    , brake = 0.0
    , handbrake = False
    , aeroTuck = 0.0
    , leanX = 0.0
    }


loadSpec : String -> VehicleSpec
loadSpec presetId =
    Ingest.findPresetById presetId
        |> Maybe.map (.ldrText >> Document.parse)
        |> Maybe.withDefault Document.empty
        |> (\doc -> (Analyze.analyzeDocument presetId doc EmbeddedParts.cache Catalog.emptyPartIndex).spec)


simulateSteps : Int -> GeneratedTrack -> VehicleSpec -> InputState -> PhysicsState -> PhysicsState
simulateSteps count track spec input state =
    if count <= 0 then
        state

    else
        let
            ( nextState, _ ) =
                Physics.stepFixed fixedDt track spec input state
        in
        simulateSteps (count - 1) track spec input nextState


buildFlatTestTrack : Float -> Float -> GeneratedTrack
buildFlatTestTrack dyDz totalLen =
    let
        spacing =
            2.0

        count =
            round (totalLen / spacing)

        rawDir =
            Vec3.normalize (vec3 0 dyDz 1.0)

        normal =
            vec3 0 (Vec3.getZ rawDir) -(Vec3.getY rawDir)
                |> Vec3.normalize

        mkSample i =
            let
                s =
                    toFloat i * spacing
            in
            { s = s
            , center = Vec3.scale s rawDir
            , tangent = rawDir
            , right = vec3 -1 0 0
            , normal = normal
            , halfWidth = 8.0
            , bankingRad = 0.0
            , surface = DirtRoad
            }

        samples : Array.Array TrackSample
        samples =
            List.range 0 count
                |> List.map mkSample
                |> Array.fromList

        cfg =
            Track.trackConfigFor FarmLane 1
    in
    { config = { cfg | lengthM = totalLen, dropM = abs (dyDz * totalLen) }
    , samples = samples
    , sampleSpacingM = spacing
    , totalLengthM = totalLen
    , totalDropM = max 0.0 -(dyDz * totalLen)
    , startPosition = vec3 0 0 0
    , startYaw = 0.0
    , finishS = totalLen - 20.0
    , checkpoints = []
    , obstacles = []
    , obstacleBuckets = Array.repeat 16 []
    , bucketSizeM = 25.0
    , ramps = []
    , brickRamps = []
    , loops = []
    , sideRamps = []
    , washes = []
    }
