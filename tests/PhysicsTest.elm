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
                    , \_ -> Expect.lessThan 0.70 (abs sRecovered.yaw)
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
        , test "High-speed obstacle collision reduces structural integrity and emits Impact" <|
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
        ]


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
    , washes = []
    }
