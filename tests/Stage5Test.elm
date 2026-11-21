module Stage5Test exposing (suite)

{-| Unit tests for Stage 5 telemetry, leaderboard persistence, camera rigor,
and WebGL matrix composition.
-}

import Dict
import Expect
import LDraw.Catalog as Catalog
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Racer.Analyze as Analyze
import Racer.Audio as Audio
import Racer.Camera as RacerCamera
import Racer.Ingest as Ingest
import Racer.Leaderboard as Leaderboard
import Racer.Physics as Physics
import Racer.Render as Render
import Racer.Track as Track
import Racer.Types exposing (CameraMode(..), SurfaceType(..), TrackId(..))
import Render.Camera as Camera
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        presetLdr =
            Ingest.findPresetById "turntable-kart"
                |> Maybe.map .ldrText
                |> Maybe.withDefault "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"

        vehicleSpec =
            Analyze.analyzeDocument "Test Kart" (Document.parse presetLdr) Dict.empty Catalog.emptyPartIndex
                |> .spec

        track =
            Track.generate (Track.trackConfigFor PineRidge 1)

        basePhysics =
            Physics.initPhysics track vehicleSpec
    in
    describe "Stage 5 Camera, Audio & Leaderboard"
        [ describe "Racer.Audio"
            [ test "muted or paused returns inactive zeroed audio state" <|
                \_ ->
                    let
                        fastState =
                            { basePhysics | velocity = vec3 0 0 22.0 }

                        mutedTel =
                            Audio.deriveAudioState True False True fastState vehicleSpec

                        pausedTel =
                            Audio.deriveAudioState True True False fastState vehicleSpec

                        idleTel =
                            Audio.deriveAudioState False False False fastState vehicleSpec
                    in
                    Expect.equal
                        ( False, False, False )
                        ( mutedTel.active, pausedTel.active, idleTel.active )
            , test "gravel increases ground rumble over asphalt and airborne zeros rumble" <|
                \_ ->
                    let
                        asphalt =
                            Audio.deriveAudioState True
                                False
                                False
                                { basePhysics | velocity = vec3 0 0 18.0, surfaceUnderCar = AsphaltTrack, groundedWheels = 4 }
                                vehicleSpec

                        gravel =
                            Audio.deriveAudioState True
                                False
                                False
                                { basePhysics | velocity = vec3 0 0 18.0, surfaceUnderCar = GravelShoulder, groundedWheels = 4 }
                                vehicleSpec

                        airborne =
                            Audio.deriveAudioState True
                                False
                                False
                                { basePhysics | velocity = vec3 0 0 18.0, surfaceUnderCar = AsphaltTrack, groundedWheels = 0 }
                                vehicleSpec
                    in
                    Expect.all
                        [ \_ -> Expect.equal True asphalt.active
                        , \_ -> Expect.greaterThan asphalt.rumbleGain gravel.rumbleGain
                        , \_ -> Expect.within (Expect.Absolute 0.0001) 0.0 airborne.rumbleGain
                        , \_ -> Expect.greaterThan 0.05 airborne.windGain
                        ]
                        ()
            , test "chassis scraping produces continuous scrape gain when grounded" <|
                \_ ->
                    let
                        scraping =
                            Audio.deriveAudioState True
                                False
                                False
                                { basePhysics | velocity = vec3 0 0 14.0, chassisScraping = True, groundedWheels = 4 }
                                vehicleSpec
                    in
                    Expect.greaterThan 0.05 scraping.scrapeGain
            ]
        , describe "Racer.Leaderboard"
            [ test "time and split formatting produce deterministic strings" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "00:00.00" (Leaderboard.formatRaceTime -2.5)
                        , \_ -> Expect.equal "01:14.32" (Leaderboard.formatRaceTime 74.32)
                        , \_ -> Expect.equal "-0.62 s" (Leaderboard.formatSplitDelta -0.62)
                        , \_ -> Expect.equal "+1.05 s" (Leaderboard.formatSplitDelta 1.05)
                        ]
                        ()
            , test "leaderboard records sorted runs and roundtrips JSON" <|
                \_ ->
                    let
                        entry1 =
                            { trackId = PineRidge
                            , vehicleName = "Kart A"
                            , timeSeconds = 58.4
                            , topSpeedKmh = 92.0
                            , wheelCount = 4
                            , steeringLabel = "2x2 Turntable Bogie"
                            , seed = 1
                            }

                        entry2 =
                            { trackId = PineRidge
                            , vehicleName = "Kart B"
                            , timeSeconds = 51.2
                            , topSpeedKmh = 104.0
                            , wheelCount = 4
                            , steeringLabel = "2x2 Turntable Bogie"
                            , seed = 1
                            }

                        entrySlower =
                            { entry1 | vehicleName = "Kart C", timeSeconds = 64.1 }

                        ( lb1, best1 ) =
                            Leaderboard.recordRun entry1 Leaderboard.empty

                        ( lb2, best2 ) =
                            Leaderboard.recordRun entry2 lb1

                        ( lb3, best3 ) =
                            Leaderboard.recordRun entrySlower lb2

                        decoded =
                            Leaderboard.decodeLeaderboard (Leaderboard.encodeLeaderboard lb3)

                        bestAfter =
                            Leaderboard.bestForTrack PineRidge decoded
                    in
                    Expect.all
                        [ \_ -> Expect.equal True best1
                        , \_ -> Expect.equal True best2
                        , \_ -> Expect.equal False best3
                        , \_ -> Expect.equal (Just "Kart B") (Maybe.map .vehicleName bestAfter)
                        , \_ -> Expect.equal 3 (List.length (Leaderboard.entriesForTrack PineRidge decoded))
                        ]
                        ()
            ]
        , describe "Racer.Camera and Render"
            [ test "camera mode cycles Chase -> Hood -> Orbit -> Chase" <|
                \_ ->
                    Expect.equal
                        ( HoodCam, OrbitCam, ChaseCam )
                        ( RacerCamera.cycleMode ChaseCam
                        , RacerCamera.cycleMode HoodCam
                        , RacerCamera.cycleMode OrbitCam
                        )
            , test "race camera extends farPlane > 1000m for mountain vistas" <|
                \_ ->
                    let
                        cam0 =
                            RacerCamera.initRaceCamera 720 basePhysics vehicleSpec

                        camChase =
                            RacerCamera.updateRaceCamera 0.016 ChaseCam basePhysics vehicleSpec cam0

                        camHood =
                            RacerCamera.updateRaceCamera 0.016 HoodCam basePhysics vehicleSpec cam0
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 1000.0 (Camera.farPlane camChase)
                        , \_ -> Expect.greaterThan 1000.0 (Camera.farPlane camHood)
                        , \_ -> Expect.notEqual (Camera.position camChase) (Camera.position camHood)
                        ]
                        ()
            , test "buildCarWorldMatrix translates local origin to physics.position" <|
                \_ ->
                    let
                        shifted =
                            { basePhysics | position = vec3 12.5 34.0 -7.25 }

                        transformed =
                            Mat4.transform (Render.buildCarWorldMatrix shifted) (vec3 0 0 0)
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 12.5 (Vec3.getX transformed)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 34.0 (Vec3.getY transformed)
                        , \_ -> Expect.within (Expect.Absolute 0.001) -7.25 (Vec3.getZ transformed)
                        ]
                        ()
            ]
        ]
