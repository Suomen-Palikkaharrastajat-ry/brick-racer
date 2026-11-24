module Racer.Audio exposing (AudioTelemetry, deriveAudioState)

{-| Pure derivation of continuous procedural Web Audio synthesizer parameters
from `PhysicsState` and `VehicleSpec`.
-}

import Math.Vector3 as Vec3
import Racer.Types exposing (PhysicsState, SurfaceType(..), VehicleSpec)


{-| Continuous gain and filter frequency parameters sent over `Ports.updateAudioState`.
-}
type alias AudioTelemetry =
    { active : Bool
    , windGain : Float
    , windHz : Float
    , rumbleGain : Float
    , rumbleHz : Float
    , clatterGain : Float
    , scrapeGain : Float
    }


{-| Derive continuous Web Audio synthesis parameters from vehicle physics state.
-}
deriveAudioState : Bool -> Bool -> PhysicsState -> VehicleSpec -> AudioTelemetry
deriveAudioState isRacing isMuted physics spec =
    if not isRacing || isMuted then
        { active = False
        , windGain = 0.0
        , windHz = 220.0
        , rumbleGain = 0.0
        , rumbleHz = 60.0
        , clatterGain = 0.0
        , scrapeGain = 0.0
        }

    else
        let
            speed =
                Vec3.length physics.velocity

            speedNorm =
                clamp 0.0 1.5 (speed / 28.0)

            airBoost =
                if physics.groundedWheels == 0 then
                    1.22

                else
                    1.0

            windGain =
                clamp 0.0 0.45 ((speedNorm ^ 1.55) * 0.34 * airBoost)

            windHz =
                clamp 180.0 1900.0 (210.0 + (speed ^ 1.25) * 28.0)

            groundedFactor =
                if physics.groundedWheels > 0 then
                    1.0

                else
                    0.0

            surfaceRumbleMult =
                case physics.surfaceUnderCar of
                    DirtRoad ->
                        0.7

                    ForestTrack ->
                        0.9

                    GravelRoad ->
                        1.15

                    GrassPath ->
                        0.75

                    RoughVerge ->
                        1.0

                    MeadowGrass ->
                        0.95

            hasRubber =
                List.any .hasRubberTire spec.wheels

            rimPenalty =
                if hasRubber then
                    1.0

                else
                    1.6

            rumbleGain =
                clamp 0.0 0.42 (groundedFactor * clamp 0.0 1.0 (speed / 22.0) * 0.25 * surfaceRumbleMult)

            rumbleHz =
                clamp 45.0 240.0 (48.0 + speed * 4.5)

            clatterBase =
                case physics.surfaceUnderCar of
                    DirtRoad ->
                        0.14

                    ForestTrack ->
                        0.24

                    GravelRoad ->
                        0.34

                    GrassPath ->
                        0.12

                    RoughVerge ->
                        0.22

                    MeadowGrass ->
                        0.16

            clatterGain =
                clamp 0.0 0.4 (groundedFactor * clamp 0.0 1.0 (speed / 16.0) * clatterBase * rimPenalty)

            scrapeGain =
                if physics.chassisScraping && physics.groundedWheels > 0 then
                    clamp 0.0 0.35 (clamp 0.0 1.0 (speed / 15.0) * 0.32)

                else
                    0.0
        in
        { active = True
        , windGain = windGain
        , windHz = windHz
        , rumbleGain = rumbleGain
        , rumbleHz = rumbleHz
        , clatterGain = clatterGain
        , scrapeGain = scrapeGain
        }
