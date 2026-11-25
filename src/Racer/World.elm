module Racer.World exposing
    ( Fall(..)
    , fogRange
    , gripScale
    , lightning
    , lightningFlash
    , precipitation
    , rollingResistScale
    )

{-| How a map's season and weather change the way a course plays and looks.

Grip and rolling resistance are multipliers on the road: a winter course
grips less and rolls harder, a storm is slipperier than a clear day. Fog
sets how close the view fades out. Precipitation and lightning are what the
renderer draws on top of the course. A storm's flashes are timed by how far
the car has come (`lightningFlash`), not by the clock, so the same run always
flashes at the same places.

The neutral map (summer, clear) gives grip 1, rolling resistance 1 and the
usual fog, so courses on it play exactly as before.

-}

import Game.MapParams as Map exposing (MapParams, Season(..), WeatherKind(..))
import Random


{-| What falls from the sky.
-}
type Fall
    = RainFall
    | SnowFall


seasonGrip : Season -> Float
seasonGrip season =
    case season of
        Spring ->
            0.95

        Summer ->
            1.0

        Autumn ->
            0.93

        Winter ->
            0.8


seasonRolling : Season -> Float
seasonRolling season =
    case season of
        Spring ->
            1.15

        Summer ->
            1.0

        Autumn ->
            1.0

        Winter ->
            1.25


weatherGrip : WeatherKind -> Float
weatherGrip weather =
    case weather of
        Clear ->
            1.0

        Overcast ->
            1.0

        Rain ->
            0.86

        Storm ->
            0.8

        Fog ->
            0.97

        Snow ->
            0.78


weatherRolling : WeatherKind -> Float
weatherRolling weather =
    case weather of
        Clear ->
            1.0

        Overcast ->
            1.0

        Rain ->
            1.1

        Storm ->
            1.1

        Fog ->
            1.0

        Snow ->
            1.25


{-| Multiplier on the road's lateral grip, from 0.6 to 1.0.
-}
gripScale : MapParams -> Float
gripScale map =
    clamp 0.6 1.0 (seasonGrip map.season * weatherGrip map.weather)


{-| Multiplier on the road's rolling resistance, from 1.0 to 1.5.
-}
rollingResistScale : MapParams -> Float
rollingResistScale map =
    clamp 1.0 1.5 (seasonRolling map.season * weatherRolling map.weather)


{-| Distances in metres at which the fog starts (near) and is complete (far).
-}
fogRange : MapParams -> { near : Float, far : Float }
fogRange map =
    case map.weather of
        Clear ->
            { near = 140, far = 620 }

        Overcast ->
            { near = 120, far = 560 }

        Rain ->
            { near = 100, far = 460 }

        Storm ->
            { near = 80, far = 320 }

        Fog ->
            { near = 35, far = 170 }

        Snow ->
            { near = 70, far = 300 }


{-| What falls around the car and how hard, from 0 to 1, if anything does.
-}
precipitation : MapParams -> Maybe { fall : Fall, intensity : Float }
precipitation map =
    case map.weather of
        Rain ->
            Just { fall = RainFall, intensity = 0.6 }

        Storm ->
            Just { fall = RainFall, intensity = 1.0 }

        Snow ->
            Just { fall = SnowFall, intensity = 0.7 }

        _ ->
            Nothing


{-| Whether lightning flashes now and then (storms only).
-}
lightning : MapParams -> Bool
lightning map =
    map.weather == Storm


{-| How bright the lightning is at a distance along the course, in metres,
from 0 (no flash) to 1. Only storms flash.

The course is cut into 45 m stretches. Each stretch has, from the map's
"weather" seed, a one in three chance of a flash somewhere in it: a bright
strike that fades over about 6 m, with a short dip in the middle so it
flickers. The same map and the same progress always give the same flash.

-}
lightningFlash : MapParams -> Float -> Float
lightningFlash map progressM =
    if not (lightning map) then
        0

    else
        let
            stretchM =
                45

            flashM =
                6

            stretch =
                floor (progressM / stretchM)

            ( ( chance, offset ), strength ) =
                Random.step
                    (Random.map2 Tuple.pair
                        (Random.map2 Tuple.pair (Random.float 0 1) (Random.float 0 (stretchM - flashM)))
                        (Random.float 0.6 1.0)
                    )
                    (Random.initialSeed (Map.streamSeed "weather" map + stretch * 7919))
                    |> Tuple.first

            d =
                progressM - (toFloat stretch * stretchM + offset)
        in
        if chance > 0.34 || d < 0 || d >= flashM then
            0

        else
            let
                fade =
                    (1 - d / flashM) ^ 2

                flicker =
                    if d > 1.2 && d < 2.0 then
                        0.35

                    else
                        1
            in
            strength * fade * flicker
