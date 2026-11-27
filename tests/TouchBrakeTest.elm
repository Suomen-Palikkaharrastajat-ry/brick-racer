module TouchBrakeTest exposing (suite)

{-| Touch brake on the steer pads (`Racer.TouchBrake`): a finger on either pad
sets the braking flag and adds the service brake to the input without touching
steering, release clears it, and a mouse press only steers.
-}

import Expect
import Json.Decode as Decode
import Racer.TouchBrake as TouchBrake exposing (Side(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Racer.TouchBrake"
        [ test "a finger on the left pad sets the braking flag" <|
            \_ ->
                TouchBrake.none
                    |> TouchBrake.press Left { held = True, touch = True }
                    |> TouchBrake.braking
                    |> Expect.equal True
        , test "a finger on the right pad sets the braking flag" <|
            \_ ->
                TouchBrake.none
                    |> TouchBrake.press Right { held = True, touch = True }
                    |> TouchBrake.braking
                    |> Expect.equal True
        , test "release clears the braking flag" <|
            \_ ->
                TouchBrake.none
                    |> TouchBrake.press Left { held = True, touch = True }
                    |> TouchBrake.press Left { held = False, touch = True }
                    |> TouchBrake.braking
                    |> Expect.equal False
        , test "braking stays on while the other pad is still touched" <|
            \_ ->
                TouchBrake.none
                    |> TouchBrake.press Left { held = True, touch = True }
                    |> TouchBrake.press Right { held = True, touch = True }
                    |> TouchBrake.press Left { held = False, touch = True }
                    |> TouchBrake.braking
                    |> Expect.equal True
        , test "a mouse press on a steer pad does not brake" <|
            \_ ->
                TouchBrake.none
                    |> TouchBrake.press Left { held = True, touch = False }
                    |> TouchBrake.braking
                    |> Expect.equal False
        , test "the steer-pad brake adds the service brake and keeps steering" <|
            \_ ->
                let
                    input =
                        { steer = -1.0, brake = 0.0, handbrake = False, aeroTuck = 0.0, leanX = -0.85 }

                    braked =
                        TouchBrake.withSteerBrake { left = True, right = False } input
                in
                Expect.equal { input | brake = TouchBrake.steerPadBrake } braked
        , test "no steer-pad touch leaves the input unchanged" <|
            \_ ->
                let
                    input =
                        { steer = 1.0, brake = 0.0, handbrake = False, aeroTuck = 0.0, leanX = 0.85 }
                in
                Expect.equal input (TouchBrake.withSteerBrake TouchBrake.none input)
        , test "the pad decoder reads pointerType" <|
            \_ ->
                Expect.equal
                    [ Ok { held = True, touch = True }, Ok { held = True, touch = False }, Ok { held = False, touch = False } ]
                    [ Decode.decodeString (TouchBrake.padPressDecoder True) """{"pointerType":"touch"}"""
                    , Decode.decodeString (TouchBrake.padPressDecoder True) """{"pointerType":"mouse"}"""
                    , Decode.decodeString (TouchBrake.padPressDecoder False) "{}"
                    ]
        ]
