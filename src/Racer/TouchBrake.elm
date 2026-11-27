module Racer.TouchBrake exposing
    ( PadPress
    , Side(..)
    , SteerTouch
    , braking
    , none
    , padPressDecoder
    , press
    , steerPadBrake
    , withSteerBrake
    )

{-| Touch brake on the on-screen steer pads.

A finger on the left or right steer pad (a pointer whose `pointerType` is
`"touch"`) still steers as before, and also brakes for as long as it is held.
Mouse and pen presses on the pads only steer. Lifting, cancelling or sliding
the finger off the pad clears that side's brake flag.

-}

import Json.Decode as Decode exposing (Decoder)
import Racer.Types exposing (InputState)


{-| Which steer pad was pressed.
-}
type Side
    = Left
    | Right


{-| One pointer event on a steer pad: whether the pad is now held, and whether
the pointer is a finger.
-}
type alias PadPress =
    { held : Bool
    , touch : Bool
    }


{-| Per-side flags: the pad is held down by a finger.
-}
type alias SteerTouch =
    { left : Bool
    , right : Bool
    }


{-| No finger on either steer pad.
-}
none : SteerTouch
none =
    { left = False, right = False }


{-| Apply a pad press: the side brakes only while held by a touch pointer.
-}
press : Side -> PadPress -> SteerTouch -> SteerTouch
press side pad st =
    let
        on =
            pad.held && pad.touch
    in
    case side of
        Left ->
            { st | left = on }

        Right ->
            { st | right = on }


{-| Whether a finger holds either steer pad.
-}
braking : SteerTouch -> Bool
braking st =
    st.left || st.right


{-| Service brake amount (0 .. 1, the same scale as the brake pad and the
S / Down keys) applied while a steer pad is touched.
-}
steerPadBrake : Float
steerPadBrake =
    1.0


{-| Add the steer-pad brake to sampled input. Steering, tuck and the
handbrake are left as they are.
-}
withSteerBrake : SteerTouch -> InputState -> InputState
withSteerBrake st input =
    if braking st then
        { input | brake = max input.brake steerPadBrake }

    else
        input


{-| Decode a pointer event on a steer pad into a `PadPress` with the given
held state; a missing `pointerType` counts as not touch.
-}
padPressDecoder : Bool -> Decoder PadPress
padPressDecoder held =
    Decode.oneOf [ Decode.field "pointerType" Decode.string, Decode.succeed "" ]
        |> Decode.map (\kind -> { held = held, touch = kind == "touch" })
