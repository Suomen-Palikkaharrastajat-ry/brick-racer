module PlayFeelTest exposing (suite)

{-| Tests for the phone play fixes: tilt steering clears the touch pads.
-}

import Expect
import Racer.HUD as HUD
import Test exposing (Test, describe, test)


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
        ]
