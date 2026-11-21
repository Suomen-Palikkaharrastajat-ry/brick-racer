module RacerSmokeTest exposing (suite)

{-| Initial smoke test ensuring test harness and toolchain wiring work cleanly.
-}

import Expect
import Test exposing (Test, describe, test)


{-| Verify Stage 1 test runner boots cleanly.
-}
suite : Test
suite =
    describe "Brick Racer Stage 1 Smoke"
        [ test "test runner active" <|
            \_ -> Expect.equal 2 (1 + 1)
        ]
