module AnalyzeTest exposing (suite)

{-| Unit tests for the pure validation helpers of `Racer.Analyze`: grounded
wheels, centre-of-mass support, turntable linkage, fix actions and lint stars.
-}

import Expect
import Math.Vector3 exposing (vec3)
import Racer.Analyze as Analyze exposing (ValidationContext)
import Racer.Types as Types exposing (Fix(..), SteeringMechanism(..), ValidationCode(..), ValidationIssue, ValidationSeverity(..), WheelSpec)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Racer.Analyze validation"
        [ groundedSuite
        , supportSuite
        , steeringSuite
        , starSuite
        ]


{-| A 12 LDU wheel at car-space `x`, `z` (LDU), its tyre bottom `lift` LDU above
the contact plane. Its placement ids are `10 * id` and `10 * id + 1`.
-}
wheelAt : Int -> Float -> Float -> Float -> WheelSpec
wheelAt id x z lift =
    let
        hub =
            vec3 x (12.0 + lift) z
    in
    { id = id
    , placementIds = [ 10 * id, 10 * id + 1 ]
    , localHubPosLdu = hub
    , localHubPosM = Types.vec3LduToMeters hub
    , spinAxis = vec3 1 0 0
    , radiusLdu = 12.0
    , radiusM = Types.lduToMeters 12.0
    , widthLdu = 8.0
    , widthM = Types.lduToMeters 8.0
    , isFront = z > 0
    , isSteerable = False
    , hasRubberTire = True
    }


{-| Four grounded wheels, 60 LDU track and 100 LDU wheelbase around the origin.
-}
fourWheels : List WheelSpec
fourWheels =
    [ wheelAt 1 -30 50 0
    , wheelAt 2 30 50 0
    , wheelAt 3 -30 -50 0
    , wheelAt 4 30 -50 0
    ]


contextWith : List WheelSpec -> ValidationContext
contextWith wheels =
    { placementCount = 20
    , groundClearanceLdu = 4.0
    , comM = vec3 0 0.05 0
    , looseCount = 0
    , missingCount = 0
    , steering = FixedAxleFallback
    , wheels = wheels
    , dragCoeff = 0.9
    }


codesOf : ValidationContext -> List ValidationCode
codesOf ctx =
    List.map .code (Analyze.buildValidationReport ctx)


hasNoseOverRisk : List ValidationCode -> Bool
hasNoseOverRisk codes =
    List.any
        (\c ->
            case c of
                NoseOverRisk _ ->
                    True

                _ ->
                    False
        )
        codes


groundedSuite : Test
groundedSuite =
    describe "grounded wheels"
        [ test "a wheel within 2 LDU of the ground is grounded" <|
            \_ ->
                Expect.equal [ True, True, False ]
                    (List.map Analyze.isWheelGrounded [ wheelAt 1 0 0 0, wheelAt 2 0 0 1.5, wheelAt 3 0 0 10 ])
        , test "a lifted fifth wheel is reported as WheelOffGround and not counted" <|
            \_ ->
                let
                    codes =
                        codesOf (contextWith (fourWheels ++ [ wheelAt 5 0 0 20 ]))
                in
                Expect.all
                    [ \c -> Expect.equal True (List.member (WheelOffGround 1) c)
                    , \c -> Expect.equal False (List.member CoMOutsideSupport c)
                    , \c -> Expect.equal [] (List.filter (\code -> code == TooFewWheels 4 || code == TooFewWheels 5) c)
                    ]
                    codes
        , test "two lifted wheels leave too few wheels, with the rim and tyre fix" <|
            \_ ->
                let
                    issues =
                        Analyze.buildValidationReport
                            (contextWith [ wheelAt 1 -30 50 0, wheelAt 2 30 50 0, wheelAt 3 -30 -50 15, wheelAt 4 30 -50 15 ])
                in
                Expect.all
                    [ \i -> Expect.equal True (List.member (WheelOffGround 2) (List.map .code i))
                    , \i ->
                        Expect.equal [ ( Blocker, Just (AddParts [ "4624.dat", "3641.dat" ]) ) ]
                            (i
                                |> List.filter (\issue -> issue.code == TooFewWheels 2)
                                |> List.map (\issue -> ( issue.severity, issue.fix ))
                            )
                    ]
                    issues
        , test "a fixed-axle car offers the turntable pair" <|
            \_ ->
                Expect.equal [ Just (AddParts Analyze.turntablePairFiles) ]
                    (Analyze.buildValidationReport (contextWith fourWheels)
                        |> List.filter (\issue -> issue.code == FixedAxleTip)
                        |> List.map .fix
                    )
        , test "the turntable pair is the base and the top" <|
            \_ -> Expect.equal [ "3680.dat", "3679.dat" ] Analyze.turntablePairFiles
        ]


supportSuite : Test
supportSuite =
    describe "centre of mass support"
        [ test "a centred centre of mass has half the wheelbase as margin" <|
            \_ ->
                case Analyze.comSupportMargin 0 fourWheels of
                    Just margin ->
                        Expect.within (Expect.Absolute 1.0e-6) 0.5 margin

                    Nothing ->
                        Expect.fail "expected a margin"
        , test "wheels in one line across give no margin" <|
            \_ -> Expect.equal Nothing (Analyze.comSupportMargin 0 [ wheelAt 1 -30 0 0, wheelAt 2 30 0 0 ])
        , test "a centred car has no support issue" <|
            \_ ->
                let
                    codes =
                        codesOf (contextWith fourWheels)
                in
                Expect.all
                    [ \c -> Expect.equal False (List.member CoMOutsideSupport c)
                    , \c -> Expect.equal False (hasNoseOverRisk c)
                    ]
                    codes
        , test "a centre of mass ahead of the front axle is a blocker" <|
            \_ ->
                let
                    ctx =
                        contextWith fourWheels
                in
                Expect.equal True
                    (List.any (\issue -> issue.severity == Blocker && issue.code == CoMOutsideSupport)
                        (Analyze.buildValidationReport { ctx | comM = vec3 0 0.05 0.5 })
                    )
        , test "a centre of mass just behind the front axle is a NoseOverRisk warning" <|
            \_ ->
                let
                    ctx =
                        contextWith fourWheels

                    -- Front hubs at 50 LDU (0.3125 m), wheelbase 0.625 m: 5 % margin.
                    codes =
                        codesOf { ctx | comM = vec3 0 0.05 0.28125 }
                in
                Expect.all
                    [ \c -> Expect.equal True (hasNoseOverRisk c)
                    , \c -> Expect.equal False (List.member CoMOutsideSupport c)
                    ]
                    codes
        ]


steeringSuite : Test
steeringSuite =
    let
        bogieWith ids =
            TurntableBogie
                { pivotPosLdu = vec3 0 20 50
                , pivotPosM = Types.vec3LduToMeters (vec3 0 20 50)
                , bogiePlacementIds = ids
                }

        withSteering mech =
            let
                ctx =
                    contextWith fourWheels
            in
            codesOf { ctx | steering = mech }
    in
    describe "turntable linkage"
        [ test "a turntable with no wheel on it is not linked and earns no bonus" <|
            \_ ->
                Expect.all
                    [ \c -> Expect.equal True (List.member SteeringNotLinked c)
                    , \c -> Expect.equal False (List.member RealSteeringBonus c)
                    ]
                    (withSteering (bogieWith [ 999 ]))
        , test "a turntable carrying the front wheels earns the bonus" <|
            \_ ->
                Expect.all
                    [ \c -> Expect.equal False (List.member SteeringNotLinked c)
                    , \c -> Expect.equal True (List.member RealSteeringBonus c)
                    ]
                    (withSteering (bogieWith [ 999, 10, 11, 20, 21 ]))
        , test "only rear wheels on the bogie ids do not link it" <|
            \_ -> Expect.equal True (List.member SteeringNotLinked (withSteering (bogieWith [ 30, 31, 40, 41 ])))
        ]


issueOf : ValidationSeverity -> ValidationIssue
issueOf severity =
    { severity = severity, code = EmptyModel, detail = "", fix = Nothing }


starSuite : Test
starSuite =
    describe "lint stars"
        [ test "a clean drivable car has three stars" <|
            \_ -> Expect.equal 3 (Analyze.starRating [] True)
        , test "a warning costs a star" <|
            \_ -> Expect.equal 2 (Analyze.starRating [ issueOf Warning ] True)
        , test "a blocker leaves no stars" <|
            \_ -> Expect.equal 0 (Analyze.starRating [ issueOf Blocker, issueOf Bonus ] False)
        , test "bonuses add stars up to five" <|
            \_ -> Expect.equal 5 (Analyze.starRating (List.map issueOf [ Bonus, Bonus, Bonus, Info ]) True)
        , test "the rating never drops below one" <|
            \_ -> Expect.equal 1 (Analyze.starRating (List.map issueOf [ Warning, Warning, Warning, Warning ]) True)
        , test "a car that is not drivable is capped at two" <|
            \_ -> Expect.equal 2 (Analyze.starRating (List.map issueOf [ Bonus, Bonus ]) False)
        ]
