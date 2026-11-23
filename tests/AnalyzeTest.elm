module AnalyzeTest exposing (suite)

{-| Unit tests for the pure validation helpers of `Racer.Analyze`: grounded
wheels, centre-of-mass support, turntable linkage, fix actions, lint stars, the
stud-contact attachment graph and the cleanup checks (duplicate, far and
off-grid parts, surplus wheels).
-}

import Dict
import Expect
import LDraw.Catalog as Catalog
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (vec3)
import Racer.Analyze as Analyze exposing (PlacementRecord, ValidationContext)
import Racer.Types as Types exposing (Fix(..), SteeringMechanism(..), ValidationCode(..), ValidationIssue, ValidationSeverity(..), WheelSpec)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Racer.Analyze validation"
        [ groundedSuite
        , supportSuite
        , steeringSuite
        , starSuite
        , attachmentSuite
        , duplicateSuite
        , gridSuite
        , farSuite
        , surplusWheelSuite
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
    , looseIds = []
    , missingCount = 0
    , steering = FixedAxleFallback
    , wheels = wheels
    , dragCoeff = 0.9
    , duplicateIds = []
    , farIds = []
    , offGridIds = []
    , placementMassGrams = Dict.empty
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


{-| An LDraw placement matrix from a type 1 line's `x y z a b c d e f g h i`.
-}
ldraw : Float -> Float -> Float -> ( Float, Float, Float ) -> ( Float, Float, Float ) -> ( Float, Float, Float ) -> Mat4
ldraw x y z ( a, b, c ) ( d, e, f ) ( g, h, i ) =
    Mat4.fromRecord
        { m11 = a
        , m12 = b
        , m13 = c
        , m14 = x
        , m21 = d
        , m22 = e
        , m23 = f
        , m24 = y
        , m31 = g
        , m32 = h
        , m33 = i
        , m34 = z
        , m41 = 0
        , m42 = 0
        , m43 = 0
        , m44 = 1
        }


{-| A record for placement `id` of `part` at `matrix`, measured offline from its
connectivity boxes.
-}
recordOf : Int -> String -> Mat4 -> PlacementRecord
recordOf id part matrix =
    Analyze.inspectPlacement Dict.empty
        Catalog.emptyPartIndex
        { id = id, part = part, fileText = "", color = 4, matrix = matrix, extra = [] }


at : Float -> Float -> Float -> Mat4
at x y z =
    Mat4.makeTranslate3 x y z


{-| The rotation of the starter bases' right-hand rims (spin axis along X).
-}
rimRight : Float -> Float -> Float -> Mat4
rimRight x y z =
    ldraw x y z ( 0, 0, 1 ) ( 0, 1, 0 ) ( -1, 0, 0 )


attachmentSuite : Test
attachmentSuite =
    describe "stud-contact attachment graph"
        [ test "three stacked 2 x 2 plates are one component; a plate shifted 5 LDU is not on their studs" <|
            \_ ->
                Expect.equal [ [ 1, 2, 3 ], [ 4 ] ]
                    (Analyze.attachmentComponents
                        [ recordOf 1 "3022.dat" (at 0 0 0)
                        , recordOf 2 "3022.dat" (at 0 -8 0)
                        , recordOf 3 "3022.dat" (at 0 -16 0)
                        , recordOf 4 "3022.dat" (at 5 -24 0)
                        ]
                    )
        , test "the shifted plate is the loose part" <|
            \_ ->
                Expect.equal [ 4 ]
                    (Analyze.findLoosePlacements
                        [ recordOf 1 "3022.dat" (at 0 0 0)
                        , recordOf 2 "3022.dat" (at 0 -8 0)
                        , recordOf 3 "3022.dat" (at 0 -16 0)
                        , recordOf 4 "3022.dat" (at 5 -24 0)
                        ]
                    )
        , test "a rim on the holder pin and its tyre are attached; a rim hovering under the holder is loose" <|
            \_ ->
                Expect.equal [ 4 ]
                    (Analyze.findLoosePlacements
                        [ recordOf 1 "4600.dat" (at 0 0 0)
                        , recordOf 2 "4624.dat" (rimRight 33 5 0)
                        , recordOf 3 "3641.dat" (rimRight 33 5 0)
                        , recordOf 4 "4624.dat" (rimRight -33 25 0)
                        ]
                    )
        , test "a 6014b rim seated 12.75 LDU along the pin and its offset tyre stay attached" <|
            \_ ->
                Expect.equal []
                    (Analyze.findLoosePlacements
                        [ recordOf 1 "4600.dat" (at 0 0 0)
                        , recordOf 2 "6014b.dat" (ldraw 34 5 0 ( 0, 0, -1 ) ( 0, 1, 0 ) ( 1, 0, 0 ))
                        , recordOf 3 "56890.dat" (rimRight 40 5 0)
                        ]
                    )
        , test "a part with no connectivity entry is joined by touching boxes" <|
            \_ ->
                Expect.equal []
                    (Analyze.findLoosePlacements
                        [ recordOf 1 "3022.dat" (at 0 0 0)
                        , recordOf 2 "99999.dat" (at 0 -20 0)
                        ]
                    )
        , test "the heavier component is the car" <|
            \_ ->
                Expect.equal [ 1 ]
                    (Analyze.findLoosePlacements
                        [ recordOf 1 "3024.dat" (at 200 0 0)
                        , recordOf 2 "3001.dat" (at 0 0 0)
                        , recordOf 3 "3001.dat" (at 0 -24 0)
                        ]
                    )
        , test "loose parts are a warning that selects them" <|
            \_ ->
                let
                    ctx =
                        contextWith fourWheels
                in
                Expect.equal [ ( Warning, Just (Select [ 7, 9 ]) ) ]
                    (Analyze.buildValidationReport { ctx | looseIds = [ 7, 9 ] }
                        |> List.filter (\issue -> issue.code == LooseParts [ 7, 9 ])
                        |> List.map (\issue -> ( issue.severity, issue.fix ))
                    )
        ]


duplicateSuite : Test
duplicateSuite =
    describe "duplicate parts"
        [ test "later copies at the same matrix or the same box are duplicates" <|
            \_ ->
                Expect.equal [ 2, 4, 6 ]
                    (Analyze.findDuplicateParts
                        [ recordOf 1 "3022.dat" (at 0 0 0)
                        , recordOf 2 "3022.dat" (at 0.3 0 0)
                        , recordOf 3 "3022.dat" (at 20 0 0)
                        , recordOf 4 "3022.dat" (ldraw 0 0 0 ( 0, 0, 1 ) ( 0, 1, 0 ) ( -1, 0, 0 ))
                        , recordOf 5 "3020.dat" (at 0 0 0)
                        , recordOf 6 "3022.dat" (at 0 0 0)
                        ]
                    )
        , test "a stacked copy is not a duplicate" <|
            \_ ->
                Expect.equal []
                    (Analyze.findDuplicateParts
                        [ recordOf 1 "3022.dat" (at 0 0 0)
                        , recordOf 2 "3022.dat" (at 0 -8 0)
                        ]
                    )
        , test "duplicates are info with a remove fix" <|
            \_ ->
                let
                    ctx =
                        contextWith fourWheels
                in
                Expect.equal [ ( Info, Just (Remove [ 3 ]) ) ]
                    (Analyze.buildValidationReport { ctx | duplicateIds = [ 3 ] }
                        |> List.filter (\issue -> issue.code == DuplicateParts [ 3 ])
                        |> List.map (\issue -> ( issue.severity, issue.fix ))
                    )
        ]


gridSuite : Test
gridSuite =
    describe "off-grid parts"
        [ test "a plate on the stud grid is on grid" <|
            \_ -> Expect.equal False (Analyze.isOffGrid "3022.dat" (at 10 -8 20))
        , test "half a stud over, a third of a plate up or turned 45 degrees is off grid" <|
            \_ ->
                Expect.equal [ True, True, True ]
                    [ Analyze.isOffGrid "3022.dat" (at 5 0 0)
                    , Analyze.isOffGrid "3022.dat" (at 0 -4 0)
                    , Analyze.isOffGrid "3022.dat" (Mat4.makeRotate (pi / 4) (vec3 0 1 0))
                    ]
        , test "a quarter turn is on grid" <|
            \_ -> Expect.equal False (Analyze.isOffGrid "3022.dat" (rimRight 20 -8 0))
        , test "turntables, hinges, jumpers, rims and tyres are never off grid" <|
            \_ ->
                Expect.equal [ False, False, False, False, False ]
                    (List.map (\part -> Analyze.isOffGrid part (rimRight 33 5 3))
                        [ "3679.dat", "3937.dat", "3794b.dat", "4624.dat", "3641.dat" ]
                    )
        , test "an unknown part is checked too" <|
            \_ -> Expect.equal True (Analyze.isOffGrid "99999.dat" (at 3 0 0))
        ]


farSuite : Test
farSuite =
    let
        car =
            { min = vec3 -40 0 -80, max = vec3 40 40 80 }
    in
    describe "far parts"
        [ test "a part 60 LDU beside the car is not far" <|
            \_ -> Expect.equal False (Analyze.isFarOutside Analyze.farPartDistanceLdu car { min = vec3 100 0 0, max = vec3 120 24 20 })
        , test "a part 90 LDU beside the car is far" <|
            \_ -> Expect.equal True (Analyze.isFarOutside Analyze.farPartDistanceLdu car { min = vec3 130 0 0, max = vec3 150 24 20 })
        , test "a part 90 LDU above the car is far" <|
            \_ -> Expect.equal True (Analyze.isFarOutside Analyze.farPartDistanceLdu car { min = vec3 0 130 0, max = vec3 20 154 20 })
        , test "far parts are info that selects them" <|
            \_ ->
                let
                    ctx =
                        contextWith fourWheels
                in
                Expect.equal [ ( Info, Just (Select [ 5 ]) ) ]
                    (Analyze.buildValidationReport { ctx | farIds = [ 5 ] }
                        |> List.filter (\issue -> issue.code == FarParts [ 5 ])
                        |> List.map (\issue -> ( issue.severity, issue.fix ))
                    )
        ]


surplusWheelSuite : Test
surplusWheelSuite =
    describe "surplus wheels"
        [ test "one wheel per corner has no surplus" <|
            \_ -> Expect.equal [] (Analyze.surplusWheelClusters fourWheels)
        , test "a second wheel on the front left corner is surplus" <|
            \_ -> Expect.equal [ 5 ] (List.map .id (Analyze.surplusWheelClusters (fourWheels ++ [ wheelAt 5 -30 70 0 ])))
        , test "a grounded wheel is kept before a lifted one on the same corner" <|
            \_ -> Expect.equal [ 1 ] (List.map .id (Analyze.surplusWheelClusters [ wheelAt 1 -30 50 10, wheelAt 2 -30 60 0 ]))
        , test "surplus wheels are a warning that removes them and reports the grams saved" <|
            \_ ->
                let
                    ctx =
                        contextWith (fourWheels ++ [ wheelAt 5 -30 70 0 ])
                in
                Expect.equal [ ( Warning, Just (Remove [ 50, 51 ]) ) ]
                    (Analyze.buildValidationReport { ctx | placementMassGrams = Dict.fromList [ ( 50, 1.2 ), ( 51, 2.8 ) ] }
                        |> List.filter (\issue -> issue.code == ExtraWheels 4)
                        |> List.map (\issue -> ( issue.severity, issue.fix ))
                    )
        ]
