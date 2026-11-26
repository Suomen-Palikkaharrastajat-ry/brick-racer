module AnalyzeTest exposing (suite)

{-| Unit tests for the pure validation helpers of `Racer.Analyze`: grounded
wheels, centre-of-mass support, turntable linkage, fix actions, lint stars, the
stud-contact attachment graph and the cleanup checks (duplicate, far and
off-grid parts, surplus wheels) and the even wheel count.
-}

import Dict
import Expect
import LDraw.Catalog as Catalog
import LDraw.Document as Document
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3)
import Racer.Analyze as Analyze exposing (PlacementRecord, ValidationContext)
import Racer.Ingest as Ingest
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
        , snapSuite
        , farSuite
        , surplusWheelSuite
        , wheelCountSuite
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
    , offGridSnapSafe = False
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
        , test "three lifted wheels leave too few wheels, with the rim and tyre fix" <|
            \_ ->
                let
                    issues =
                        Analyze.buildValidationReport
                            (contextWith [ wheelAt 1 -30 50 0, wheelAt 2 30 50 15, wheelAt 3 -30 -50 15, wheelAt 4 30 -50 15 ])
                in
                Expect.all
                    [ \i -> Expect.equal True (List.member (WheelOffGround 3) (List.map .code i))
                    , \i ->
                        Expect.equal [ ( Blocker, Just (AddParts [ "4624.dat", "3641.dat" ]) ) ]
                            (i
                                |> List.filter (\issue -> issue.code == TooFewWheels 1)
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


{-| Whether two matrices agree entry by entry within 1.0e-9.
-}
sameEntries : Mat4 -> Mat4 -> Bool
sameEntries a b =
    let
        entries m =
            let
                r =
                    Mat4.toRecord m
            in
            [ r.m11, r.m12, r.m13, r.m14, r.m21, r.m22, r.m23, r.m24, r.m31, r.m32, r.m33, r.m34, r.m41, r.m42, r.m43, r.m44 ]
    in
    List.all identity (List.map2 (\x y -> abs (x - y) <= 1.0e-9) (entries a) (entries b))


{-| A one-file document of `( part, x, y, z )` placements (identity rotation,
colour 4), and the ids `Document.parse` gave them, in order.
-}
docOf : List ( String, ( Float, Float, Float ) ) -> ( Document.Document, List Int )
docOf parts =
    let
        line ( part, ( x, y, z ) ) =
            "1 4 " ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ " " ++ String.fromFloat z ++ " 1 0 0 0 1 0 0 0 1 " ++ part

        doc =
            Document.parse ("0 Name: Snap\n" ++ String.join "\n" (List.map line parts) ++ "\n")
    in
    ( doc, List.map .id (Document.mainPlacements doc) )


{-| Whether snapping the placement at `index` of `docOf parts` is safe, with an
empty cache (procedural fallback boxes).
-}
snapSafeAt : Int -> List ( String, ( Float, Float, Float ) ) -> Bool
snapSafeAt index parts =
    let
        ( doc, ids ) =
            docOf parts
    in
    Analyze.snapIsSafe Dict.empty Catalog.emptyPartIndex doc (List.drop index ids |> List.take 1)


snapSuite : Test
snapSuite =
    describe "snapping off-grid parts"
        [ test "a 2 x 4 brick 3 LDU over in X and 1 LDU off in Y snaps to the nearest stud and plate" <|
            \_ -> Expect.equal True (sameEntries (at 0 -24 0) (Analyze.snapMatrix (at 3 -23 0)))
        , test "X and Z round to 10 LDU, Y to 8 LDU" <|
            \_ -> Expect.equal True (sameEntries (at 20 16 -30) (Analyze.snapMatrix (at 16 13 -27)))
        , test "a part turned 20 degrees snaps back to straight and a part turned 80 degrees to a quarter turn" <|
            \_ ->
                Expect.equal [ True, True ]
                    [ sameEntries (at 10 0 0) (Analyze.snapMatrix (Mat4.mul (at 12 0 0) (Mat4.makeRotate (pi / 9) (vec3 0 1 0))))
                    , sameEntries (Mat4.makeRotate (pi / 2) (vec3 0 1 0)) (Analyze.snapMatrix (Mat4.makeRotate (4 * pi / 9) (vec3 0 1 0)))
                    ]
        , test "a snapped part is on grid" <|
            \_ ->
                Expect.equal False
                    (Analyze.isOffGrid "3001.dat" (Analyze.snapMatrix (Mat4.mul (at 3 -23 7) (Mat4.makeRotate 0.3 (vec3 0 1 0)))))
        , test "a brick lifted 1 LDU and slid 3 LDU off the brick below is safe to snap onto its studs" <|
            \_ -> Expect.equal True (snapSafeAt 1 [ ( "3001.dat", ( 0, 0, 0 ) ), ( "3001.dat", ( 3, -23, 0 ) ) ])
        , test "a brick 3 LDU beside another snaps inside it, so it is not safe" <|
            \_ -> Expect.equal False (snapSafeAt 1 [ ( "3001.dat", ( 0, 0, 0 ) ), ( "3001.dat", ( 3, 0, 0 ) ) ])
        , test "hinges and rims are never snapped" <|
            \_ ->
                Expect.equal [ False, False ]
                    [ snapSafeAt 1 [ ( "3001.dat", ( 0, 0, 0 ) ), ( "3937.dat", ( 3, -23, 0 ) ) ]
                    , snapSafeAt 1 [ ( "4600.dat", ( 0, 0, 0 ) ), ( "4624.dat", ( 33, 5, 3 ) ) ]
                    ]
        , test "a safe snap is offered as a snap fix, otherwise the parts are shown" <|
            \_ ->
                let
                    ctx =
                        contextWith fourWheels

                    offGridFix safe =
                        Analyze.buildValidationReport { ctx | offGridIds = [ 5 ], offGridSnapSafe = safe }
                            |> List.filter (\issue -> issue.code == OffGrid [ 5 ])
                            |> List.map .fix
                in
                Expect.equal [ [ Just (Snap [ 5 ]) ], [ Just (Select [ 5 ]) ] ] [ offGridFix True, offGridFix False ]
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


{-| The blockers of a car standing on `wheels`, as `( code, fix )`.
-}
blockersOn : List WheelSpec -> List ( ValidationCode, Maybe Fix )
blockersOn wheels =
    Analyze.buildValidationReport (contextWith wheels)
        |> List.filter (\issue -> issue.severity == Blocker)
        |> List.map (\issue -> ( issue.code, issue.fix ))


{-| A two-wheel car: one wheel on the centreline in front, one behind.
-}
inlineTwo : List WheelSpec
inlineTwo =
    [ wheelAt 1 0 50 0, wheelAt 2 0 -50 0 ]


wheelCountSuite : Test
wheelCountSuite =
    describe "even wheel count"
        [ test "no wheel is too few, with the rim and tyre fix" <|
            \_ -> Expect.equal [ ( TooFewWheels 0, Just (AddParts Analyze.wheelPairFiles) ) ] (blockersOn [])
        , test "one wheel is too few, with the rim and tyre fix" <|
            \_ -> Expect.equal [ ( TooFewWheels 1, Just (AddParts Analyze.wheelPairFiles) ) ] (blockersOn [ wheelAt 1 0 50 0 ])
        , test "two wheels one behind the other have no blocker" <|
            \_ -> Expect.equal [] (blockersOn inlineTwo)
        , test "two wheels side by side on one axle are collinear" <|
            \_ -> Expect.equal [ ( CollinearStance, Nothing ) ] (blockersOn [ wheelAt 1 -30 0 0, wheelAt 2 30 0 0 ])
        , test "two wheels 10 LDU apart fore-aft are collinear" <|
            \_ -> Expect.equal [ ( CollinearStance, Nothing ) ] (blockersOn [ wheelAt 1 0 5 0, wheelAt 2 0 -5 0 ])
        , test "three wheels are an odd count that shows the wheel without a partner" <|
            \_ ->
                Expect.equal [ ( WheelCountNotEven 3, Just (Select [ 10, 11 ]) ) ]
                    (blockersOn [ wheelAt 1 0 50 0, wheelAt 2 -30 -50 0, wheelAt 3 30 -50 0 ])
        , test "four wheels have no blocker" <|
            \_ -> Expect.equal [] (blockersOn fourWheels)
        , test "five wheels are an odd count that shows the wheel without a partner" <|
            \_ ->
                Expect.equal [ ( WheelCountNotEven 5, Just (Select [ 50, 51 ]) ) ]
                    (blockersOn (fourWheels ++ [ wheelAt 5 0 0 0 ]))
        , test "six wheels on three axles have no blocker and no extra wheels" <|
            \_ ->
                let
                    codes =
                        codesOf (contextWith (fourWheels ++ [ wheelAt 5 -30 0 0, wheelAt 6 30 0 0 ]))
                in
                Expect.all
                    [ \_ -> Expect.equal [] (blockersOn (fourWheels ++ [ wheelAt 5 -30 0 0, wheelAt 6 30 0 0 ]))
                    , \c -> Expect.equal [] (List.filter (\code -> code == ExtraWheels 0) c)
                    ]
                    codes
        , test "a mirrored pair is not unpaired; a lone wheel is" <|
            \_ ->
                Expect.equal [ 3 ]
                    (List.map .id (Analyze.unpairedWheels [ wheelAt 1 -30 -50 0, wheelAt 2 30 -50 0, wheelAt 3 0 50 0 ]))
        , test "a two-wheel car with its front wheel on a turntable earns the steering bonus" <|
            \_ ->
                let
                    ctx =
                        contextWith inlineTwo

                    codes =
                        codesOf
                            { ctx
                                | steering =
                                    TurntableBogie
                                        { pivotPosLdu = vec3 0 20 50
                                        , pivotPosM = Types.vec3LduToMeters (vec3 0 20 50)
                                        , bogiePlacementIds = [ 10, 11 ]
                                        }
                            }
                in
                Expect.all
                    [ \c -> Expect.equal True (List.member RealSteeringBonus c)
                    , \c -> Expect.equal False (List.member SteeringNotLinked c)
                    , \_ -> Expect.equal [] (blockersOn inlineTwo)
                    ]
                    codes
        , test "a two-wheel car rates like a four-wheel car" <|
            \_ ->
                let
                    stars wheels =
                        Analyze.starRating (Analyze.buildValidationReport (contextWith wheels)) True
                in
                Expect.equal (stars fourWheels) (stars inlineTwo)
        , test "turntable kart wheels have concentric rim and tire pairing without vertical hub distortion" <|
            \_ ->
                let
                    doc =
                        Ingest.findPresetById "turntable-kart"
                            |> Maybe.map (.ldrText >> Document.parse)
                            |> Maybe.withDefault Document.empty

                    analyzed =
                        Analyze.analyzeDocument "Turntable GP Kart" doc Dict.empty Catalog.emptyPartIndex

                    wheels =
                        analyzed.spec.wheels
                in
                Expect.all
                    [ \ws -> Expect.equal 4 (List.length ws)
                    , \ws -> Expect.equal True (List.all (\w -> Vec3.getX w.spinAxis > 0.0) ws)
                    , \ws ->
                        let
                            frontY =
                                List.filter .isFront ws |> List.map (\w -> Vec3.getY w.localHubPosLdu)

                            rearY =
                                List.filter (not << .isFront) ws |> List.map (\w -> Vec3.getY w.localHubPosLdu)
                        in
                        case ( frontY, rearY ) of
                            ( [ fy1, fy2 ], [ ry1, ry2 ] ) ->
                                Expect.all
                                    [ \_ -> Expect.within (Expect.Absolute 0.01) fy1 fy2
                                    , \_ -> Expect.within (Expect.Absolute 0.01) ry1 ry2
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 2 front and 2 rear wheels"
                    ]
                    wheels
        ]
