module IngestTest exposing (suite)

{-| Unit tests for `Racer.Ingest` URL/hash decoding and `Racer.Analyze` vehicle decomposition.
-}

import Dict
import EmbeddedParts
import Expect
import LDraw.Catalog as Catalog
import LDraw.Connectivity as Connectivity exposing (ConnectionType(..))
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Racer.Analyze as Analyze exposing (FallbackCacheMode(..))
import Racer.Ingest as Ingest exposing (IngestError(..))
import Racer.Types as Types exposing (SteeringMechanism(..), TrackId(..), ValidationCode(..), ValidationSeverity(..))
import Render.Camera as Camera
import Set
import Studio.Health as Health
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Model ingestion and vehicle analysis"
        [ ingestSuite
        , analyzeSuite
        ]


ingestSuite : Test
ingestSuite =
    describe "Racer.Ingest URL & hash codecs"
        [ test "roundtrips document, track, and seed through encodeRaceHash -> decodeRaceInput" <|
            \_ ->
                let
                    doc =
                        case Ingest.findPresetById "pinewood-classic" of
                            Just p ->
                                Document.parse p.ldrText

                            Nothing ->
                                Document.empty

                    encoded =
                        Ingest.encodeRaceHash ForestTrail 4242 doc
                in
                case Ingest.decodeRaceInput Nothing encoded of
                    Ok decoded ->
                        Expect.all
                            [ \_ -> Expect.equal (Just ForestTrail) decoded.trackId
                            , \_ -> Expect.equal (Just 4242) decoded.seed
                            , \_ -> Expect.equal (List.length (Document.mainPlacements doc)) (List.length (Document.mainPlacements decoded.document))
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Expected Ok from encoded race hash, got " ++ Debug.toString err)
        , test "roundtrips garage builder hash through encodeGarageHash -> decodeRaceInput" <|
            \_ ->
                let
                    doc =
                        case Ingest.findPresetById "pinewood-classic" of
                            Just p ->
                                Document.parse p.ldrText

                            Nothing ->
                                Document.empty

                    encoded =
                        Ingest.encodeGarageHash ForestTrail 555 doc
                in
                case Ingest.decodeRaceInput Nothing encoded of
                    Ok decoded ->
                        Expect.all
                            [ \_ -> Expect.equal (Just ForestTrail) decoded.trackId
                            , \_ -> Expect.equal (Just 555) decoded.seed
                            , \_ -> Expect.equal True decoded.isGarage
                            , \_ -> Expect.equal (List.length (Document.mainPlacements doc)) (List.length (Document.mainPlacements decoded.document))
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Expected Ok from encoded garage hash, got " ++ Debug.toString err)
        , test "decodes plain #garage hash to default soapbox preset with isGarage flag" <|
            \_ ->
                case Ingest.decodeRaceInput Nothing "#garage" of
                    Ok decoded ->
                        Expect.all
                            [ \_ -> Expect.equal True decoded.isGarage
                            , \_ -> Expect.equal False (List.isEmpty (Document.mainPlacements decoded.document))
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Expected Ok from #garage hash, got " ++ Debug.toString err)
        , test "extracts #b/ payload from full Brick Play URL and combined track URL" <|
            \_ ->
                let
                    doc =
                        Document.parse "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"

                    raceHash =
                        Ingest.encodeRaceHash MeadowPath 99 doc

                    fullUrl =
                        "https://catalog.palikkaharrastajat.fi/racer/" ++ raceHash
                in
                case Ingest.decodeRaceInput Nothing fullUrl of
                    Ok decoded ->
                        Expect.all
                            [ \_ -> Expect.equal (Just MeadowPath) decoded.trackId
                            , \_ -> Expect.equal (Just 99) decoded.seed
                            , \_ -> Expect.equal 1 (List.length (Document.mainPlacements decoded.document))
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed to decode full URL: " ++ Debug.toString err)
        , test "decodes viewer #model=...&parts=...&free=1 hash alongside a legacy track= id" <|
            \_ ->
                let
                    url =
                        "#track=pine-ridge&seed=7&model=1%204%200%200%200%201%200%200%200%201%200%200%200%201%203001.dat&free=1"
                in
                case Ingest.decodeRaceInput Nothing url of
                    Ok decoded ->
                        Expect.all
                            [ \_ -> Expect.equal (Just FarmLane) decoded.trackId
                            , \_ -> Expect.equal (Just 7) decoded.seed
                            , \_ -> Expect.equal 1 (List.length (Document.mainPlacements decoded.document))
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed to decode #model= hash: " ++ Debug.toString err)
        , test "rejects empty input cleanly" <|
            \_ ->
                Expect.equal (Err EmptyInput) (Ingest.decodeRaceInput Nothing "   ")
        , test "rejects malformed binary payload cleanly" <|
            \_ ->
                Expect.equal (Err InvalidBinaryPayload) (Ingest.decodeRaceInput Nothing "#b/%%%NOTVALID%%%")
        , test "decodes user minimal car hash and validates as drivable 4-wheel vehicle" <|
            \_ ->
                let
                    userHash =
                        "#b/c3JjZGBhMTEzMAASRiYsxmYmhizG5pamjHwM3AwgwA8mGU4wMjIwO3EdB1JMzYwMDEBeCyNYFMxlAkIoeZyRCSwHZDOD9DEzMDtKlgMA"
                in
                case Ingest.decodeRaceInput Nothing userHash of
                    Ok decoded ->
                        let
                            placements =
                                Document.mainPlacements decoded.document

                            analyzed =
                                Analyze.analyzeDocument "UserCar" decoded.document EmbeddedParts.cache Catalog.emptyPartIndex
                        in
                        Expect.all
                            [ \_ -> Expect.greaterThan 0 (List.length placements)
                            , \_ -> Expect.equal True analyzed.spec.isDrivable
                            , \_ -> Expect.equal 4 (List.length analyzed.spec.wheels)
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed to decode user car hash: " ++ Debug.toString err)
        , test "decodes user bumpy car hash and validates connectivity without loose bricks" <|
            \_ ->
                let
                    bumpyHash =
                        "#b/HYuxFYAwCETvIJUW1m4RTYzP0hSu5AC6WxxLAu9x8OGuXkQIucRosuaQSl5C2o9NiQnAaA3frD6SWodmQx4CRi_96igiZnJtFOk_I-05hZ7z_QM"
                in
                case Ingest.decodeRaceInput Nothing bumpyHash of
                    Ok decoded ->
                        let
                            placements =
                                Document.mainPlacements decoded.document

                            analyzed =
                                Analyze.analyzeDocument "BumpyCar" decoded.document EmbeddedParts.cache Catalog.emptyPartIndex
                        in
                        Expect.all
                            [ \_ -> Expect.equal 11 (List.length placements)
                            , \_ -> Expect.equal True analyzed.spec.isDrivable
                            , \_ -> Expect.equal 4 (List.length analyzed.spec.wheels)
                            , \_ -> Expect.equal [] analyzed.spec.loosePlacementIds
                            , \_ -> Expect.equal (List.repeat 4 False) (List.map .isSteerable analyzed.spec.wheels)
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed to decode bumpy car hash: " ++ Debug.toString err)
        , test "decodes user new build hash" <|
            \_ ->
                let
                    newHash =
                        "#b/fcsxEgFBEAXQ_6ebRWC2SiQSKlmbGbs7md1A6hCOQugOHNERdDmA5FV3_f-nC7HU0pk5qWjuylFzX09NHlK9mT-Wq5OSZ4M5fVUiAlijdfFm_JCUaQUy3AmEEH7Ig6BAxsNVvRg3Mw93-7kA56efQPv6s2wE4xYLbwNf"
                in
                case Ingest.decodeRaceInput Nothing newHash of
                    Ok decoded ->
                        let
                            analyzed =
                                Analyze.analyzeDocument "UserTurntableCar" decoded.document EmbeddedParts.cache Catalog.emptyPartIndex
                        in
                        Expect.all
                            [ \_ -> Expect.equal 16 (List.length (Document.mainPlacements decoded.document))
                            , \_ -> Expect.equal True analyzed.spec.isDrivable
                            , \_ -> Expect.equal 4 (List.length analyzed.spec.wheels)
                            , \_ -> Expect.equal [] analyzed.spec.loosePlacementIds
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed: " ++ Debug.toString err)
        , test "decodes user bug report URL with meadow-path, seed 5212, and b/ payload" <|
            \_ ->
                let
                    url =
                        "#track=meadow-path&seed=5212&b/dYm7DYRAEEM9O8Ohu-CIyeiAWXbFR2QbIDKKoAlyGqFDWmAgx5YsPztNhFxiq2rRRAlt9BI0WFP1ErUb8tA3w6o-m1EAEPzNjzYiTj84xwZEbicYOANT-p70-jNzKjcRRr1k9lYHfe59vAA"
                in
                case Ingest.decodeRaceInput Nothing url of
                    Ok decoded ->
                        Expect.all
                            [ \_ -> Expect.equal (Just MeadowPath) decoded.trackId
                            , \_ -> Expect.equal (Just 5212) decoded.seed
                            , \_ -> Expect.equal False (List.isEmpty (Document.mainPlacements decoded.document))
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed to decode: " ++ Debug.toString err)
        , test "All 4 built-in presets are 100% legal brick builds with zero Studio.Health issues, zero tidy moves, and zero loose parts" <|
            \_ ->
                let
                    checkPreset preset =
                        let
                            doc =
                                Document.parse preset.ldrText

                            healthIssues =
                                Health.check EmbeddedParts.cache doc Set.empty

                            tidyFixes =
                                Health.plan EmbeddedParts.cache doc Set.empty healthIssues

                            analyzed =
                                Analyze.analyzeDocument preset.nameEn doc EmbeddedParts.cache Catalog.emptyPartIndex

                            spec =
                                analyzed.spec

                            hasBlockerOrWarning =
                                List.any
                                    (\i -> i.severity == Blocker || i.severity == Warning)
                                    spec.issues

                            placements =
                                Document.mainPlacements doc

                            -- Verify every non-bottom studded part has at least one anti-stud coinciding within 0.5 LDU of a stud below it
                            allWorldStuds =
                                placements
                                    |> List.concatMap
                                        (\pl ->
                                            case Connectivity.knownCatalogPart pl.part of
                                                Just conn ->
                                                    conn.connections
                                                        |> List.filter (\c -> c.connType == Stud)
                                                        |> List.map (Connectivity.transformPoint pl.matrix pl.id)

                                                Nothing ->
                                                    []
                                        )

                            floatingStudParts =
                                placements
                                    |> List.filter
                                        (\pl ->
                                            case Connectivity.knownCatalogPart pl.part of
                                                Just conn ->
                                                    let
                                                        antis =
                                                            conn.connections
                                                                |> List.filter (\c -> c.connType == AntiStud)
                                                                |> List.map (Connectivity.transformPoint pl.matrix pl.id)

                                                        hasTech =
                                                            List.any
                                                                (\c ->
                                                                    case c.connType of
                                                                        TechnicPinMale _ ->
                                                                            True

                                                                        TechnicPinFemale _ ->
                                                                            True

                                                                        TechnicAxleMale _ ->
                                                                            True

                                                                        TechnicAxleFemale _ ->
                                                                            True

                                                                        _ ->
                                                                            False
                                                                )
                                                                conn.connections

                                                        matchesStud anti =
                                                            List.any
                                                                (\st ->
                                                                    st.placementId
                                                                        /= pl.id
                                                                        && Vec3.distance anti.worldPosition st.worldPosition
                                                                        <= 0.5
                                                                        && Vec3.dot anti.worldNormal st.worldNormal
                                                                        < -0.7
                                                                )
                                                                allWorldStuds
                                                    in
                                                    not (List.isEmpty antis) && not hasTech && not (List.any matchesStud antis)

                                                Nothing ->
                                                    True
                                        )
                                    |> List.map .id
                        in
                        { id = preset.id
                        , healthIssues = healthIssues
                        , tidyMoveCount = List.length tidyFixes
                        , isDrivable = spec.isDrivable
                        , loosePlacementIds = spec.loosePlacementIds
                        , floatingStudPartIds = floatingStudParts
                        , hasBlockerOrWarning = hasBlockerOrWarning
                        }

                    results =
                        List.map checkPreset Ingest.presets

                    expected =
                        List.map
                            (\p ->
                                { id = p.id
                                , healthIssues = []
                                , tidyMoveCount = 0
                                , isDrivable = True
                                , loosePlacementIds = []
                                , floatingStudPartIds = []
                                , hasBlockerOrWarning = False
                                }
                            )
                            Ingest.presets
                in
                Expect.equal expected results
        , test "decodes user grouped car hash and validates as drivable 4-wheel vehicle" <|
            \_ ->
                let
                    groupedHash =
                        "#b/ZY09DsIwFIP98gIpSKhITEyMbChtQiEjHZg5A5StCAnECeAecFTcserwfvxZsuujZM7GyvvZ83W-3ZtrW2za5kFURhuqWNiwS1sX9mW6eAofkhXMkQOYcMBPBMh-YozW5stj3gSqhqaqfigs9LA8jQjyxZh8tZZ-H9CZgEOXMuUehvwB"
                in
                case Ingest.decodeRaceInput Nothing groupedHash of
                    Ok decoded ->
                        let
                            analyzed =
                                Analyze.analyzeDocument "GroupedCar" decoded.document EmbeddedParts.cache Catalog.emptyPartIndex
                        in
                        Expect.all
                            [ \_ -> Expect.equal 4 (List.length analyzed.spec.wheels)
                            , \_ -> Expect.equal True analyzed.spec.isDrivable
                            ]
                            ()

                    Err err ->
                        Expect.fail ("Failed to decode grouped car hash: " ++ Debug.toString err)
        ]


analyzeSuite : Test
analyzeSuite =
    describe "Racer.Analyze vehicle decomposition"
        [ test "Turntable GP Kart detects 4 rubber wheels, turntable steering bogie, and steering wheel" <|
            \_ ->
                let
                    preset =
                        Ingest.presets |> List.head

                    doc =
                        preset |> Maybe.map (.ldrText >> Document.parse) |> Maybe.withDefault Document.empty

                    analyzed =
                        Analyze.analyzeDocument "Turntable GP Kart" doc EmbeddedParts.cache Catalog.emptyPartIndex

                    spec =
                        analyzed.spec
                in
                Expect.all
                    [ \_ -> Expect.equal True spec.isDrivable
                    , \_ -> Expect.equal 4 (List.length spec.wheels)
                    , \_ -> Expect.equal True (List.all .hasRubberTire spec.wheels)
                    , \_ ->
                        Expect.equal 2
                            (List.length (List.filter .isFront spec.wheels))
                    , \_ ->
                        case spec.steering of
                            TurntableBogie _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("Expected TurntableBogie, got " ++ Debug.toString other)
                    , \_ -> Expect.equal True spec.hasSteeringWheelIndicator
                    , \_ -> Expect.atLeast 0.0 spec.groundClearanceLdu
                    , \_ -> Expect.atLeast 0.5 spec.wheelBaseM
                    , \_ -> Expect.atLeast 0.25 spec.trackWidthM
                    ]
                    ()
        , test "Pinewood Plank Racer uses FixedAxleFallback and stays drivable" <|
            \_ ->
                let
                    doc =
                        Ingest.findPresetById "pinewood-classic"
                            |> Maybe.map (.ldrText >> Document.parse)
                            |> Maybe.withDefault Document.empty

                    spec =
                        (Analyze.analyzeDocument "Pinewood" doc EmbeddedParts.cache Catalog.emptyPartIndex).spec
                in
                Expect.all
                    [ \_ -> Expect.equal True spec.isDrivable
                    , \_ -> Expect.equal 4 (List.length spec.wheels)
                    , \_ -> Expect.equal FixedAxleFallback spec.steering
                    , \_ ->
                        Expect.equal True
                            (List.any (\i -> i.code == FixedAxleTip) spec.issues)
                    ]
                    ()
        , test "Trike Wedge detects 3 ground wheels (1 steerable front, 2 rear)" <|
            \_ ->
                let
                    doc =
                        Ingest.findPresetById "trike-wedge"
                            |> Maybe.map (.ldrText >> Document.parse)
                            |> Maybe.withDefault Document.empty

                    spec =
                        (Analyze.analyzeDocument "Trike" doc EmbeddedParts.cache Catalog.emptyPartIndex).spec
                in
                Expect.all
                    [ \_ -> Expect.equal True spec.isDrivable
                    , \_ -> Expect.equal 3 (List.length spec.wheels)
                    , \_ -> Expect.equal 1 (List.length (List.filter .isFront spec.wheels))
                    ]
                    ()
        , test "Alpine Trail Crawler has 4 off-road wheels, turntable steering, and zero loose parts" <|
            \_ ->
                let
                    doc =
                        Ingest.findPresetById "alpine-crawler"
                            |> Maybe.map (.ldrText >> Document.parse)
                            |> Maybe.withDefault Document.empty

                    spec =
                        (Analyze.analyzeDocument "Alpine" doc EmbeddedParts.cache Catalog.emptyPartIndex).spec

                    isTurntable =
                        case spec.steering of
                            TurntableBogie _ ->
                                True

                            _ ->
                                False
                in
                Expect.all
                    [ \_ -> Expect.equal True spec.isDrivable
                    , \_ -> Expect.equal 4 (List.length spec.wheels)
                    , \_ -> Expect.equal True isTurntable
                    , \_ -> Expect.equal [] spec.loosePlacementIds
                    ]
                    ()
        , test "Two-wheel model triggers TooFewWheels blocker and isDrivable == False" <|
            \_ ->
                let
                    twoWheeler =
                        String.join "\n"
                            [ "1 4 0 0 0 1 0 0 0 1 0 0 0 1 4600.dat"
                            , "1 15 28 5 0 0 0 1 0 1 0 -1 0 0 4624.dat"
                            , "1 0 28 5 0 0 0 1 0 1 0 -1 0 0 3641.dat"
                            , "1 15 -28 5 0 0 0 -1 0 1 0 1 0 0 4624.dat"
                            , "1 0 -28 5 0 0 0 -1 0 1 0 1 0 0 3641.dat"
                            ]

                    spec =
                        (Analyze.analyzeDocument "Bike" (Document.parse twoWheeler) EmbeddedParts.cache Catalog.emptyPartIndex).spec
                in
                Expect.all
                    [ \_ -> Expect.equal False spec.isDrivable
                    , \_ -> Expect.equal 2 (List.length spec.wheels)
                    , \_ ->
                        Expect.equal True
                            (List.any (\i -> i.severity == Blocker && i.code == TooFewWheels 2) spec.issues)
                    ]
                    ()
        , test "Belly-dragging brick below tires emits BellyScrape warning while staying drivable" <|
            \_ ->
                let
                    lowBelly =
                        String.join "\n"
                            [ "1 72 0 0 -50 1 0 0 0 1 0 0 0 1 4600.dat"
                            , "1 15 28 5 -50 0 0 1 0 1 0 -1 0 0 4624.dat"
                            , "1 0 28 5 -50 0 0 1 0 1 0 -1 0 0 3641.dat"
                            , "1 15 -28 5 -50 0 0 -1 0 1 0 1 0 0 4624.dat"
                            , "1 0 -28 5 -50 0 0 -1 0 1 0 1 0 0 3641.dat"
                            , "1 72 0 0 50 1 0 0 0 1 0 0 0 1 4600.dat"
                            , "1 15 28 5 50 0 0 1 0 1 0 -1 0 0 4624.dat"
                            , "1 0 28 5 50 0 0 1 0 1 0 -1 0 0 3641.dat"
                            , "1 15 -28 5 50 0 0 -1 0 1 0 1 0 0 4624.dat"
                            , "1 0 -28 5 50 0 0 -1 0 1 0 1 0 0 3641.dat"
                            , "1 4 0 20 0 1 0 0 0 1 0 0 0 1 3001.dat"
                            ]

                    spec =
                        (Analyze.analyzeDocument "LowBelly" (Document.parse lowBelly) EmbeddedParts.cache Catalog.emptyPartIndex).spec
                in
                Expect.all
                    [ \_ -> Expect.equal True spec.isDrivable
                    , \_ -> Expect.lessThan 0.0 spec.groundClearanceLdu
                    , \_ ->
                        Expect.equal True
                            (List.any
                                (\i ->
                                    case ( i.severity, i.code ) of
                                        ( Warning, BellyScrape _ ) ->
                                            True

                                        _ ->
                                            False
                                )
                                spec.issues
                            )
                    ]
                    ()
        , test "Heading normalization faces +Z regardless of whether car was built along +X" <|
            \_ ->
                let
                    sidewaysCar =
                        String.join "\n"
                            [ "1 72 -50 0 0 0 0 1 0 1 0 -1 0 0 4600.dat"
                            , "1 15 -50 5 28 1 0 0 0 1 0 0 0 1 4624.dat"
                            , "1 0 -50 5 28 1 0 0 0 1 0 0 0 1 3641.dat"
                            , "1 15 -50 5 -28 -1 0 0 0 1 0 0 0 -1 4624.dat"
                            , "1 0 -50 5 -28 -1 0 0 0 1 0 0 0 -1 3641.dat"
                            , "1 72 50 0 0 0 0 1 0 1 0 -1 0 0 4600.dat"
                            , "1 15 50 5 28 1 0 0 0 1 0 0 0 1 4624.dat"
                            , "1 0 50 5 28 1 0 0 0 1 0 0 0 1 3641.dat"
                            , "1 15 50 5 -28 -1 0 0 0 1 0 0 0 -1 4624.dat"
                            , "1 0 50 5 -28 -1 0 0 0 1 0 0 0 -1 3641.dat"
                            , "1 4 50 -16 0 0 0 1 0 1 0 -1 0 0 3829c01.dat"
                            ]

                    spec =
                        (Analyze.analyzeDocument "Sideways" (Document.parse sidewaysCar) EmbeddedParts.cache Catalog.emptyPartIndex).spec

                    ext =
                        Vec3.sub spec.boundsMaxLdu spec.boundsMinLdu
                in
                Expect.all
                    [ \_ -> Expect.equal True spec.isDrivable
                    , \_ -> Expect.greaterThan (Vec3.getX ext) (Vec3.getZ ext)
                    , \_ -> Expect.atLeast 0.4 spec.wheelBaseM
                    ]
                    ()
        , test "Disconnected floating brick is detected in loosePlacementIds" <|
            \_ ->
                let
                    withLoose =
                        (Ingest.findPresetById "pinewood-classic" |> Maybe.map .ldrText |> Maybe.withDefault "")
                            ++ "\n1 14 250 -150 250 1 0 0 0 1 0 0 0 1 3005.dat"

                    spec =
                        (Analyze.analyzeDocumentWithMode StrictCacheOnly "LooseTest" (Document.parse withLoose) EmbeddedParts.cache Catalog.emptyPartIndex).spec
                in
                Expect.equal 1 (List.length spec.loosePlacementIds)
        , test "Populated PartIndex descriptions for wheel holders (4600) and steering stands (3829c01) never create phantom wheels" <|
            \_ ->
                let
                    populatedIndex =
                        Catalog.fromPartList
                            [ { file = "4600.dat", description = "Plate 2 x 2 with Wheel Holder", category = "Plate" }
                            , { file = "3829c01.dat", description = "Car Steering Wheel and Stand", category = "Vehicle" }
                            , { file = "6014.dat", description = "Wheel 11 x 8", category = "Wheel" }
                            , { file = "56890.dat", description = "Tyre 24 x 12 R Balloon", category = "Tyre" }
                            , { file = "99998.dat", description = "Vehicle, Mudguard Arch 2 x 4 with Wheel Well", category = "Vehicle" }
                            ]

                    presetLdr =
                        Ingest.findPresetById "turntable-kart"
                            |> Maybe.map .ldrText
                            |> Maybe.withDefault ""

                    withUnknownArch =
                        presetLdr ++ "\n1 4 0 -16 0 1 0 0 0 1 0 0 0 1 99998.dat"

                    analyzed =
                        Analyze.analyzeDocument "Indexed Kart" (Document.parse withUnknownArch) EmbeddedParts.cache populatedIndex

                    spec =
                        analyzed.spec
                in
                Expect.all
                    [ \_ -> Expect.equal 4 (List.length spec.wheels)
                    , \_ -> Expect.equal 4 (List.length (List.filter .hasRubberTire spec.wheels))
                    , \_ -> Expect.equal True spec.hasSteeringWheelIndicator
                    , \_ -> Expect.equal [] spec.steeringWheelIds
                    , \_ -> Expect.equal Nothing analyzed.meshes.steeringWheel
                    ]
                    ()
        ]
