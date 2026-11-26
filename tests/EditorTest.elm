module EditorTest exposing (suite)

{-| Checks of the car editor's rules (`Racer.Editor`):

  - the palette has 8 different colours and the parts list stays short,
  - every part is one `Racer.Analyze` knows offline, and adding it to any
    starter base where the editor drops it keeps the car drivable on 4 wheels
    with no loose part (`cannotSitOnStuds` lists the one part that finds no
    free studs on a base and is reported loose),
  - a base with several parts added still analyses as a 4-wheel car and
    survives a build link round trip with its base found again,
  - the edits that would split the base are refused by `baseIntact`.

-}

import EmbeddedParts
import Expect
import Game.MapParams as Map
import LDraw.Catalog as Catalog
import LDraw.Connectivity as Connectivity
import LDraw.Document as Document exposing (Document)
import Racer.Analyze as Analyze
import Racer.Editor as Editor exposing (Base)
import Racer.Ingest as Ingest
import Racer.Types exposing (ValidationSeverity(..), VehicleSpec)
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Car editor"
        [ test "the palette has 8 different colours" <|
            \_ ->
                Expect.all
                    [ \codes -> Expect.equal 8 (List.length codes)
                    , \codes -> Expect.equal 8 (Set.size (Set.fromList codes))
                    ]
                    (List.map .code Editor.palette)
        , test "the default colour is in the palette" <|
            \_ -> Expect.equal True (List.member Editor.defaultColour (List.map .code Editor.palette))
        , test "the parts list is short and has no duplicates" <|
            \_ ->
                let
                    files =
                        List.map .file Editor.parts
                in
                Expect.all
                    [ \fs -> Expect.lessThan 100 (List.length fs)
                    , \fs -> Expect.equal (List.length fs) (Set.size (Set.fromList fs))
                    ]
                    files
        , test "every part has an offline connectivity entry" <|
            \_ ->
                Editor.parts
                    |> List.filter (\p -> Connectivity.knownCatalogPart p.file == Nothing)
                    |> List.map .file
                    |> Expect.equal []
        , test "the wheel parts are in the list and are the starter bases' rims and tyres" <|
            \_ ->
                let
                    used =
                        Editor.bases
                            |> List.concatMap (\b -> Document.flattenedPlacements b.document)
                            |> List.map .part
                            |> Set.fromList
                in
                Expect.all
                    [ \ws -> Expect.equal True (List.all (\w -> List.member w (List.map .file Editor.parts)) ws)
                    , \ws -> Expect.equal True (List.all (\w -> Set.member w used) ws)
                    ]
                    Editor.wheelParts
        , test "there are five bases, each found again in its own document" <|
            \_ ->
                Expect.all
                    [ \bs -> Expect.equal 5 (List.length bs)
                    , \bs -> Expect.equal (List.map .id bs) (List.filterMap (\b -> Editor.baseOf b.document |> Maybe.map .id) bs)
                    ]
                    Editor.bases
        , test "the startup car is the pinewood starter base, and the editor recognises it" <|
            \_ ->
                Editor.starterPreset "pinewood-classic"
                    |> Maybe.andThen (\p -> Editor.baseOf (Document.parse p.ldrText))
                    |> Maybe.map .id
                    |> Expect.equal (Just "pinewood-classic")
        , describe "every part added to every base keeps the car drivable"
            (Editor.bases |> List.map partsOnBaseTest)
        , describe "a base with added parts" (List.map builtCarTest Editor.bases)
        , describe "the base cannot be split" (List.map guardTest Editor.bases)
        ]


partsOnBaseTest : Base -> Test
partsOnBaseTest base =
    test base.id <|
        \_ ->
            Editor.parts
                |> List.filter (\p -> not (List.member p.file Editor.wheelParts))
                |> List.filterMap
                    (\p ->
                        let
                            spec =
                                analyse (addPart p.file base.document)
                        in
                        if drives spec then
                            Nothing

                        else if List.member ( base.id, p.file ) cannotSitOnStuds && drivesWithOnlyAddedPartLoose base spec then
                            Nothing

                        else
                            Just (p.file ++ ": " ++ describeSpec spec)
                    )
                |> Expect.equal []


{-| Parts that find no free studs on a base: a 4 x 4 plate is wider than the
gap between the alpine crawler's off-road tyres, so the editor rests it on top
of the tyres, 4 LDU above the plate's studs. The stud-contact analysis then
rightly reports it, and only it, as loose.
-}
cannotSitOnStuds : List ( String, String )
cannotSitOnStuds =
    [ ( "alpine-crawler", "3031.dat" ) ]


{-| Drivable on 2, 4 or 6 wheels with no blocker, and the only loose part is the
one just added (the first id after the base document's).
-}
drivesWithOnlyAddedPartLoose : Base -> VehicleSpec -> Bool
drivesWithOnlyAddedPartLoose base spec =
    spec.isDrivable
        && List.member (List.length spec.wheels) [ 2, 4, 6 ]
        && spec.loosePlacementIds
        == [ base.document.nextId ]
        && List.isEmpty (List.filter (\i -> i.severity == Blocker) spec.issues)


builtCarTest : Base -> Test
builtCarTest base =
    let
        built =
            List.foldl addPart base.document [ "3001.dat", "3001.dat", "3039.dat", "3829c01.dat", "3023b.dat", "3070b.dat" ]

        roundTrip =
            Ingest.decodeRaceInput Nothing (Ingest.encodeRaceHash { biome = Map.Farmland, season = Map.Summer, weather = Map.Clear, seed = 7 } built)
                |> Result.map .document
    in
    describe base.id
        [ test "drives on 2, 4 or 6 wheels with no loose part" <|
            \_ ->
                let
                    spec =
                        analyse built
                in
                Expect.equal True (drives spec)
                    |> Expect.onFail (describeSpec spec)
        , test "adds the parts" <|
            \_ -> Expect.equal (base.partCount + 6) (List.length (Document.flattenedPlacements built))
        , test "survives a build link round trip with its base" <|
            \_ ->
                case roundTrip of
                    Ok doc ->
                        Expect.all
                            [ \d -> Expect.equal (Just base.id) (Editor.baseOf d |> Maybe.map .id)
                            , \d -> Expect.equal (List.length (Document.flattenedPlacements built)) (List.length (Document.flattenedPlacements d))
                            , \d -> Expect.equal True (drives (analyse d))
                            ]
                            doc

                    Err _ ->
                        Expect.fail "the link did not decode"
        ]


guardTest : Base -> Test
guardTest base =
    let
        doc =
            addPart "3001.dat" base.document

        baseRef =
            Document.mainPlacements doc |> List.filter (Editor.isBasePlacement base) |> List.head

        exploded =
            baseRef |> Maybe.map (\r -> Document.explodeSubmodel r.id doc)

        removed =
            baseRef |> Maybe.map (\r -> Document.removePlacement r.id doc)

        duplicated =
            baseRef |> Maybe.map (\r -> Document.addPlacement (Document.mainSubmodel doc).name { part = r.part, color = r.color, matrix = r.matrix } doc |> Tuple.first)
    in
    describe base.id
        [ test "a build on the base keeps it intact" <|
            \_ -> Expect.equal True (Editor.baseIntact base doc)
        , test "ungrouping the base breaks it" <|
            \_ -> Expect.equal (Just False) (Maybe.map (Editor.baseIntact base) exploded)
        , test "deleting the base breaks it" <|
            \_ -> Expect.equal (Just False) (Maybe.map (Editor.baseIntact base) removed)
        , test "a second copy of the base breaks it" <|
            \_ -> Expect.equal (Just False) (Maybe.map (Editor.baseIntact base) duplicated)
        ]


addPart : String -> Document -> Document
addPart file doc =
    Document.addPlacement (Document.mainSubmodel doc).name { part = file, color = 4, matrix = Editor.dropMatrix doc Set.empty file } doc
        |> Tuple.first


analyse : Document -> VehicleSpec
analyse doc =
    (Analyze.analyzeDocument "test" doc EmbeddedParts.cache Catalog.emptyPartIndex).spec


drives : VehicleSpec -> Bool
drives spec =
    spec.isDrivable
        && List.member (List.length spec.wheels) [ 2, 4, 6 ]
        && List.isEmpty spec.loosePlacementIds
        && List.isEmpty (List.filter (\i -> i.severity == Blocker) spec.issues)


describeSpec : VehicleSpec -> String
describeSpec spec =
    "wheels "
        ++ String.fromInt (List.length spec.wheels)
        ++ ", loose "
        ++ String.fromInt (List.length spec.loosePlacementIds)
        ++ ", issues "
        ++ Debug.toString (List.map .code spec.issues)
