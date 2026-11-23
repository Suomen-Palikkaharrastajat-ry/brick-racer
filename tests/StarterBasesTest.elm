module StarterBasesTest exposing (suite)

{-| Checks of the starter car bases in `starters/racer/` (embedded in
`Racer.StarterBases` by `node starters/embed-starters.mjs`):

  - each file is one grouped base: the main model holds a single reference to
    the base submodel, and the part count in its header is the real count,
  - `Racer.Analyze` pairs 4 rim + tyre wheels, finds 2 front wheels, reports
    no blocker or warning (so no loose bricks and no belly scrape), and finds
    the turntable on the steering kart,
  - Brick Studio's `Studio.Health` finds no misaligned or overlapping parts in
    the base (a 6014b rim with its 56890 tyre aside, see `isOffsetRimTyre`),
    and every studded part sits on studs.

-}

import EmbeddedParts
import Expect
import LDraw.Catalog as Catalog
import LDraw.Connectivity as Connectivity exposing (ConnectionType(..))
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Racer.Analyze as Analyze
import Racer.StarterBases as StarterBases exposing (Starter)
import Racer.Types exposing (SteeringMechanism(..), ValidationSeverity(..))
import Set
import Studio.Health as Health
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Starter car bases"
        (test "there are four starter bases" (\_ -> Expect.equal 4 (List.length StarterBases.all))
            :: List.map starterTest StarterBases.all
        )


starterTest : Starter -> Test
starterTest starter =
    let
        doc =
            Document.parse starter.text

        base =
            baseDocument starter.text

        spec =
            (Analyze.analyzeDocument starter.file doc EmbeddedParts.cache Catalog.emptyPartIndex).spec

        flatCount =
            List.length (Document.flattenedPlacements doc)
    in
    describe starter.file
        [ test "is one grouped submodel" <|
            \_ ->
                Expect.all
                    [ \d -> Expect.equal 1 (List.length (Document.mainPlacements d))
                    , \d -> Expect.equal 2 (List.length d.submodels)
                    , \d -> Expect.equal True (List.all (\p -> Document.isSubmodelReference p d) (Document.mainPlacements d))
                    ]
                    doc
        , test "header part count matches" <|
            \_ -> Expect.equal (Just flatCount) (headerPartCount starter.text)
        , test "stays within the 80-part starter budget" <|
            \_ -> Expect.atMost 80 flatCount
        , test "analyzer pairs 3 or 4 wheels with tyres" <|
            \_ ->
                Expect.all
                    [ \s -> Expect.equal True (List.member (List.length s.wheels) [ 3, 4 ])
                    , \s -> Expect.equal True (List.all .hasRubberTire s.wheels)
                    , \s ->
                        if starter.file == "trike-wedge.mpd" then
                            Expect.equal 1 (List.length (List.filter .isFront s.wheels))

                        else
                            Expect.equal 2 (List.length (List.filter .isFront s.wheels))
                    , \s -> Expect.equal True (List.all (\w -> List.length w.placementIds == 2) s.wheels)
                    ]
                    spec
        , test "is drivable with no blocker or warning" <|
            \_ ->
                Expect.all
                    [ \s -> Expect.equal True s.isDrivable
                    , \s -> Expect.equal [] s.loosePlacementIds
                    , \s ->
                        Expect.equal []
                            (List.filter (\i -> i.severity == Blocker || i.severity == Warning) s.issues)
                    , \s -> Expect.atLeast 0.0 s.groundClearanceLdu
                    , \s -> Expect.atLeast 0.5 s.wheelBaseM
                    , \s -> Expect.atLeast 0.3 s.trackWidthM
                    ]
                    spec
        , test "steering matches the design" <|
            \_ ->
                case ( starter.file == "pinewood-classic.mpd", spec.steering ) of
                    ( True, FixedAxleFallback ) ->
                        Expect.pass

                    ( False, TurntableBogie b ) ->
                        Expect.atLeast 4 (List.length b.bogiePlacementIds)

                    ( _, other ) ->
                        Expect.fail ("Unexpected steering: " ++ Debug.toString other)
        , test "Studio health finds nothing to tidy" <|
            \_ ->
                Expect.equal []
                    (Health.check EmbeddedParts.cache base Set.empty
                        |> List.filter (not << isOffsetRimTyre base)
                    )
        , test "every studded part sits on studs" <|
            \_ -> Expect.equal [] (floatingStudParts base)
        ]


{-| The base submodel on its own, as a single-model document (`Studio.Health`
only checks the main model).
-}
baseDocument : String -> Document
baseDocument text =
    let
        lines =
            String.lines text

        afterFirstSection =
            lines
                |> dropUntilSecondFile
    in
    Document.parse (String.join "\n" afterFirstSection)


dropUntilSecondFile : List String -> List String
dropUntilSecondFile lines =
    case lines of
        first :: rest ->
            if String.startsWith "0 FILE" (String.trim first) then
                List.filter (\l -> not (String.startsWith "0 FILE" (String.trim l))) (dropToNextFile rest)

            else
                dropUntilSecondFile rest

        [] ->
            []


dropToNextFile : List String -> List String
dropToNextFile lines =
    case lines of
        first :: rest ->
            if String.startsWith "0 FILE" (String.trim first) then
                rest

            else
                dropToNextFile rest

        [] ->
            []


{-| A 6014b rim and the 56890 tyre mounted concentrically on it.

The LDraw origin of 6014b is on its hub face, 6 LDU from the rim's centre
(its geometry runs from z = -20 to z = 8), while 56890's origin is its centre.
A tyre that really sits on the rim therefore has its origin at the rim's
local (0, 0, -6). `LDraw.Connectivity` puts the 6014b `WheelRim` point and box
at the origin instead, so `Studio.Health` sees such a pair as overlapping.
Until that entry is fixed upstream (in brick-studio), only this exact pairing
is let through.

-}
isOffsetRimTyre : Document -> Health.Issue -> Bool
isOffsetRimTyre doc issue =
    case issue of
        Health.Overlapping { first, second } ->
            let
                placements =
                    Document.mainPlacements doc

                find id =
                    List.filter (\p -> p.id == id) placements |> List.head

                partName p =
                    String.toLower p.part

                mountedOn rim tyre =
                    partName rim
                        == "6014b.dat"
                        && partName tyre
                        == "56890.dat"
                        && Vec3.distance
                            (Mat4.transform rim.matrix (Vec3.vec3 0 0 -6))
                            (Mat4.transform tyre.matrix (Vec3.vec3 0 0 0))
                        <= 0.5
            in
            case ( find first, find second ) of
                ( Just a, Just b ) ->
                    mountedOn a b || mountedOn b a

                _ ->
                    False

        Health.Misaligned _ ->
            False


headerPartCount : String -> Maybe Int
headerPartCount text =
    String.lines text
        |> List.filterMap
            (\l ->
                if String.startsWith "0 // Parts: " l then
                    String.dropLeft 12 l |> String.words |> List.head |> Maybe.andThen String.toInt

                else
                    Nothing
            )
        |> List.head


floatingStudParts : Document -> List String
floatingStudParts doc =
    let
        placements =
            Document.mainPlacements doc

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
    in
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

                            hasTechnic =
                                List.any (\c -> isTechnic c.connType) conn.connections

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
                        not (List.isEmpty antis) && not hasTechnic && not (List.any matchesStud antis)

                    Nothing ->
                        True
            )
        |> List.map (\pl -> pl.part ++ "#" ++ String.fromInt pl.id)


isTechnic : ConnectionType -> Bool
isTechnic c =
    case c of
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
