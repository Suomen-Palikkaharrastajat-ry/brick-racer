module Racer.Editor exposing
    ( Base
    , PartEntry
    , Swatch
    , baseIntact
    , baseOf
    , bases
    , defaultColour
    , dropMatrix
    , isBasePlacement
    , palette
    , parts
    , presets
    , wheelParts
    )

{-| The rules of the car editor (`Pages.Build`): its 8 colours, its fixed
parts list, the starter bases a car is built on, where a new part goes, and the
check that keeps the base in one piece.

A car starts from one of the starter bases in `starters/racer/`
(`Racer.StarterBases`). Each is an `.mpd` whose main model holds a single
reference to the base submodel (the chassis, the wheels and their holders), so
`Studio.Core` selects and moves the whole base as one piece. The editor never
ungroups it: `baseIntact` is checked after every edit and an edit that breaks
the base (ungroup, delete, cut, duplicate, regroup) is dropped. The game
flattens the submodel when it analyses the car (`Racer.Analyze`), so the build
is analysed, shared and raced like any other loaded model.

-}

import Dict exposing (Dict)
import LDraw.Connectivity as Connectivity exposing (BoundingBox3D, ConnectionType(..))
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Ingest as Ingest
import Racer.StarterBases as StarterBases
import Racer.Types exposing (PresetBuild)
import Set exposing (Set)



-- ── Colours ──────────────────────────────────────────────────────────────────


{-| A palette colour: its LDraw code and its English and Finnish names.
-}
type alias Swatch =
    { code : Int
    , nameEn : String
    , nameFi : String
    }


{-| The 8 colours of the car editor. Bright colours that read on a car and
stand out from the green fields, the brown and grey roads and the sky; no
green, brown or grey. The bases bring their own dark grey chassis and black
tyres.
-}
palette : List Swatch
palette =
    [ { code = 4, nameEn = "Red", nameFi = "Punainen" }
    , { code = 25, nameEn = "Orange", nameFi = "Oranssi" }
    , { code = 14, nameEn = "Yellow", nameFi = "Keltainen" }
    , { code = 1, nameEn = "Blue", nameFi = "Sininen" }
    , { code = 322, nameEn = "Medium Azure", nameFi = "Taivaansininen" }
    , { code = 26, nameEn = "Magenta", nameFi = "Magenta" }
    , { code = 15, nameEn = "White", nameFi = "Valkoinen" }
    , { code = 0, nameEn = "Black", nameFi = "Musta" }
    ]


{-| The colour new parts get until the player picks another.
-}
defaultColour : Int
defaultColour =
    4



-- ── Parts ────────────────────────────────────────────────────────────────────


{-| A part of the editor's dock: its file and its English and Finnish names.
-}
type alias PartEntry =
    { file : String
    , nameEn : String
    , nameFi : String
    }


{-| The fixed parts list of the car editor, in dock order: bricks, plates,
slopes, tiles, then the car parts. Every part has a hand-made
`LDraw.Connectivity.knownCatalogPart` entry, so `Racer.Analyze` can weigh it
and join it to the car by its studs even before its geometry has loaded
(`tests/EditorTest.elm` checks that a base with any of them added still
drives). The rims and tyres are the three pairs the starter bases use, which
the analyzer pairs into wheels.
-}
parts : List PartEntry
parts =
    [ -- Bricks
      part "3005.dat" "Brick 1 x 1" "Palikka 1 x 1"
    , part "3004.dat" "Brick 1 x 2" "Palikka 1 x 2"
    , part "3622.dat" "Brick 1 x 3" "Palikka 1 x 3"
    , part "3010.dat" "Brick 1 x 4" "Palikka 1 x 4"
    , part "3009.dat" "Brick 1 x 6" "Palikka 1 x 6"
    , part "3003.dat" "Brick 2 x 2" "Palikka 2 x 2"
    , part "3002.dat" "Brick 2 x 3" "Palikka 2 x 3"
    , part "3001.dat" "Brick 2 x 4" "Palikka 2 x 4"

    -- Plates
    , part "3024.dat" "Plate 1 x 1" "Levy 1 x 1"
    , part "3023b.dat" "Plate 1 x 2" "Levy 1 x 2"
    , part "3623.dat" "Plate 1 x 3" "Levy 1 x 3"
    , part "3710.dat" "Plate 1 x 4" "Levy 1 x 4"
    , part "3666.dat" "Plate 1 x 6" "Levy 1 x 6"
    , part "3022.dat" "Plate 2 x 2" "Levy 2 x 2"
    , part "3021.dat" "Plate 2 x 3" "Levy 2 x 3"
    , part "3020.dat" "Plate 2 x 4" "Levy 2 x 4"
    , part "3795.dat" "Plate 2 x 6" "Levy 2 x 6"
    , part "3034.dat" "Plate 2 x 8" "Levy 2 x 8"
    , part "3031.dat" "Plate 4 x 4" "Levy 4 x 4"

    -- Slopes and curves
    , part "3040b.dat" "Slope 45° 2 x 1" "Viiste 45° 2 x 1"
    , part "3039.dat" "Slope 45° 2 x 2" "Viiste 45° 2 x 2"
    , part "3298.dat" "Slope 33° 3 x 2" "Viiste 33° 3 x 2"
    , part "54200.dat" "Slope 1 x 1 (cheese)" "Viiste 1 x 1"
    , part "85984.dat" "Slope 1 x 2 (cheese)" "Viiste 1 x 2"
    , part "11477.dat" "Curved slope 2 x 1" "Kaari 2 x 1"
    , part "15068.dat" "Curved slope 2 x 2" "Kaari 2 x 2"
    , part "50950.dat" "Curved slope 3 x 1" "Kaari 3 x 1"
    , part "3665b.dat" "Inverted slope 2 x 1" "Käänteinen viiste 2 x 1"

    -- Tiles
    , part "3070b.dat" "Tile 1 x 1" "Laatta 1 x 1"
    , part "3069b.dat" "Tile 1 x 2" "Laatta 1 x 2"
    , part "2431.dat" "Tile 1 x 4" "Laatta 1 x 4"
    , part "3068b.dat" "Tile 2 x 2" "Laatta 2 x 2"

    -- Car parts
    , part "3829c01.dat" "Steering wheel" "Ohjauspyörä"
    , part "4079.dat" "Seat 2 x 2" "Istuin 2 x 2"
    , part "3823.dat" "Windscreen 2 x 4 x 2" "Tuulilasi 2 x 4 x 2"
    , part "50745.dat" "Mudguard 4 x 2.5" "Lokasuoja 4 x 2,5"
    , part "4070.dat" "Headlight brick 1 x 1" "Valopalikka 1 x 1"
    , part "2412b.dat" "Grille tile 1 x 2" "Säleikkö 1 x 2"
    , part "3700.dat" "Technic brick 1 x 2" "Tekniikkapalikka 1 x 2"
    , part "3679.dat" "Turntable top 2 x 2" "Kääntöpöydän yläosa 2 x 2"
    , part "3680.dat" "Turntable base 2 x 2" "Kääntöpöydän alaosa 2 x 2"
    , part "4600.dat" "Wheel holder 2 x 2" "Pyöränpidike 2 x 2"
    , part "4488.dat" "Wheel holder 2 x 2, 1 pin" "Pyöränpidike 2 x 2, 1 tappi"
    , part "4624.dat" "Small rim" "Pieni vanne"
    , part "3641.dat" "Small tyre" "Pieni rengas"
    , part "6014b.dat" "Balloon rim" "Ilmakumivanne"
    , part "56890.dat" "Balloon tyre" "Ilmakumirengas"
    , part "55981.dat" "Off-road rim" "Maastovanne"
    , part "56891.dat" "Off-road tyre" "Maastorengas"
    ]


part : String -> String -> String -> PartEntry
part file nameEn nameFi =
    { file = file, nameEn = nameEn, nameFi = nameFi }


{-| The rims and tyres of the parts list: the three rim + tyre pairs of the
starter bases.
-}
wheelParts : List String
wheelParts =
    [ "4624.dat", "3641.dat", "6014b.dat", "56890.dat", "55981.dat", "56891.dat" ]



-- ── Bases ────────────────────────────────────────────────────────────────────


{-| A starter base: its file stem (`turntable-kart`), names, the base submodel the
main model references (`turntable-kart-base.ldr`), how many parts that submodel
holds, and the whole starter document.
-}
type alias Base =
    { id : String
    , nameEn : String
    , nameFi : String
    , submodel : String
    , partCount : Int
    , document : Document
    }


{-| The four starter bases, in menu order.
-}
bases : List Base
bases =
    StarterBases.all
        |> List.filterMap (\s -> fromStarter s.file s.text)
        |> List.sortBy (\b -> Maybe.withDefault 99 (Dict.get b.id baseOrder))


baseOrder : Dict String Int
baseOrder =
    Dict.fromList
        [ ( "turntable-kart", 0 )
        , ( "pinewood-classic", 1 )
        , ( "alpine-crawler", 2 )
        , ( "trike-wedge", 3 )
        ]


finnishNames : Dict String String
finnishNames =
    Dict.fromList
        [ ( "turntable-kart", "Kääntöpöytä-karting" )
        , ( "pinewood-classic", "Klassinen mäkiauto" )
        , ( "alpine-crawler", "Alppimaasturi" )
        , ( "trike-wedge", "Virtaviivainen kolmipyörä" )
        ]


fromStarter : String -> String -> Maybe Base
fromStarter file text =
    let
        doc =
            Document.parse text

        id =
            String.replace ".mpd" "" file
    in
    case Document.mainPlacements doc of
        [ ref ] ->
            let
                nameEn =
                    String.lines text
                        |> List.filterMap
                            (\l ->
                                if String.startsWith "0 Name:" l then
                                    Just (String.trim (String.dropLeft 7 l) |> String.replace " Base" "")

                                else
                                    Nothing
                            )
                        |> List.head
                        |> Maybe.withDefault id
            in
            Just
                { id = id
                , nameEn = nameEn
                , nameFi = Maybe.withDefault nameEn (Dict.get id finnishNames)
                , submodel = ref.part
                , partCount = List.length (Document.placementsOf ref.part doc)
                , document = doc
                }

        _ ->
            Nothing


{-| The starter base a build stands on: its main model holds exactly one
reference to a starter's base submodel, and that submodel still holds all of
the starter's parts. A build made in the editor (and shared by link) has one;
other loaded models do not, and are not opened in the editor.
-}
baseOf : Document -> Maybe Base
baseOf doc =
    bases
        |> List.filter (\b -> baseIntact b doc)
        |> List.head


{-| Whether the build still holds the base in one piece: one reference to the
base submodel in the main model, and the submodel with all of its parts.
-}
baseIntact : Base -> Document -> Bool
baseIntact base doc =
    let
        refs =
            Document.mainPlacements doc
                |> List.filter (isBasePlacement base)
    in
    List.length refs == 1 && List.length (Document.placementsOf base.submodel doc) == base.partCount


{-| Whether a placement is the reference to the base submodel.
-}
isBasePlacement : Base -> Placement -> Bool
isBasePlacement base placement =
    String.toLower (String.trim placement.part) == String.toLower base.submodel



-- ── Where a new part goes ────────────────────────────────────────────────────


{-| A part in world space: its file and its world matrix.
-}
type alias WorldPart =
    { part : String
    , matrix : Mat4
    }


{-| The placement of a new `part`: on free studs of the build, as close as
possible to the selected parts (or to the middle of the build when nothing,
or only the base, is selected), with all of its anti-studs on studs and
without running into any part. Placed like this it is joined to the car at
once, so `Racer.Analyze` does not count it as loose.

A part with no anti-studs (a rim, a tyre) goes on top of the selection, or of
the middle of the build. The part is never rotated: the player turns it after.

-}
dropMatrix : Document -> Set Int -> String -> Mat4
dropMatrix doc selection partFile =
    let
        everything =
            Document.mainPlacements doc |> List.concatMap (worldParts doc)

        selected =
            Document.mainPlacements doc
                |> List.filter (\p -> Set.member p.id selection && not (Document.isSubmodelReference p doc))
                |> List.concatMap (worldParts doc)

        targetBox =
            unionBoxes
                (List.map worldBox
                    (if List.isEmpty selected then
                        everything

                     else
                        selected
                    )
                )

        target =
            boxCentre targetBox

        newPart =
            localInfo partFile
    in
    if List.isEmpty newPart.antiStuds then
        restOn
            (if List.isEmpty selected then
                everything

             else
                selected
            )
            newPart
            target

    else
        case studSpot everything newPart target of
            Just translation ->
                Mat4.makeTranslate translation

            Nothing ->
                restOn everything newPart target


{-| The part above `target` on the stud grid, resting on the highest of the
`below` parts under it (LDraw, -Y up).
-}
restOn : List WorldPart -> LocalInfo -> Vec3 -> Mat4
restOn below info target =
    let
        x =
            snap20 (Vec3.getX target)

        z =
            snap20 (Vec3.getZ target)

        footprint =
            shiftBox (vec3 x 0 z) info.box

        belowBoxes =
            List.filterMap worldBox below

        under =
            List.filter (\b -> overlapsAcross footprint b) belowBoxes

        topY =
            (if List.isEmpty under then
                belowBoxes

             else
                under
            )
                |> List.map (.min >> Vec3.getY)
                |> List.minimum
                |> Maybe.withDefault 0
    in
    Mat4.makeTranslate3 x (topY + studHeight - Vec3.getY info.box.max) z


snap20 : Float -> Float
snap20 v =
    toFloat (round (v / 20)) * 20


studHeight : Float
studHeight =
    4


{-| The best free spot for a part with anti-studs: try putting each of its
anti-studs on each free upward stud of the build, drop the spots where it runs
into a part, and keep the one with the most anti-studs on free studs, then the
one nearest the target, then the highest.
-}
studSpot : List WorldPart -> LocalInfo -> Vec3 -> Maybe Vec3
studSpot everything info target =
    let
        covered =
            everything
                |> List.concatMap (worldPoints AntiStud)
                |> List.map (.position >> key)
                |> Set.fromList

        free =
            everything
                |> List.concatMap (worldPoints Stud)
                |> List.filter (\s -> Vec3.getY s.normal < -0.7 && not (Set.member (key s.position) covered))

        freeKeys =
            free |> List.map (.position >> key) |> Set.fromList

        boxes =
            List.map worldBox everything |> List.filterMap identity

        candidates =
            free
                |> List.concatMap (\s -> List.map (\a -> Vec3.sub s.position a) info.antiStuds)
                |> List.map (\t -> ( key t, t ))
                |> Dict.fromList
                |> Dict.values

        matched translation =
            List.length (List.filter (\a -> Set.member (key (Vec3.add a translation)) freeKeys) info.antiStuds)

        clear translation =
            not (List.any (overlaps (shiftBox translation info.box)) boxes)
    in
    candidates
        |> List.filter clear
        |> List.map (\t -> ( ( -(matched t), round (horizontalDistance t target), Vec3.getY t ), t ))
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.second


horizontalDistance : Vec3 -> Vec3 -> Float
horizontalDistance a b =
    sqrt ((Vec3.getX a - Vec3.getX b) ^ 2 + (Vec3.getZ a - Vec3.getZ b) ^ 2)


key : Vec3 -> ( Int, Int, Int )
key v =
    ( round (Vec3.getX v), round (Vec3.getY v), round (Vec3.getZ v) )


{-| Every leaf part of a main placement in world space (a submodel reference is
expanded, nested ones too).
-}
worldParts : Document -> Placement -> List WorldPart
worldParts doc placement =
    worldPartsHelp doc 0 Mat4.identity placement


worldPartsHelp : Document -> Int -> Mat4 -> Placement -> List WorldPart
worldPartsHelp doc depth parent placement =
    let
        matrix =
            Mat4.mul parent placement.matrix
    in
    if depth < 8 && Document.isSubmodelReference placement doc then
        Document.placementsOf placement.part doc
            |> List.concatMap (worldPartsHelp doc (depth + 1) matrix)

    else
        [ { part = placement.part, matrix = matrix } ]


{-| What the editor knows of a part from its connectivity entry: its anti-studs
and its box, in part space.
-}
type alias LocalInfo =
    { antiStuds : List Vec3
    , box : BoundingBox3D
    }


localInfo : String -> LocalInfo
localInfo partFile =
    case Connectivity.knownCatalogPart partFile of
        Just conn ->
            { antiStuds =
                conn.connections
                    |> List.filter (\c -> c.connType == AntiStud && Vec3.getY c.normal > 0.7)
                    |> List.map .localPosition
            , box =
                unionBoxes (List.map Just conn.boundingBoxes)
                    |> Maybe.withDefault defaultBox
            }

        Nothing ->
            { antiStuds = [], box = defaultBox }


defaultBox : BoundingBox3D
defaultBox =
    { min = vec3 -10 -4 -10, max = vec3 10 24 10 }


worldPoints : ConnectionType -> WorldPart -> List { position : Vec3, normal : Vec3 }
worldPoints kind wp =
    case Connectivity.knownCatalogPart wp.part of
        Just conn ->
            conn.connections
                |> List.filter (\c -> c.connType == kind)
                |> List.map
                    (\c ->
                        let
                            position =
                                Mat4.transform wp.matrix c.localPosition
                        in
                        { position = position
                        , normal = Vec3.sub (Mat4.transform wp.matrix (Vec3.add c.localPosition c.normal)) position
                        }
                    )

        Nothing ->
            []


worldBox : WorldPart -> Maybe BoundingBox3D
worldBox wp =
    let
        box =
            (localInfo wp.part).box

        corners =
            [ vec3 (Vec3.getX box.min) (Vec3.getY box.min) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.min) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.min) (Vec3.getY box.max) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.min) (Vec3.getY box.min) (Vec3.getZ box.max)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.max) (Vec3.getZ box.min)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.min) (Vec3.getZ box.max)
            , vec3 (Vec3.getX box.min) (Vec3.getY box.max) (Vec3.getZ box.max)
            , vec3 (Vec3.getX box.max) (Vec3.getY box.max) (Vec3.getZ box.max)
            ]
                |> List.map (Mat4.transform wp.matrix)
    in
    boundsOf corners


boundsOf : List Vec3 -> Maybe BoundingBox3D
boundsOf points =
    case points of
        [] ->
            Nothing

        first :: rest ->
            Just
                (List.foldl
                    (\p acc ->
                        { min = vec3 (min (Vec3.getX p) (Vec3.getX acc.min)) (min (Vec3.getY p) (Vec3.getY acc.min)) (min (Vec3.getZ p) (Vec3.getZ acc.min))
                        , max = vec3 (max (Vec3.getX p) (Vec3.getX acc.max)) (max (Vec3.getY p) (Vec3.getY acc.max)) (max (Vec3.getZ p) (Vec3.getZ acc.max))
                        }
                    )
                    { min = first, max = first }
                    rest
                )


unionBoxes : List (Maybe BoundingBox3D) -> Maybe BoundingBox3D
unionBoxes boxes =
    boxes
        |> List.filterMap identity
        |> List.concatMap (\b -> [ b.min, b.max ])
        |> boundsOf


boxCentre : Maybe BoundingBox3D -> Vec3
boxCentre box =
    case box of
        Just b ->
            Vec3.scale 0.5 (Vec3.add b.min b.max)

        Nothing ->
            vec3 0 0 0


shiftBox : Vec3 -> BoundingBox3D -> BoundingBox3D
shiftBox t b =
    { min = Vec3.add b.min t, max = Vec3.add b.max t }


{-| Whether two boxes overlap seen from above (in X and Z).
-}
overlapsAcross : BoundingBox3D -> BoundingBox3D -> Bool
overlapsAcross a b =
    let
        apart getter =
            getter a.max - 2 <= getter b.min || getter b.max - 2 <= getter a.min
    in
    not (apart Vec3.getX || apart Vec3.getZ)


{-| Whether two boxes overlap by more than the studs and a little slack: a
part sitting on the studs of another touches it but does not overlap it.
-}
overlaps : BoundingBox3D -> BoundingBox3D -> Bool
overlaps a b =
    let
        apart getter slack =
            getter a.max - slack <= getter b.min || getter b.max - slack <= getter a.min
    in
    not (apart Vec3.getX 2 || apart Vec3.getY (studHeight + 1) || apart Vec3.getZ 2)


{-| Built-in starter soapbox presets ready to inspect and race.
-}
presets : List PresetBuild
presets =
    Ingest.presets
