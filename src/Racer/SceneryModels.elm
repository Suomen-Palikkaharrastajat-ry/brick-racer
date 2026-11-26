module Racer.SceneryModels exposing
    ( all
    , barn
    , barrels
    , brickGate
    , brickPallet
    , brickWall
    , chalet
    , cottage
    , countdownBoard
    , crate
    , crateStack
    , fence
    , gnome
    , pitGarage
    , slopeBlock
    , tileStack
    )

{-| The racer's own crashable roadside props, as LDraw models of catalog
parts, in the same conventions as `Game.BrickModels` (-Y up, 1 stud = 20 LDU,
1 brick = 24 LDU, a part's origin at the top of its body, the ground at
y = 0, centred on x = z = 0, the front towards LDraw -Z). They are drawn with
`Game.BrickProps` like the shared models.

Only the looks live here; where a prop stands and how big its collision
cylinder is stays in `Racer.Track`.

Every redesigned building, fence section and dock-built prop (`fence`,
`cottage`, `barn`, `chalet`, `pitGarage`, `brickWall`, `brickPallet`,
`tileStack`, `brickGate`, `slopeBlock`, `countdownBoard`) uses only catalog
parts from the car editor's dock (`Racer.Editor.parts`), which
`tests/SceneryModelsTest.elm` checks.

-}


{-| Every model with its name, for loading them all.
-}
all : List ( String, String )
all =
    [ ( "crate", crate )
    , ( "crateStack", crateStack )
    , ( "barrels", barrels )
    , ( "gnome", gnome )
    , ( "fence", fence )
    , ( "cottage", cottage )
    , ( "barn", barn )
    , ( "brickWall", brickWall )
    , ( "brickPallet", brickPallet )
    , ( "tileStack", tileStack )
    , ( "brickGate", brickGate )
    , ( "slopeBlock", slopeBlock )
    , ( "countdownBoard", countdownBoard )
    , ( "chalet", chalet )
    , ( "pitGarage", pitGarage )
    ]


type Turn
    = Straight
    | Quarter
    | Half
    | ThreeQuarter
    | LieFlat


part : Int -> ( Float, Float, Float ) -> Turn -> String -> String
part colour ( x, y, z ) turn file =
    let
        matrix =
            case turn of
                Straight ->
                    [ "1", "0", "0", "0", "1", "0", "0", "0", "1" ]

                Quarter ->
                    [ "0", "0", "1", "0", "1", "0", "-1", "0", "0" ]

                Half ->
                    [ "-1", "0", "0", "0", "1", "0", "0", "0", "-1" ]

                ThreeQuarter ->
                    [ "0", "0", "-1", "0", "1", "0", "1", "0", "0" ]

                LieFlat ->
                    [ "1", "0", "0", "0", "0", "-1", "0", "1", "0" ]
    in
    String.join " " ([ "1", String.fromInt colour, String.fromFloat x, String.fromFloat y, String.fromFloat z ] ++ matrix ++ [ file ])


model : String -> List String -> String
model name lines =
    String.join "\n" (("0 " ++ name) :: "0 Author: Brick Racer" :: "0 BFC CERTIFY CCW" :: lines)


layerY : Int -> Float
layerY k =
    -24 * toFloat (k + 1)


reddishBrown : Int
reddishBrown =
    70


darkTan : Int
darkTan =
    28


tan : Int
tan =
    19


blue : Int
blue =
    1


transLightBlue : Int
transLightBlue =
    41


red : Int
red =
    4


darkRed : Int
darkRed =
    320


green : Int
green =
    2


lightBluishGrey : Int
lightBluishGrey =
    71


darkBluishGrey : Int
darkBluishGrey =
    72


white : Int
white =
    15


yellow : Int
yellow =
    14


black : Int
black =
    0


{-| Subtract any `[oa, ob]` openings from a closed stud interval `[start, end]`,
returning sorted disjoint solid spans.
-}
carveOpenings : List ( Float, Float ) -> ( Float, Float ) -> List ( Float, Float )
carveOpenings openings span =
    let
        carveOne ( oa, ob ) segs =
            List.concatMap
                (\( sa, sb ) ->
                    if ob <= sa + 0.1 || oa >= sb - 0.1 then
                        [ ( sa, sb ) ]

                    else
                        let
                            left =
                                if oa > sa + 0.1 then
                                    [ ( sa, min sb oa ) ]

                                else
                                    []

                            right =
                                if ob < sb - 0.1 then
                                    [ ( max sa ob, sb ) ]

                                else
                                    []
                        in
                        left ++ right
                )
                segs
    in
    List.foldl carveOne [ span ] openings


{-| Tile an exact multiple-of-20 LDU span with 1-stud-thick catalog bricks
(`3009`, `3010`, `3622`, `3004`, `3005`), staggering long runs when `stagger` is True
for realistic running-bond masonry joints.
-}
tileChunks : Bool -> Float -> Float -> List ( Float, String )
tileChunks stagger start end =
    let
        totalStuds =
            round ((end - start) / 20.0)

        step ( pos, remStuds, acc ) =
            if remStuds <= 0 then
                List.reverse acc

            else
                let
                    ( chunk, file ) =
                        if remStuds >= 6 then
                            ( 6, "3009.dat" )

                        else if remStuds >= 4 then
                            ( 4, "3010.dat" )

                        else if remStuds >= 3 then
                            ( 3, "3622.dat" )

                        else if remStuds == 2 then
                            ( 2, "3004.dat" )

                        else
                            ( 1, "3005.dat" )

                    center =
                        pos + toFloat chunk * 10.0
                in
                step ( pos + toFloat chunk * 20.0, remStuds - chunk, ( center, file ) :: acc )
    in
    if totalStuds <= 0 then
        []

    else if stagger && totalStuds >= 4 then
        let
            firstCenter =
                start + 20.0
        in
        ( firstCenter, "3004.dat" ) :: step ( start + 40.0, totalStuds - 2, [] )

    else
        step ( start, totalStuds, [] )


tileBricksX : Int -> Float -> Float -> Bool -> List ( Float, Float ) -> ( Float, Float ) -> List String
tileBricksX colour y z stagger openings span =
    carveOpenings openings span
        |> List.concatMap
            (\( a, b ) ->
                tileChunks stagger a b
                    |> List.map (\( cx, file ) -> part colour ( cx, y, z ) Straight file)
            )


tileBricksZ : Int -> Float -> Float -> Bool -> List ( Float, Float ) -> ( Float, Float ) -> List String
tileBricksZ colour y x stagger openings span =
    carveOpenings openings span
        |> List.concatMap
            (\( a, b ) ->
                tileChunks stagger a b
                    |> List.map (\( cz, file ) -> part colour ( x, y, cz ) Quarter file)
            )


{-| One interlocking course of four 1-stud-thick perimeter walls at arbitrary
vertical coordinate `y` spanning outer box `[-xHalf, +xHalf]` x
`[-zHalf, +zHalf]`. When `stagger` is False, front/back carry full X corners;
when `stagger` is True, left/right carry full Z corners so consecutive courses
alternate cleanly without overlapping bricks.
-}
perimeterCourseAt :
    { xHalf : Float
    , zHalf : Float
    , y : Float
    , stagger : Bool
    , frontColour : Int
    , backColour : Int
    , sideColour : Int
    , frontOpenings : List ( Float, Float )
    , backOpenings : List ( Float, Float )
    , leftOpenings : List ( Float, Float )
    , rightOpenings : List ( Float, Float )
    }
    -> List String
perimeterCourseAt spec =
    let
        ( xSpan, zSpan ) =
            if not spec.stagger then
                ( ( -spec.xHalf, spec.xHalf ), ( -spec.zHalf + 20, spec.zHalf - 20 ) )

            else
                ( ( -spec.xHalf + 20, spec.xHalf - 20 ), ( -spec.zHalf, spec.zHalf ) )
    in
    tileBricksX spec.frontColour spec.y -(spec.zHalf - 10) spec.stagger spec.frontOpenings xSpan
        ++ tileBricksX spec.backColour spec.y (spec.zHalf - 10) spec.stagger spec.backOpenings xSpan
        ++ tileBricksZ spec.sideColour spec.y -(spec.xHalf - 10) spec.stagger spec.leftOpenings zSpan
        ++ tileBricksZ spec.sideColour spec.y (spec.xHalf - 10) spec.stagger spec.rightOpenings zSpan


{-| One interlocking perimeter wall course on discrete brick layer `layer`.
-}
perimeterCourse :
    { xHalf : Float
    , zHalf : Float
    , layer : Int
    , frontColour : Int
    , backColour : Int
    , sideColour : Int
    , frontOpenings : List ( Float, Float )
    , backOpenings : List ( Float, Float )
    , leftOpenings : List ( Float, Float )
    , rightOpenings : List ( Float, Float )
    }
    -> List String
perimeterCourse spec =
    perimeterCourseAt
        { xHalf = spec.xHalf
        , zHalf = spec.zHalf
        , y = layerY spec.layer
        , stagger = modBy 2 spec.layer == 1
        , frontColour = spec.frontColour
        , backColour = spec.backColour
        , sideColour = spec.sideColour
        , frontOpenings = spec.frontOpenings
        , backOpenings = spec.backOpenings
        , leftOpenings = spec.leftOpenings
        , rightOpenings = spec.rightOpenings
        }



-- CRATES


{-| One crate standing on `y0` (LDU, negative is up): a reddish brown 2 x 2
brick framed top and bottom by dark tan 2 x 2 plates, 40 LDU each way.
-}
crateAt : ( Float, Float, Float ) -> List String
crateAt ( x, y0, z ) =
    [ part darkTan ( x, y0 - 8, z ) Straight "3022.dat"
    , part reddishBrown ( x, y0 - 32, z ) Straight "3003.dat"
    , part darkTan ( x, y0 - 40, z ) Straight "3022.dat"
    ]


{-| A single wooden crate.
-}
crate : String
crate =
    model "Crate" (crateAt ( 0, 0, 0 ))


{-| Two crates stacked with a third beside them.
-}
crateStack : String
crateStack =
    model "Crate stack"
        (crateAt ( -15, 0, 0 )
            ++ crateAt ( -15, -40, 0 )
            ++ crateAt ( 35, 0, 12 )
        )



-- BARRELS


{-| Two blue oil drums side by side: each two 2 x 2 round bricks with a grey
1 x 1 round plate for the bung.
-}
barrels : String
barrels =
    model "Barrels"
        (List.concatMap
            (\x ->
                [ part blue ( x, -24, 0 ) Straight "3941.dat"
                , part blue ( x, -48, 0 ) Straight "3941.dat"
                , part lightBluishGrey ( x + 10, -56, 0 ) Straight "6141.dat"
                ]
            )
            [ -22, 22 ]
        )



-- GNOME


{-| A garden gnome: black boots, blue trousers, a red coat, a white beard, a
light nougat face and a red cone hat.
-}
gnome : String
gnome =
    model "Garden gnome"
        [ part black ( 0, -8, 0 ) Straight "3022.dat"
        , part blue ( 0, -32, 0 ) Straight "3003.dat"
        , part red ( 0, -56, 0 ) Straight "3003.dat"
        , part white ( 0, -64, 0 ) Straight "3022.dat"
        , part 78 ( 0, -88, 0 ) Straight "3062b.dat"
        , part red ( 0, -112, 0 ) Straight "4589.dat"
        ]



-- FENCE


{-| A continuous 3.0 m (480 LDU = 24 studs) post-and-rail roadside fence section
along Z: three reddish-brown posts spaced every 8 studs (-170, -10, +150, so
consecutive 3.0 m sections keep an exact 8-stud rhythm across seams with no
double posts) capped with dark tan tiles, locked to two continuous white 1 x 6
brick horizontal rails spanning the full -240 to +240 LDU envelope so consecutive
3.0 m track segments meet flush with zero gaps.
-}
fence : String
fence =
    let
        postZs =
            [ -170, -10, 150 ]

        railZs =
            [ -180, -60, 60, 180 ]

        posts =
            List.concatMap
                (\z ->
                    List.map (\k -> part reddishBrown ( 10, layerY k, z ) Straight "3005.dat") [ 0, 1, 2, 3, 4 ]
                        ++ [ part darkTan ( 0, -128, z ) Straight "3023b.dat"
                           , part reddishBrown ( 10, -152, z ) Straight "3005.dat"
                           , part darkTan ( 10, -160, z ) Straight "3070b.dat"
                           ]
                )
                postZs

        rails =
            List.concatMap
                (\z ->
                    [ part white ( -10, -72, z ) Quarter "3009.dat"
                    , part white ( -10, -120, z ) Quarter "3009.dat"
                    ]
                )
                railZs
    in
    model "Fence" (posts ++ rails)



-- COTTAGE


{-| A full-scale country cottage (24 x 20 studs footprint, 26 x 22 studs with
eaves, 3.65 m ridge and 4.05 m stone chimney): dark grey foundation, white
running-bond walls, recessed wooden front door with doorstep, shuttered
windows, 45° bright red roof slopes with filled gables and a stone chimney.
-}
cottage : String
cottage =
    let
        xHalf =
            240

        zHalf =
            200

        doorStep =
            [ part lightBluishGrey ( 0, -8, -220 ) Straight "3020.dat"
            , part lightBluishGrey ( 0, -16, -210 ) Straight "3023b.dat"
            ]

        foundation =
            perimeterCourse
                { xHalf = xHalf
                , zHalf = zHalf
                , layer = 0
                , frontColour = darkBluishGrey
                , backColour = darkBluishGrey
                , sideColour = darkBluishGrey
                , frontOpenings = []
                , backOpenings = []
                , leftOpenings = []
                , rightOpenings = []
                }
                ++ perimeterCourse
                    { xHalf = xHalf
                    , zHalf = zHalf
                    , layer = 1
                    , frontColour = darkBluishGrey
                    , backColour = darkBluishGrey
                    , sideColour = darkBluishGrey
                    , frontOpenings = [ ( -40, 40 ) ]
                    , backOpenings = []
                    , leftOpenings = []
                    , rightOpenings = []
                    }

        walls =
            List.concatMap
                (\k ->
                    let
                        winOpenings =
                            if k >= 4 && k <= 7 then
                                [ ( -200, -80 ), ( 80, 200 ) ]

                            else
                                []

                        sideWinOpenings =
                            if k >= 4 && k <= 7 then
                                [ ( -140, -20 ), ( 20, 140 ) ]

                            else
                                []

                        doorOpenings =
                            if k <= 9 then
                                ( -40, 40 ) :: winOpenings

                            else
                                winOpenings
                    in
                    perimeterCourse
                        { xHalf = xHalf
                        , zHalf = zHalf
                        , layer = k
                        , frontColour = white
                        , backColour = white
                        , sideColour = white
                        , frontOpenings = doorOpenings
                        , backOpenings = winOpenings
                        , leftOpenings = sideWinOpenings
                        , rightOpenings = sideWinOpenings
                        }
                )
                (List.range 2 11)

        recessedDoor =
            List.concatMap
                (\k ->
                    if k == 4 then
                        [ part reddishBrown ( -20, layerY k, -170 ) Straight "3004.dat"
                        , part reddishBrown ( 10, layerY k, -170 ) Straight "3005.dat"
                        , part yellow ( 30, layerY k, -170 ) Straight "3005.dat"
                        ]

                    else
                        [ part reddishBrown ( 0, layerY k, -170 ) Straight "3010.dat" ]
                )
                (List.range 1 9)

        recessedWindows =
            let
                paneRow y z =
                    [ part transLightBlue ( -140, y, z ) Straight "3010.dat"
                    , part transLightBlue ( 140, y, z ) Straight "3010.dat"
                    ]

                sidePaneRow y x =
                    [ part transLightBlue ( x, y, -80 ) Quarter "3010.dat"
                    , part transLightBlue ( x, y, 80 ) Quarter "3010.dat"
                    ]

                shutters y =
                    List.concatMap
                        (\sx ->
                            [ part darkRed ( sx, y, -190 ) Straight "3005.dat"
                            , part darkRed ( sx, y, 190 ) Straight "3005.dat"
                            ]
                        )
                        [ -190, -90, 90, 190 ]
                        ++ List.concatMap
                            (\sz ->
                                [ part darkRed ( -230, y, sz ) Straight "3005.dat"
                                , part darkRed ( 230, y, sz ) Straight "3005.dat"
                                ]
                            )
                            [ -130, -30, 30, 130 ]

                sills =
                    List.concatMap
                        (\wx ->
                            [ part darkBluishGrey ( wx, layerY 3 - 8, -190 ) Straight "3710.dat"
                            , part darkBluishGrey ( wx, layerY 3 - 8, 190 ) Straight "3710.dat"
                            ]
                        )
                        [ -140, 140 ]
                        ++ List.concatMap
                            (\wz ->
                                [ part darkBluishGrey ( -230, layerY 3 - 8, wz ) Quarter "3710.dat"
                                , part darkBluishGrey ( 230, layerY 3 - 8, wz ) Quarter "3710.dat"
                                ]
                            )
                            [ -80, 80 ]
            in
            List.concatMap
                (\k ->
                    let
                        y =
                            layerY k
                    in
                    paneRow y -170 ++ paneRow y 170 ++ sidePaneRow y -210 ++ sidePaneRow y 210 ++ shutters y
                )
                [ 4, 5, 6, 7 ]
                ++ sills

        roofZs =
            List.map (\i -> -200 + 40 * toFloat i) (List.range 0 10)

        roof =
            List.concatMap
                (\r ->
                    let
                        y =
                            layerY (12 + r)

                        xc =
                            230 - 20 * toFloat r
                    in
                    List.concatMap
                        (\z ->
                            if r == 0 && z == 0 then
                                [ part red ( -xc, y, z ) Quarter "3039.dat"
                                , part red ( 230, y, 0 ) Quarter "3004.dat"
                                ]

                            else
                                [ part red ( -xc, y, z ) Quarter "3039.dat"
                                , part red ( xc, y, z ) ThreeQuarter "3039.dat"
                                ]
                        )
                        roofZs
                )
                (List.range 0 11)
                ++ List.map (\z -> part darkRed ( 0, layerY 23 - 8, z ) Straight "3068b.dat") roofZs

        gables =
            List.concatMap
                (\r ->
                    let
                        y =
                            layerY (12 + r)

                        halfInner =
                            220 - 20 * toFloat r

                        atticOpening =
                            if r >= 3 && r <= 5 then
                                [ ( -20, 20 ) ]

                            else
                                []

                        atticGlass =
                            if r >= 3 && r <= 5 then
                                [ part transLightBlue ( 0, y, -170 ) Straight "3004.dat"
                                , part transLightBlue ( 0, y, 170 ) Straight "3004.dat"
                                ]

                            else
                                []
                    in
                    tileBricksX white y -190 (modBy 2 r == 1) atticOpening ( -halfInner, halfInner )
                        ++ tileBricksX white y 190 (modBy 2 r == 1) atticOpening ( -halfInner, halfInner )
                        ++ atticGlass
                )
                (List.range 0 10)

        chimney =
            let
                chimneyCol k =
                    if k <= 1 || k == 12 || k >= 24 then
                        darkBluishGrey

                    else
                        lightBluishGrey
            in
            List.map (\k -> part (chimneyCol k) ( 260, layerY k, 0 ) Straight "3003.dat") (List.range 0 25)
                ++ [ part lightBluishGrey ( 260, layerY 25 - 8, 0 ) Straight "3022.dat"
                   , part darkBluishGrey ( 260, layerY 25 - 16, 0 ) Straight "3022.dat"
                   ]
    in
    model "Cottage" (doorStep ++ foundation ++ walls ++ recessedDoor ++ recessedWindows ++ roof ++ gables ++ chimney)



-- BARN


{-| A substantial countryside gambrel barn (24 x 34 studs footprint, 26 x 36 studs
with eaves, 3.65 m tall): stone foundation, tall dark red walls with white trim,
wide double barn doors and hayloft doors at both ends, framed side windows, and a
two-pitch dark grey gambrel roof (45° lower + 33° upper slopes).
-}
barn : String
barn =
    let
        xHalf =
            240

        zHalf =
            340

        foundation =
            List.concatMap
                (\k ->
                    perimeterCourse
                        { xHalf = xHalf
                        , zHalf = zHalf
                        , layer = k
                        , frontColour = darkBluishGrey
                        , backColour = darkBluishGrey
                        , sideColour = darkBluishGrey
                        , frontOpenings = [ ( -80, 80 ) ]
                        , backOpenings = [ ( -80, 80 ) ]
                        , leftOpenings = []
                        , rightOpenings = []
                        }
                )
                [ 0, 1 ]
                ++ [ part darkBluishGrey ( 0, layerY 0, -330 ) Straight "3009.dat"
                   , part darkBluishGrey ( 0, layerY 0, 330 ) Straight "3009.dat"
                   , part darkBluishGrey ( -70, layerY 0, -330 ) Straight "3005.dat"
                   , part darkBluishGrey ( 70, layerY 0, -330 ) Straight "3005.dat"
                   , part darkBluishGrey ( -70, layerY 0, 330 ) Straight "3005.dat"
                   , part darkBluishGrey ( 70, layerY 0, 330 ) Straight "3005.dat"
                   ]

        walls =
            List.concatMap
                (\k ->
                    let
                        mainCol =
                            if k == 15 then
                                white

                            else
                                darkRed

                        cornerOpenX =
                            if k <= 14 && modBy 2 k == 0 then
                                [ ( -240, -220 ), ( 220, 240 ) ]

                            else
                                []

                        cornerOpenZ =
                            if k <= 14 && modBy 2 k == 1 then
                                [ ( -340, -320 ), ( 320, 340 ) ]

                            else
                                []

                        endDoorOpen =
                            if k <= 10 then
                                ( -80, 80 ) :: cornerOpenX

                            else
                                cornerOpenX

                        sideWinOpen =
                            if k >= 5 && k <= 10 then
                                [ ( -220, -140 ), ( -40, 40 ), ( 140, 220 ) ] ++ cornerOpenZ

                            else
                                cornerOpenZ
                    in
                    perimeterCourse
                        { xHalf = xHalf
                        , zHalf = zHalf
                        , layer = k
                        , frontColour = mainCol
                        , backColour = mainCol
                        , sideColour = mainCol
                        , frontOpenings = endDoorOpen
                        , backOpenings = endDoorOpen
                        , leftOpenings = sideWinOpen
                        , rightOpenings = sideWinOpen
                        }
                )
                (List.range 2 15)

        cornerPilasters =
            List.concatMap
                (\k ->
                    let
                        y =
                            layerY k
                    in
                    [ part white ( -230, y, -330 ) Straight "3005.dat"
                    , part white ( 230, y, -330 ) Straight "3005.dat"
                    , part white ( -230, y, 330 ) Straight "3005.dat"
                    , part white ( 230, y, 330 ) Straight "3005.dat"
                    ]
                )
                (List.range 2 14)

        barnDoors =
            List.concatMap
                (\k ->
                    let
                        y =
                            layerY k

                        doorLine z =
                            [ part reddishBrown ( -50, y, z ) Straight "3622.dat"
                            , part white ( 0, y, z ) Straight "3004.dat"
                            , part reddishBrown ( 50, y, z ) Straight "3622.dat"
                            ]
                    in
                    doorLine -310 ++ doorLine 310
                )
                (List.range 1 10)
                ++ List.concatMap
                    (\z ->
                        [ part white ( 0, layerY 11, z ) Straight "3009.dat"
                        , part white ( -80, layerY 11, z ) Straight "3004.dat"
                        , part white ( 80, layerY 11, z ) Straight "3004.dat"
                        ]
                    )
                    [ -350, 350 ]

        sideWindows =
            List.concatMap
                (\k ->
                    let
                        y =
                            layerY k
                    in
                    List.concatMap
                        (\wz ->
                            [ part transLightBlue ( -210, y, wz ) Quarter "3010.dat"
                            , part transLightBlue ( 210, y, wz ) Quarter "3010.dat"
                            ]
                        )
                        [ -180, 0, 180 ]
                )
                [ 6, 7, 8, 9 ]
                ++ List.concatMap
                    (\wz ->
                        [ part white ( -230, layerY 5, wz ) Quarter "3010.dat"
                        , part white ( 230, layerY 5, wz ) Quarter "3010.dat"
                        , part white ( -230, layerY 10, wz ) Quarter "3010.dat"
                        , part white ( 230, layerY 10, wz ) Quarter "3010.dat"
                        ]
                    )
                    [ -180, 0, 180 ]

        roofZs =
            List.map (\i -> -340 + 40 * toFloat i) (List.range 0 17)

        lowerRoof =
            List.concatMap
                (\g ->
                    let
                        y =
                            layerY (16 + g)

                        xc =
                            230 - 20 * toFloat g
                    in
                    List.concatMap
                        (\z ->
                            [ part darkBluishGrey ( -xc, y, z ) Quarter "3039.dat"
                            , part darkBluishGrey ( xc, y, z ) ThreeQuarter "3039.dat"
                            ]
                        )
                        roofZs
                )
                [ 0, 1, 2, 3 ]

        upperRoof =
            List.concatMap
                (\g ->
                    let
                        y =
                            layerY (20 + g)

                        xc =
                            130 - 40 * toFloat g
                    in
                    List.concatMap
                        (\z ->
                            [ part darkBluishGrey ( -xc, y, z ) Quarter "3298.dat"
                            , part darkBluishGrey ( xc, y, z ) ThreeQuarter "3298.dat"
                            ]
                        )
                        roofZs
                )
                [ 0, 1, 2, 3 ]

        ridgeCap =
            List.map (\z -> part black ( 0, layerY 23 - 8, z ) Straight "3068b.dat") roofZs

        gables =
            let
                gableSpec g =
                    let
                        y =
                            layerY (16 + g)

                        halfInner =
                            if g < 4 then
                                220 - 20 * toFloat g

                            else
                                120 - 40 * toFloat (g - 4)

                        loftOpen =
                            if g <= 3 then
                                [ ( -40, 40 ) ]

                            else
                                []

                        loftDoors =
                            if g <= 3 then
                                [ part reddishBrown ( 0, y, -310 ) Straight "3010.dat"
                                , part reddishBrown ( 0, y, 310 ) Straight "3010.dat"
                                ]

                            else if g == 4 then
                                [ part white ( 0, y, -350 ) Straight "3010.dat"
                                , part white ( 0, y, 350 ) Straight "3010.dat"
                                , part darkBluishGrey ( 0, layerY 21, -360 ) Quarter "3004.dat"
                                ]

                            else
                                []
                    in
                    tileBricksX darkRed y -330 (modBy 2 g == 1) loftOpen ( -halfInner, halfInner )
                        ++ tileBricksX darkRed y 330 (modBy 2 g == 1) loftOpen ( -halfInner, halfInner )
                        ++ loftDoors
            in
            List.concatMap gableSpec (List.range 0 6)
    in
    model "Barn" (foundation ++ walls ++ cornerPilasters ++ barnDoors ++ sideWindows ++ lowerRoof ++ upperRoof ++ ridgeCap ++ gables)



-- BRICK WALL


{-| A section of garden wall along Z, 20 studs (2.5 m) long and two studs thick:
two stone-grey 2 x 2 pillar piers flanking five running-bond courses of red and
dark red bricks, capped with light grey coping plates and smooth tiles.
-}
brickWall : String
brickWall =
    let
        colourOf i j =
            if modBy 2 (i + j) == 0 then
                red

            else
                darkRed

        wallCourses =
            List.concatMap
                (\k ->
                    let
                        y =
                            layerY k
                    in
                    if modBy 2 k == 0 then
                        List.indexedMap (\j z -> part (colourOf k j) ( 0, y, z ) Quarter "3001.dat") [ -120, -40, 40, 120 ]

                    else
                        List.indexedMap
                            (\j ( z, file ) -> part (colourOf k j) ( 0, y, z ) Quarter file)
                            [ ( -140, "3003.dat" ), ( -80, "3001.dat" ), ( 0, "3001.dat" ), ( 80, "3001.dat" ), ( 140, "3003.dat" ) ]
                )
                [ 0, 1, 2, 3, 4 ]

        piers =
            List.concatMap
                (\z ->
                    List.map (\k -> part lightBluishGrey ( 0, layerY k, z ) Straight "3003.dat") [ 0, 1, 2, 3, 4, 5 ]
                        ++ [ part darkBluishGrey ( 0, layerY 5 - 8, z ) Straight "3068b.dat" ]
                )
                [ -180, 180 ]

        coping =
            List.concatMap
                (\z ->
                    [ part lightBluishGrey ( 0, layerY 4 - 8, z ) Quarter "3020.dat"
                    , part lightBluishGrey ( 0, layerY 4 - 16, z - 20 ) Straight "3068b.dat"
                    , part lightBluishGrey ( 0, layerY 4 - 16, z + 20 ) Straight "3068b.dat"
                    ]
                )
                [ -120, -40, 40, 120 ]
    in
    model "Brick wall" (wallCourses ++ piers ++ coping)



-- BRICK PALLET


{-| A full Euro-scale (6 x 6 stud = 0.75 m) wooden pallet stacked with four
courses of construction bricks: three 1 x 6 reddish brown runners under 2 x 6
pallet deck plates, loaded with alternating layers of red, yellow, blue and white
2 x 4 and 2 x 2 bricks.
-}
brickPallet : String
brickPallet =
    let
        runners =
            List.map (\x -> part reddishBrown ( x, -24, 0 ) Quarter "3009.dat") [ -50, 0, 50 ]

        deck =
            List.map (\z -> part reddishBrown ( 0, -32, z ) Straight "3795.dat") [ -40, 0, 40 ]

        layer1 =
            List.map (\( c, z ) -> part c ( -20, -56, z ) Straight "3001.dat") [ ( red, -40 ), ( yellow, 0 ), ( blue, 40 ) ]
                ++ List.map (\( c, z ) -> part c ( 40, -56, z ) Quarter "3003.dat") [ ( blue, -40 ), ( red, 0 ), ( white, 40 ) ]

        layer2 =
            List.map (\( c, x ) -> part c ( x, -80, 20 ) Quarter "3001.dat") [ ( yellow, -40 ), ( blue, 0 ), ( red, 40 ) ]
                ++ [ part white ( -40, -80, -40 ) Straight "3003.dat"
                   , part red ( 0, -80, -40 ) Straight "3003.dat"
                   , part yellow ( 40, -80, -40 ) Straight "3003.dat"
                   ]

        layer3 =
            [ part red ( -20, -104, -40 ) Straight "3001.dat"
            , part blue ( -20, -104, 0 ) Straight "3001.dat"
            , part yellow ( 40, -104, -20 ) Quarter "3001.dat"
            , part white ( -40, -104, 40 ) Straight "3003.dat"
            ]

        layer4 =
            [ part yellow ( -20, -128, -20 ) Quarter "3001.dat"
            , part red ( 20, -128, -40 ) Straight "3003.dat"
            ]
    in
    model "Brick pallet" (runners ++ deck ++ layer1 ++ layer2 ++ layer3 ++ layer4)



-- TILE STACK


{-| A 6 x 6 stud (0.75 m) builder's heap of crosswise grey and white 2 x 6 plates,
upper 2 x 4 and 2 x 2 plates, and smooth 2 x 2, 1 x 4 and 1 x 2 tiles on top.
-}
tileStack : String
tileStack =
    let
        straightLayer colour y =
            List.map (\z -> part colour ( 0, y, z ) Straight "3795.dat") [ -40, 0, 40 ]

        crossLayer colour y =
            List.map (\x -> part colour ( x, y, 0 ) Quarter "3795.dat") [ -40, 0, 40 ]
    in
    model "Tile stack"
        (straightLayer lightBluishGrey -8
            ++ crossLayer darkBluishGrey -16
            ++ straightLayer white -24
            ++ crossLayer lightBluishGrey -32
            ++ straightLayer darkBluishGrey -40
            ++ List.map (\( x, z ) -> part white ( x, -48, z ) Straight "3020.dat") [ ( -20, -40 ), ( -20, 0 ), ( -20, 40 ) ]
            ++ [ part white ( 40, -48, -20 ) Quarter "3020.dat"
               , part lightBluishGrey ( -20, -56, -20 ) Quarter "3020.dat"
               , part yellow ( 20, -56, -40 ) Straight "3022.dat"
               , part blue ( 20, -56, 0 ) Straight "3022.dat"
               , part white ( -20, -64, -40 ) Straight "3068b.dat"
               , part darkBluishGrey ( -20, -64, 0 ) Straight "3068b.dat"
               , part red ( 20, -64, -40 ) Straight "3068b.dat"
               , part yellow ( 20, -64, -10 ) Straight "3069b.dat"
               , part blue ( 20, -64, 10 ) Straight "3069b.dat"
               , part white ( -20, -72, 0 ) Straight "3068b.dat"
               ]
        )



-- BRICK GATE


{-| A tall brick trackside gateway along Z (14 studs = 1.75 m wide, 1.70 m tall):
dark grey plinths under white 2 x 2 pillars nine bricks high, bridged by a
red-and-white checkered lintel beam and topped with yellow cap tiles.
-}
brickGate : String
brickGate =
    let
        piers =
            List.concatMap
                (\z ->
                    part darkBluishGrey ( 0, layerY 0, z ) Straight "3003.dat"
                        :: List.map (\k -> part white ( 0, layerY k, z ) Straight "3003.dat") (List.range 1 9)
                )
                [ -120, 120 ]

        beam =
            List.indexedMap
                (\i z ->
                    part
                        (if modBy 2 i == 0 then
                            red

                         else
                            white
                        )
                        ( 0, layerY 10, z )
                        Quarter
                        "3003.dat"
                )
                [ -120, -80, -40, 0, 40, 80, 120 ]
                ++ List.map (\z -> part yellow ( 0, layerY 10 - 8, z ) Straight "3068b.dat") [ -120, -80, -40, 0, 40, 80, 120 ]
    in
    model "Brick gate" (piers ++ beam)



-- SLOPE BLOCK


{-| A substantial hazard chevron barrier block along Z (4 x 10 studs = 0.50 m x
1.25 m, 0.55 m tall): dark grey 2 x 8 + 2 x 2 base plates, alternating yellow
and black 1-brick core, 33° lower slopes, 45° upper slopes and smooth yellow ridge
tiles.
-}
slopeBlock : String
slopeBlock =
    let
        colourOf x =
            if x < 0 then
                yellow

            else
                black
    in
    model "Slope block"
        (List.concatMap
            (\x ->
                [ part darkBluishGrey ( x, -8, -20 ) Quarter "3034.dat"
                , part darkBluishGrey ( x, -8, 80 ) Straight "3022.dat"
                , part (colourOf x) ( x, -32, -80 ) Straight "3003.dat"
                , part (colourOf -x) ( x, -32, -40 ) Straight "3003.dat"
                , part (colourOf x) ( x, -32, 0 ) Straight "3003.dat"
                , part (colourOf -x) ( x, -32, 40 ) Straight "3003.dat"
                , part (colourOf x) ( x, -32, 80 ) Straight "3003.dat"
                , part (colourOf x) ( x, -56, -50 ) Straight "3298.dat"
                , part black ( x, -56, -20 ) Straight "3003.dat"
                , part black ( x, -56, 20 ) Straight "3003.dat"
                , part (colourOf -x) ( x, -56, 50 ) Half "3298.dat"
                , part (colourOf -x) ( x, -80, -10 ) Straight "3039.dat"
                , part (colourOf x) ( x, -80, 10 ) Half "3039.dat"
                , part yellow ( x, -88, 0 ) Straight "3068b.dat"
                ]
            )
            [ -20, 20 ]
        )



-- COUNTDOWN BOARD


{-| A full-scale trackside brake/countdown board (0.75 m wide x 1.15 m tall):
a dark grey base plate, twin light grey 1 x 1 posts carrying a 6 x 4 brick-built
vertical white board with a crisp diagonal red chevron stripe visible from both
front and back, capped with dark grey tiles.
-}
countdownBoard : String
countdownBoard =
    let
        baseAndPosts =
            [ part darkBluishGrey ( 0, -8, 0 ) Straight "3020.dat"
            , part lightBluishGrey ( -20, -32, 0 ) Straight "3005.dat"
            , part lightBluishGrey ( 20, -32, 0 ) Straight "3005.dat"
            , part lightBluishGrey ( -20, -56, 0 ) Straight "3005.dat"
            , part lightBluishGrey ( 20, -56, 0 ) Straight "3005.dat"
            , part lightBluishGrey ( -20, -80, 0 ) Straight "3005.dat"
            , part lightBluishGrey ( 20, -80, 0 ) Straight "3005.dat"
            ]

        board =
            [ -- Row 0 (y = -104)
              part red ( -40, -104, 0 ) Straight "3004.dat"
            , part white ( 20, -104, 0 ) Straight "3010.dat"

            -- Row 1 (y = -128)
            , part white ( -50, -128, 0 ) Straight "3005.dat"
            , part red ( -20, -128, 0 ) Straight "3004.dat"
            , part white ( 30, -128, 0 ) Straight "3622.dat"

            -- Row 2 (y = -152)
            , part white ( -40, -152, 0 ) Straight "3004.dat"
            , part red ( 0, -152, 0 ) Straight "3004.dat"
            , part white ( 40, -152, 0 ) Straight "3004.dat"

            -- Row 3 (y = -176)
            , part white ( -30, -176, 0 ) Straight "3622.dat"
            , part red ( 20, -176, 0 ) Straight "3004.dat"
            , part white ( 50, -176, 0 ) Straight "3005.dat"

            -- Cap tiles (y = -184)
            , part darkBluishGrey ( -20, -184, 0 ) Straight "2431.dat"
            , part darkBluishGrey ( 40, -184, 0 ) Straight "3069b.dat"
            ]
    in
    model "Countdown board" (baseAndPosts ++ board)



-- CHALET


{-| An authentic two-story alpine chalet (22 x 24 studs walls, 26 x 28 studs
overall with wide overhanging eaves and stepped cantilevered timber balconies,
3.80 m tall): a stone ground story, stepped upper log walls alternating reddish
brown and dark tan, flower-boxed balconies at front and back, white-framed
windows, a 45° dark red roof and a stone ridge chimney.
-}
chalet : String
chalet =
    let
        xHalf =
            220

        zHalf =
            240

        groundFloor =
            List.concatMap
                (\k ->
                    let
                        endDoor =
                            if k <= 4 then
                                [ ( -40, 40 ) ]

                            else
                                []

                        sideWins =
                            if k == 2 || k == 3 then
                                [ ( -120, -40 ), ( 40, 120 ) ]

                            else
                                []

                        corbelOpen =
                            if k == 4 then
                                [ ( -140, -120 ), ( -60, -40 ), ( 40, 60 ), ( 120, 140 ) ]

                            else
                                []
                    in
                    perimeterCourse
                        { xHalf = xHalf
                        , zHalf = zHalf
                        , layer = k
                        , frontColour = darkBluishGrey
                        , backColour = darkBluishGrey
                        , sideColour = darkBluishGrey
                        , frontOpenings = endDoor ++ corbelOpen
                        , backOpenings = endDoor ++ corbelOpen
                        , leftOpenings = sideWins
                        , rightOpenings = sideWins
                        }
                )
                (List.range 0 4)

        groundDetails =
            List.concatMap
                (\k ->
                    [ part reddishBrown ( 0, layerY k, -210 ) Straight "3010.dat"
                    , part reddishBrown ( 0, layerY k, 210 ) Straight "3010.dat"
                    ]
                )
                (List.range 0 4)
                ++ List.concatMap
                    (\k ->
                        List.concatMap
                            (\wz ->
                                [ part transLightBlue ( -190, layerY k, wz ) Quarter "3010.dat"
                                , part transLightBlue ( 190, layerY k, wz ) Quarter "3010.dat"
                                ]
                            )
                            [ -80, 80 ]
                    )
                    [ 2, 3 ]

        corbelsAndBalcony =
            let
                corbelXs =
                    [ -130, -50, 50, 130 ]

                corbels =
                    List.concatMap
                        (\x ->
                            [ part reddishBrown ( x, layerY 4, -230 ) Straight "3665b.dat"
                            , part reddishBrown ( x, layerY 4, 230 ) Half "3665b.dat"
                            ]
                        )
                        corbelXs

                deckAndRail zDeck zRail =
                    [ part darkTan ( -80, -128, zDeck ) Straight "3795.dat"
                    , part darkTan ( 0, -128, zDeck ) Straight "3022.dat"
                    , part darkTan ( 80, -128, zDeck ) Straight "3795.dat"
                    , part reddishBrown ( -130, -152, zRail ) Straight "3005.dat"
                    , part reddishBrown ( -50, -152, zRail ) Straight "3005.dat"
                    , part reddishBrown ( 50, -152, zRail ) Straight "3005.dat"
                    , part reddishBrown ( 130, -152, zRail ) Straight "3005.dat"
                    , part darkTan ( -80, -160, zRail ) Straight "3666.dat"
                    , part darkTan ( 0, -160, zRail ) Straight "3023b.dat"
                    , part darkTan ( 80, -160, zRail ) Straight "3666.dat"
                    , part green ( -90, -168, zRail ) Straight "3070b.dat"
                    , part red ( -70, -168, zRail ) Straight "3024.dat"
                    , part yellow ( 70, -168, zRail ) Straight "3024.dat"
                    , part green ( 90, -168, zRail ) Straight "3070b.dat"
                    ]
            in
            corbels ++ deckAndRail -260 -270 ++ deckAndRail 260 270

        upperFloor =
            List.concatMap
                (\k ->
                    let
                        timberCol =
                            if modBy 2 k == 1 then
                                reddishBrown

                            else
                                darkTan

                        endDoor =
                            if k <= 8 then
                                [ ( -60, 60 ) ]

                            else
                                []

                        sideWins =
                            if k >= 6 && k <= 8 then
                                [ ( -140, -60 ), ( 60, 140 ) ]

                            else
                                []
                    in
                    perimeterCourse
                        { xHalf = xHalf
                        , zHalf = zHalf
                        , layer = k
                        , frontColour = timberCol
                        , backColour = timberCol
                        , sideColour = timberCol
                        , frontOpenings = endDoor
                        , backOpenings = endDoor
                        , leftOpenings = sideWins
                        , rightOpenings = sideWins
                        }
                )
                (List.range 5 10)

        upperWindows =
            List.concatMap
                (\k ->
                    let
                        y =
                            layerY k
                    in
                    [ part transLightBlue ( 0, y, -210 ) Straight "3010.dat"
                    , part transLightBlue ( 0, y, 210 ) Straight "3010.dat"
                    , part white ( -50, y, -230 ) Straight "3005.dat"
                    , part white ( 50, y, -230 ) Straight "3005.dat"
                    , part white ( -50, y, 230 ) Straight "3005.dat"
                    , part white ( 50, y, 230 ) Straight "3005.dat"
                    ]
                        ++ (if k >= 6 && k <= 8 then
                                List.concatMap
                                    (\wz ->
                                        [ part transLightBlue ( -190, y, wz ) Quarter "3010.dat"
                                        , part transLightBlue ( 190, y, wz ) Quarter "3010.dat"
                                        ]
                                    )
                                    [ -100, 100 ]

                            else
                                []
                           )
                )
                [ 5, 6, 7, 8 ]
                ++ List.map (\z -> part reddishBrown ( -230, layerY 10, z ) Straight "3005.dat") [ -180, -90, 0, 90, 180 ]
                ++ List.map (\z -> part reddishBrown ( 230, layerY 10, z ) Straight "3005.dat") [ -180, -90, 0, 90, 180 ]

        roofZs =
            List.map (\i -> -260 + 40 * toFloat i) (List.range 0 13)

        roof =
            List.concatMap
                (\r ->
                    let
                        y =
                            layerY (11 + r)

                        xc =
                            230 - 20 * toFloat r
                    in
                    List.concatMap
                        (\z ->
                            [ part darkRed ( -xc, y, z ) Quarter "3039.dat"
                            , part darkRed ( xc, y, z ) ThreeQuarter "3039.dat"
                            ]
                        )
                        roofZs
                )
                (List.range 0 11)
                ++ (roofZs
                        |> List.filter (\z -> z /= 100)
                        |> List.map (\z -> part darkBluishGrey ( 0, layerY 22 - 8, z ) Straight "3068b.dat")
                   )

        gables =
            List.concatMap
                (\r ->
                    let
                        y =
                            layerY (11 + r)

                        halfInner =
                            220 - 20 * toFloat r

                        timberCol =
                            if modBy 2 r == 0 then
                                reddishBrown

                            else
                                darkTan

                        atticOpen =
                            if r >= 4 && r <= 6 then
                                [ ( -20, 20 ) ]

                            else
                                []

                        atticGlass =
                            if r >= 4 && r <= 6 then
                                [ part transLightBlue ( 0, y, -210 ) Straight "3004.dat"
                                , part transLightBlue ( 0, y, 210 ) Straight "3004.dat"
                                ]

                            else
                                []
                    in
                    tileBricksX timberCol y -230 (modBy 2 r == 1) atticOpen ( -halfInner, halfInner )
                        ++ tileBricksX timberCol y 230 (modBy 2 r == 1) atticOpen ( -halfInner, halfInner )
                        ++ atticGlass
                )
                (List.range 0 10)

        chimney =
            [ part lightBluishGrey ( 0, layerY 23, 100 ) Straight "3003.dat"
            , part lightBluishGrey ( 0, layerY 24, 100 ) Straight "3003.dat"
            , part darkBluishGrey ( 0, layerY 24 - 8, 100 ) Straight "3022.dat"
            ]
    in
    model "Alpine chalet" (groundFloor ++ groundDetails ++ corbelsAndBalcony ++ upperFloor ++ upperWindows ++ roof ++ gables ++ chimney)



-- PIT GARAGE


{-| A drive-through trackside pit garage bay (24 x 28 studs floor, 26 x 30 studs
canopy, 13.4 studs x 1.35 m clear interior drive-through bay so any soapbox
fits comfortably inside): six sturdy light grey pillars, an overhanging blue,
white and yellow team canopy roof with crest sign, an interior red tool
cabinet/workbench and two stacks of racing tyres lying flat.
-}
pitGarage : String
pitGarage =
    let
        floorYs =
            -8

        pillarTopY =
            -224

        floorXs =
            [ -200, -120, -40, 40, 120, 200 ]

        floorZs =
            [ -240, -160, -80, 0, 80, 160, 240 ]

        floorPad =
            List.concatMap
                (\x ->
                    List.map (\z -> part darkBluishGrey ( x, floorYs, z ) Straight "3031.dat") floorZs
                )
                floorXs
                ++ List.indexedMap
                    (\i z ->
                        part
                            (if modBy 2 i == 0 then
                                red

                             else
                                white
                            )
                            ( 0, floorYs - 8, z )
                            Quarter
                            "2431.dat"
                    )
                    [ -120, -40, 40, 120 ]

        pillarXZs =
            [ ( -220, -240 ), ( 220, -240 ), ( -220, 0 ), ( 220, 0 ), ( -220, 240 ), ( 220, 240 ) ]

        pillars =
            List.concatMap
                (\( x, z ) ->
                    [ part darkBluishGrey ( x, -32, z ) Straight "3003.dat" ]
                        ++ List.map (\k -> part lightBluishGrey ( x, -32 - 24 * toFloat k, z ) Straight "3003.dat") (List.range 1 6)
                        ++ [ part blue ( x, -200, z ) Straight "3003.dat"
                           , part white ( x, -224, z ) Straight "3003.dat"
                           ]
                )
                pillarXZs

        girderY =
            pillarTopY - 24

        girders =
            tileBricksX blue girderY -230 False [] ( -240, 240 )
                ++ tileBricksX blue girderY 230 False [] ( -240, 240 )
                ++ tileBricksZ blue girderY -230 False [] ( -220, 220 )
                ++ tileBricksZ blue girderY 230 False [] ( -220, 220 )

        fasciaY =
            girderY - 24

        fascia =
            perimeterCourseAt
                { xHalf = 260
                , zHalf = 300
                , y = fasciaY
                , stagger = False
                , frontColour = white
                , backColour = white
                , sideColour = yellow
                , frontOpenings = []
                , backOpenings = []
                , leftOpenings = []
                , rightOpenings = []
                }

        canopyDeckY =
            fasciaY - 8

        canopyDeck =
            List.concatMap
                (\x ->
                    List.map (\z -> part lightBluishGrey ( x, canopyDeckY, z ) Straight "3031.dat") floorZs
                )
                floorXs
                ++ List.concatMap
                    (\x ->
                        [ part blue ( x, canopyDeckY, -290 ) Straight "3069b.dat"
                        , part blue ( x, canopyDeckY, 290 ) Straight "3069b.dat"
                        ]
                    )
                    (List.map (\i -> -220 + 40 * toFloat i) (List.range 0 11))
                ++ List.concatMap
                    (\z ->
                        [ part blue ( -250, canopyDeckY, z ) Quarter "3069b.dat"
                        , part blue ( 250, canopyDeckY, z ) Quarter "3069b.dat"
                        ]
                    )
                    (List.map (\i -> -260 + 40 * toFloat i) (List.range 0 13))
                ++ [ part blue ( -250, canopyDeckY, -290 ) Straight "3070b.dat"
                   , part blue ( 250, canopyDeckY, -290 ) Straight "3070b.dat"
                   , part blue ( -250, canopyDeckY, 290 ) Straight "3070b.dat"
                   , part blue ( 250, canopyDeckY, 290 ) Straight "3070b.dat"
                   ]

        crestSign =
            [ part blue ( -60, canopyDeckY - 24, -270 ) Straight "3010.dat"
            , part white ( 0, canopyDeckY - 24, -270 ) Straight "3004.dat"
            , part yellow ( 60, canopyDeckY - 24, -270 ) Straight "3010.dat"
            , part red ( -40, canopyDeckY - 48, -270 ) Straight "3004.dat"
            , part white ( 0, canopyDeckY - 48, -270 ) Straight "3004.dat"
            , part red ( 40, canopyDeckY - 48, -270 ) Straight "3004.dat"
            , part blue ( -60, canopyDeckY - 24, 270 ) Straight "3010.dat"
            , part white ( 0, canopyDeckY - 24, 270 ) Straight "3004.dat"
            , part yellow ( 60, canopyDeckY - 24, 270 ) Straight "3010.dat"
            ]

        toolBench =
            [ -- Red mechanic tool cabinet (X: -190..-150, Z: -100..+60)
              part red ( -170, -32, -60 ) Quarter "3001.dat"
            , part red ( -170, -32, 20 ) Quarter "3001.dat"
            , part red ( -170, -56, -60 ) Quarter "3001.dat"
            , part red ( -170, -56, 20 ) Quarter "3001.dat"
            , part red ( -170, -80, -60 ) Quarter "3001.dat"
            , part red ( -170, -80, 20 ) Quarter "3001.dat"
            , part darkBluishGrey ( -170, -88, -60 ) Quarter "3020.dat"
            , part darkBluishGrey ( -170, -88, 20 ) Quarter "3020.dat"
            , part lightBluishGrey ( -170, -96, -60 ) Quarter "2412b.dat"
            , part yellow ( -170, -96, 0 ) Straight "3069b.dat"
            , -- Fuel jerrycan
              part red ( -170, -32, 120 ) Straight "3003.dat"
            , part yellow ( -170, -56, 120 ) Straight "3005.dat"
            ]

        tyreStacks =
            [ -- Stack of 3 balloon tyres lying flat (thickness = 25 -> halfThick = 12.5)
              part black ( 165, -20.5, -140 ) LieFlat "56890.dat"
            , part black ( 165, -45.5, -140 ) LieFlat "56890.dat"
            , part black ( 165, -70.5, -140 ) LieFlat "56890.dat"
            , part lightBluishGrey ( 165, -70.5, -140 ) LieFlat "6014b.dat"
            , -- Stack of 2 off-road tyres (thickness = 40 -> halfThick = 20) + small tyre top (thickness = 16)
              part black ( 165, -28, 140 ) LieFlat "56891.dat"
            , part black ( 165, -68, 140 ) LieFlat "56891.dat"
            , part black ( 165, -96, 140 ) LieFlat "3641.dat"
            ]
    in
    model "Pit garage" (floorPad ++ pillars ++ girders ++ fascia ++ canopyDeck ++ crestSign ++ toolBench ++ tyreStacks)
