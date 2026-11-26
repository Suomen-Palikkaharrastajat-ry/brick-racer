module Racer.SceneryModels exposing (all, barrels, brickGate, brickPallet, brickWall, chalet, countdownBoard, crate, crateStack, gnome, pitGarage, slopeBlock, tileStack)

{-| The racer's own crashable roadside props, as LDraw models of catalog
parts, in the same conventions as `Game.BrickModels` (-Y up, 1 stud = 20 LDU,
1 brick = 24 LDU, a part's origin at the top of its body, the ground at
y = 0, centred on x = z = 0, the front towards LDraw -Z). They are drawn with
`Game.BrickProps` like the shared models.

Only the looks live here; where a crate, barrel, gnome, wall, pallet, tile
stack, gate or slope block stands and how big its collision cylinder is stays
in `Racer.Track`.

The brick pallet, tile stack, brick gate and slope block use only parts of the
car editor's dock (`Racer.Editor.parts`), which `tests/SceneryModelsTest.elm`
checks.

-}


{-| Every model with its name, for loading them all.
-}
all : List ( String, String )
all =
    [ ( "crate", crate )
    , ( "crateStack", crateStack )
    , ( "barrels", barrels )
    , ( "gnome", gnome )
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
    in
    String.join " " ([ "1", String.fromInt colour, String.fromFloat x, String.fromFloat y, String.fromFloat z ] ++ matrix ++ [ file ])


model : String -> List String -> String
model name lines =
    String.join "\n" (("0 " ++ name) :: "0 Author: Brick Racer" :: lines)


reddishBrown : Int
reddishBrown =
    70


darkTan : Int
darkTan =
    28


blue : Int
blue =
    1


red : Int
red =
    4


darkRed : Int
darkRed =
    320


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
        [ part 0 ( 0, -8, 0 ) Straight "3022.dat"
        , part blue ( 0, -32, 0 ) Straight "3003.dat"
        , part red ( 0, -56, 0 ) Straight "3003.dat"
        , part 15 ( 0, -64, 0 ) Straight "3022.dat"
        , part 78 ( 0, -88, 0 ) Straight "3062b.dat"
        , part red ( 0, -112, 0 ) Straight "4589.dat"
        ]



-- BRICK WALL


{-| A section of garden wall along Z, two studs thick: four courses of red and
dark red 2 x 4 bricks in running bond (2 x 2 bricks at the ends of every other
course), capped with grey 2 x 4 plates.
-}
brickWall : String
brickWall =
    let
        colourOf i j =
            if modBy 2 (i + j) == 0 then
                red

            else
                darkRed

        course i =
            let
                y =
                    -24 * toFloat (i + 1)
            in
            if modBy 2 i == 0 then
                List.indexedMap (\j z -> part (colourOf i j) ( 0, y, z ) Quarter "3001.dat") [ -80, 0, 80 ]

            else
                List.indexedMap
                    (\j ( z, file ) -> part (colourOf i j) ( 0, y, z ) Quarter file)
                    [ ( -100, "3003.dat" ), ( -40, "3001.dat" ), ( 40, "3001.dat" ), ( 100, "3003.dat" ) ]
    in
    model "Brick wall"
        (List.concatMap course [ 0, 1, 2, 3 ]
            ++ List.map (\z -> part lightBluishGrey ( 0, -104, z ) Quarter "3020.dat") [ -80, 0, 80 ]
        )



-- BRICK PALLET


{-| A wooden pallet of loose bricks waiting to be built: three reddish brown
1 x 4 brick runners under a 4 x 4 plate, with three courses of red, yellow and
blue 2 x 4 bricks laid crosswise on it (a 2 x 2 brick in the top course).
-}
brickPallet : String
brickPallet =
    model "Brick pallet"
        (List.map (\z -> part reddishBrown ( 0, -24, z ) Straight "3010.dat") [ -30, 0, 30 ]
            ++ [ part reddishBrown ( 0, -32, 0 ) Straight "3031.dat"
               , part red ( 0, -56, -20 ) Straight "3001.dat"
               , part yellow ( 0, -56, 20 ) Straight "3001.dat"
               , part blue ( -20, -80, 0 ) Quarter "3001.dat"
               , part red ( 20, -80, 0 ) Quarter "3001.dat"
               , part yellow ( 0, -104, -20 ) Straight "3001.dat"
               , part blue ( -20, -104, 20 ) Straight "3003.dat"
               ]
        )



-- TILE STACK


{-| A heap of spare plates and tiles: crosswise layers of grey and white 2 x 4
plates, a layer of 2 x 2 plates, then a few 2 x 2, 1 x 2 and 1 x 1 tiles on
top.
-}
tileStack : String
tileStack =
    let
        straightLayer colour y =
            List.map (\z -> part colour ( 0, y, z ) Straight "3020.dat") [ -20, 20 ]

        crossLayer colour y =
            List.map (\x -> part colour ( x, y, 0 ) Quarter "3020.dat") [ -20, 20 ]
    in
    model "Tile stack"
        (straightLayer lightBluishGrey -8
            ++ crossLayer darkBluishGrey -16
            ++ straightLayer white -24
            ++ crossLayer lightBluishGrey -32
            ++ straightLayer darkBluishGrey -40
            ++ List.map (\( x, z ) -> part white ( x, -48, z ) Straight "3022.dat") [ ( -20, -20 ), ( 20, -20 ), ( -20, 20 ), ( 20, 20 ) ]
            ++ [ part lightBluishGrey ( 0, -56, -20 ) Straight "3020.dat"
               , part yellow ( -20, -56, 20 ) Straight "3022.dat"
               , part white ( -20, -64, -20 ) Straight "3068b.dat"
               , part darkBluishGrey ( 20, -64, -20 ) Straight "3068b.dat"
               , part blue ( -20, -64, 10 ) Straight "3069b.dat"
               , part red ( -30, -64, 30 ) Straight "3070b.dat"
               ]
        )



-- BRICK GATE


{-| A garden gateway along Z: two white 2 x 2 brick posts four bricks tall,
bridged by a red 1 x 6 brick topped with a yellow 1 x 4 tile.
-}
brickGate : String
brickGate =
    model "Brick gate"
        (List.concatMap
            (\z -> List.map (\i -> part white ( 0, -24 * toFloat i, z ) Straight "3003.dat") [ 1, 2, 3, 4 ])
            [ -50, 50 ]
            ++ [ part red ( 0, -120, 0 ) Quarter "3009.dat"
               , part yellow ( 0, -128, 0 ) Quarter "2431.dat"
               ]
        )



-- SLOPE BLOCK


{-| A low chevron block along Z: grey 2 x 8 plates under yellow and black
33° and 45° slopes rising from both ends to a ridge of yellow 2 x 2 tiles over
a black 2 x 2 brick core. (A slope's origin is over its top stud row, so the
slope runs out to -Z from it.)
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
                [ part darkBluishGrey ( x, -8, 0 ) Quarter "3034.dat"
                , part (colourOf x) ( x, -32, -30 ) Straight "3298.dat"
                , part black ( x, -32, 0 ) Straight "3003.dat"
                , part (colourOf -x) ( x, -32, 30 ) Half "3298.dat"
                , part (colourOf -x) ( x, -56, -10 ) Straight "3039.dat"
                , part (colourOf x) ( x, -56, 10 ) Half "3039.dat"
                , part yellow ( x, -64, 0 ) Straight "3068b.dat"
                ]
            )
            [ -20, 20 ]
        )



-- COUNTDOWN BOARD


{-| A trackside distance countdown marker board: a white 2 x 4 plate face on a
grey post with red diagonal hash plates.
-}
countdownBoard : String
countdownBoard =
    model "Countdown board"
        [ part darkBluishGrey ( 0, -8, 0 ) Straight "3022.dat"
        , part lightBluishGrey ( 0, -32, 0 ) Straight "3004.dat"
        , part lightBluishGrey ( 0, -56, 0 ) Straight "3004.dat"
        , part white ( 0, -80, 0 ) Straight "3020.dat"
        , part red ( -20, -80, 0 ) Straight "3024.dat"
        , part red ( 0, -80, 0 ) Straight "3024.dat"
        , part red ( 20, -80, 0 ) Straight "3024.dat"
        ]



-- CHALET


{-| An alpine timber chalet: a stone foundation, reddish-brown timber walls,
windows, a dark red 45° slope roof, and a chimney, built to genuine 1:1 scale.
-}
chalet : String
chalet =
    let
        layerY k =
            -24 * toFloat (k + 1)

        foundation =
            List.concat
                [ List.concatMap
                    (\z ->
                        [ part darkBluishGrey ( -60, layerY 0, z ) Quarter "3001.dat"
                        , part darkBluishGrey ( 60, layerY 0, z ) Quarter "3001.dat"
                        ]
                    )
                    [ -80, 0, 80 ]
                , List.concatMap
                    (\x ->
                        [ part darkBluishGrey ( x, layerY 0, -100 ) Straight "3001.dat"
                        , part darkBluishGrey ( x, layerY 0, 100 ) Straight "3001.dat"
                        ]
                    )
                    [ -20, 20 ]
                ]

        wallLayer k =
            let
                col =
                    if modBy 2 k == 0 then
                        reddishBrown

                    else
                        darkTan

                sides =
                    List.concatMap
                        (\z ->
                            if z == 0 && (k == 2 || k == 3) then
                                [ part white ( -60, layerY k, z ) Quarter "3004.dat"
                                , part white ( 60, layerY k, z ) Quarter "3004.dat"
                                ]

                            else
                                [ part col ( -60, layerY k, z ) Quarter "3001.dat"
                                , part col ( 60, layerY k, z ) Quarter "3001.dat"
                                ]
                        )
                        [ -80, 0, 80 ]

                ends =
                    List.concatMap
                        (\x ->
                            [ part col ( x, layerY k, -100 ) Straight "3001.dat"
                            , part col ( x, layerY k, 100 ) Straight "3001.dat"
                            ]
                        )
                        [ -20, 20 ]
            in
            sides ++ ends

        walls =
            List.concatMap wallLayer [ 1, 2, 3, 4 ]

        roofZ =
            [ -100, -60, -20, 20, 60, 100 ]

        roof =
            List.concatMap
                (\z ->
                    [ part darkRed ( -70, layerY 5, z ) Quarter "3039.dat"
                    , part darkRed ( 70, layerY 5, z ) Straight "3039.dat"
                    , part darkRed ( -30, layerY 6, z ) Quarter "3039.dat"
                    , part darkRed ( 30, layerY 6, z ) Straight "3039.dat"
                    , part darkRed ( 0, layerY 7, z ) Straight "3020.dat"
                    ]
                )
                roofZ

        chimney =
            [ part lightBluishGrey ( 50, layerY 6, -60 ) Straight "3005.dat"
            , part lightBluishGrey ( 50, layerY 7, -60 ) Straight "3005.dat"
            , part lightBluishGrey ( 50, layerY 8, -60 ) Straight "3005.dat"
            ]
    in
    model "Alpine chalet" (foundation ++ walls ++ roof ++ chimney)



-- PIT GARAGE


{-| A roadside pit garage bay: a dark grey base plate, four light grey corner
pillars, a blue canopy roof, and stacked tires and oil drums inside, built to 1:1 scale.
-}
pitGarage : String
pitGarage =
    let
        layerY k =
            -24 * toFloat (k + 1)

        floorPlate =
            List.concatMap
                (\x ->
                    List.map (\z -> part darkBluishGrey ( x, -8, z ) Straight "3034.dat") [ -80, 0, 80 ]
                )
                [ -40, 40 ]

        pillars =
            List.concatMap
                (\( x, z ) ->
                    List.map (\k -> part lightBluishGrey ( x, layerY k, z ) Straight "3003.dat") [ 0, 1, 2, 3, 4 ]
                )
                [ ( -60, -100 ), ( 60, -100 ), ( -60, 100 ), ( 60, 100 ) ]

        beams =
            [ part lightBluishGrey ( 0, layerY 5, -100 ) Straight "3034.dat"
            , part lightBluishGrey ( 0, layerY 5, 100 ) Straight "3034.dat"
            , part lightBluishGrey ( -60, layerY 5, 0 ) Quarter "3034.dat"
            , part lightBluishGrey ( 60, layerY 5, 0 ) Quarter "3034.dat"
            ]

        canopy =
            List.concatMap
                (\x ->
                    List.map (\z -> part blue ( x, layerY 6, z ) Straight "3031.dat") [ -60, 20 ]
                )
                [ -40, 0, 40 ]

        inside =
            [ part red ( -40, layerY 0, -80 ) Straight "3003.dat"
            , part red ( -40, layerY 1, -80 ) Straight "3003.dat"
            , part yellow ( -40, layerY 2 + 16, -80 ) Straight "2412b.dat"
            , part black ( 40, layerY 0, -80 ) Straight "3641.dat"
            , part black ( 40, layerY 0 - 10, -80 ) Straight "3641.dat"
            , part black ( 40, layerY 0 - 20, -80 ) Straight "3641.dat"
            ]
    in
    model "Pit garage" (floorPlate ++ pillars ++ beams ++ canopy ++ inside)
