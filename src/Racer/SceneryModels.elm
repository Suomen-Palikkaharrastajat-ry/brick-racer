module Racer.SceneryModels exposing (all, barrels, brickWall, crate, crateStack, gnome)

{-| The racer's own crashable roadside props, as LDraw models of catalog
parts, in the same conventions as `Game.BrickModels` (-Y up, 1 stud = 20 LDU,
1 brick = 24 LDU, a part's origin at the top of its body, the ground at
y = 0, centred on x = z = 0, the front towards LDraw -Z). They are drawn with
`Game.BrickProps` like the shared models.

Only the looks live here; where a crate, barrel, gnome or wall stands and
how big its collision cylinder is stays in `Racer.Track`.

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
    ]


type Turn
    = Straight
    | Quarter


part : Int -> ( Float, Float, Float ) -> Turn -> String -> String
part colour ( x, y, z ) turn file =
    let
        matrix =
            case turn of
                Straight ->
                    [ "1", "0", "0", "0", "1", "0", "0", "0", "1" ]

                Quarter ->
                    [ "0", "0", "1", "0", "1", "0", "-1", "0", "0" ]
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
