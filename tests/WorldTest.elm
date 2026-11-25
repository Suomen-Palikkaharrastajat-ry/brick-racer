module WorldTest exposing (suite)

{-| Tests for how season and weather change grip, rolling resistance, fog
and precipitation (`Racer.World`).
-}

import Expect
import Game.MapParams as Map exposing (Biome(..), MapParams, WeatherKind(..))
import Racer.World as World exposing (Fall(..))
import Test exposing (Test, describe, test)


everyMap : List MapParams
everyMap =
    List.concatMap
        (\season ->
            List.map (\weather -> { biome = Farmland, season = season, weather = weather, seed = 1 }) Map.allWeathers
        )
        Map.allSeasons


neutral : MapParams
neutral =
    { biome = Farmland, season = Map.Summer, weather = Clear, seed = 1 }


suite : Test
suite =
    describe "Racer.World"
        [ test "the summer clear map is neutral" <|
            \_ ->
                Expect.equal
                    ( 1.0, 1.0, { near = 140, far = 620 }, Nothing )
                    ( World.gripScale neutral
                    , World.rollingResistScale neutral
                    , World.fogRange neutral
                    , World.precipitation neutral |> Maybe.map .fall
                    )
        , test "grip stays between 0.6 and 1.0 for every season and weather" <|
            \_ ->
                Expect.equal []
                    (List.filter (\m -> World.gripScale m < 0.6 || World.gripScale m > 1.0) everyMap)
        , test "rolling resistance stays between 1.0 and 1.5 for every season and weather" <|
            \_ ->
                Expect.equal []
                    (List.filter (\m -> World.rollingResistScale m < 1.0 || World.rollingResistScale m > 1.5) everyMap)
        , test "winter snow is slipperier and rolls harder than summer clear" <|
            \_ ->
                let
                    winterSnow =
                        { biome = Fell, season = Map.Winter, weather = Snow, seed = 1 }
                in
                Expect.equal [ True, True ]
                    [ World.gripScale winterSnow < World.gripScale neutral
                    , World.rollingResistScale winterSnow > World.rollingResistScale neutral
                    ]
        , test "fog is closer in fog than in clear weather" <|
            \_ ->
                Expect.equal [ True ]
                    [ (World.fogRange { neutral | weather = Fog }).near < (World.fogRange neutral).near ]
        , test "only rain, storm and snow fall" <|
            \_ ->
                Expect.equal [ Nothing, Nothing, Just RainFall, Just RainFall, Nothing, Just SnowFall ]
                    (List.map
                        (\w -> World.precipitation { neutral | weather = w } |> Maybe.map .fall)
                        [ Clear, Overcast, Rain, Storm, Fog, Snow ]
                    )
        , test "lightning only in a storm" <|
            \_ ->
                Expect.equal [ False, False, False, True, False, False ]
                    (List.map (\w -> World.lightning { neutral | weather = w }) Map.allWeathers)
        ]
