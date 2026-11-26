module Racer.MenuIcons exposing
    ( biome
    , dice
    , season
    , steepness
    , weather
    )

{-| Icons of the main menu's world row (`Pages.Garage`).

Feather has no dice, trees, cactus, flower, leaf or snowflake, so those are
drawn here with `FeatherIcons.customIcon` in Feather's own style: a 24 by 24
view box, a 2 px `currentColor` stroke and round caps. The others are Feather
icons.

-}

import FeatherIcons exposing (Icon)
import Game.MapParams exposing (Biome(..), Season(..), WeatherKind(..))
import Racer.Course exposing (Steepness(..))
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


{-| A die showing five pips, for the "random world" button.
-}
dice : Icon
dice =
    FeatherIcons.customIcon
        [ Svg.rect [ SvgAttr.x "3", SvgAttr.y "3", SvgAttr.width "18", SvgAttr.height "18", SvgAttr.rx "3" ] []
        , pip 8 8
        , pip 16 8
        , pip 12 12
        , pip 8 16
        , pip 16 16
        ]


{-| The icon of a biome: a ploughed field, a spruce, a fell or a cactus.
-}
biome : Biome -> Icon
biome b =
    case b of
        Farmland ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M2 20h20" ] []
                , Svg.path [ SvgAttr.d "M4 16c4-2 12-2 16 0" ] []
                , Svg.path [ SvgAttr.d "M7 12c3-1.5 7-1.5 10 0" ] []
                , Svg.path [ SvgAttr.d "M10 8.5c1.3-.6 2.7-.6 4 0" ] []
                ]

        Boreal ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M12 2l-5 7h3l-4 6h4l-3 4h10l-3-4h4l-4-6h3z" ] []
                , Svg.path [ SvgAttr.d "M12 19v3" ] []
                ]

        Fell ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M2 20l7-12 4 6 3-4 6 10z" ] []
                , Svg.path [ SvgAttr.d "M7.3 11l1.7 1.5 1.6-1.5" ] []
                ]

        Desert ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M12 21V5a2 2 0 0 1 4 0v16" ] []
                , Svg.path [ SvgAttr.d "M16 12h2a2 2 0 0 0 2-2V8" ] []
                , Svg.path [ SvgAttr.d "M12 14h-2a2 2 0 0 1-2-2V9" ] []
                , Svg.path [ SvgAttr.d "M4 21h16" ] []
                ]


{-| The icon of a season: a flower, the sun, a falling leaf or a snowflake.
-}
season : Season -> Icon
season s =
    case s of
        Spring ->
            FeatherIcons.customIcon
                [ Svg.circle [ SvgAttr.cx "12", SvgAttr.cy "5", SvgAttr.r "2.5" ] []
                , Svg.circle [ SvgAttr.cx "16", SvgAttr.cy "9", SvgAttr.r "2.5" ] []
                , Svg.circle [ SvgAttr.cx "8", SvgAttr.cy "9", SvgAttr.r "2.5" ] []
                , Svg.circle [ SvgAttr.cx "12", SvgAttr.cy "9", SvgAttr.r "1" ] []
                , Svg.path [ SvgAttr.d "M12 12v10" ] []
                , Svg.path [ SvgAttr.d "M12 18c-2 0-4-1-5-3 2 0 4 1 5 3z" ] []
                ]

        Summer ->
            FeatherIcons.sun

        Autumn ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M5 19C5 10 11 4 20 4c0 9-6 15-15 15z" ] []
                , Svg.path [ SvgAttr.d "M5 19l9-9" ] []
                ]

        Winter ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M12 2v20M3.3 7l17.4 10M3.3 17L20.7 7" ] []
                , Svg.path [ SvgAttr.d "M9.5 3.5L12 6l2.5-2.5M9.5 20.5L12 18l2.5 2.5" ] []
                ]


{-| The icon of a weather. Fog is three soft lines; the others are Feather's
sun and cloud icons.
-}
weather : WeatherKind -> Icon
weather w =
    case w of
        Clear ->
            FeatherIcons.sun

        Overcast ->
            FeatherIcons.cloud

        Rain ->
            FeatherIcons.cloudRain

        Storm ->
            FeatherIcons.cloudLightning

        Fog ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M3 8h18M5 12h14M3 16h18M7 20h10" ] []
                ]

        Snow ->
            FeatherIcons.cloudSnow


{-| The icon of a course's steepness: a low wedge or a tall one.
-}
steepness : Steepness -> Icon
steepness s =
    case s of
        Gentle ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M2 20h20L2 12z" ] [] ]

        Steep ->
            FeatherIcons.customIcon
                [ Svg.path [ SvgAttr.d "M2 20h20L2 4z" ] [] ]


{-| One filled pip of the die.
-}
pip : Float -> Float -> Svg msg
pip x y =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat x)
        , SvgAttr.cy (String.fromFloat y)
        , SvgAttr.r "1.3"
        , SvgAttr.fill "currentColor"
        ]
        []
