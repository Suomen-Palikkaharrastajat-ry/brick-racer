module Racer.CoursePreview exposing
    ( CoursePreview
    , PreviewPoint
    , fromTrack
    , sampleStride
    , view
    )

{-| A small plan view of a course for the garage (plan §3.3).

`fromTrack` keeps the `x, z` of every `sampleStride`-th track sample (and
always the last one, so the line reaches the finish) together with its road
surface, and the bounds of every sample. `view` draws them as an SVG, the
course running left to right from start to finish, scaled to fit with one
uniform scale so bends keep their true shape, and each stretch of road in its
surface's road colour (`Racer.TerrainMesh.roadColors`).

-}

import Array
import DesignTokens.Colors as Brand
import Html exposing (Html)
import Html.Attributes
import Math.Vector3 as Vec3 exposing (Vec3)
import Racer.TerrainMesh as TerrainMesh
import Racer.Types exposing (GeneratedTrack, SurfaceType)
import Svg exposing (Svg)
import Svg.Attributes as SvgA


{-| One kept sample: its plan position in metres and its road surface.
-}
type alias PreviewPoint =
    { x : Float
    , z : Float
    , surface : SurfaceType
    }


{-| The kept points, start first, and the plan bounds of every track sample.
-}
type alias CoursePreview =
    { points : List PreviewPoint
    , minX : Float
    , maxX : Float
    , minZ : Float
    , maxZ : Float
    }


{-| Every how many track samples one point is kept (samples are 2 m apart,
so one point per 20 m).
-}
sampleStride : Int
sampleStride =
    10


{-| The preview of a generated course.
-}
fromTrack : GeneratedTrack -> CoursePreview
fromTrack track =
    let
        samples =
            Array.toList track.samples

        lastIndex =
            List.length samples - 1

        points =
            samples
                |> List.indexedMap Tuple.pair
                |> List.filter (\( i, _ ) -> modBy sampleStride i == 0 || i == lastIndex)
                |> List.map
                    (\( _, sample ) ->
                        { x = Vec3.getX sample.center, z = Vec3.getZ sample.center, surface = sample.surface }
                    )

        xs =
            List.map (.center >> Vec3.getX) samples

        zs =
            List.map (.center >> Vec3.getZ) samples
    in
    { points = points
    , minX = List.minimum xs |> Maybe.withDefault 0
    , maxX = List.maximum xs |> Maybe.withDefault 0
    , minZ = List.minimum zs |> Maybe.withDefault 0
    , maxZ = List.maximum zs |> Maybe.withDefault 0
    }


{-| Draw the preview. `label` is the accessible name of the picture.
-}
view : String -> CoursePreview -> Html msg
view label preview =
    let
        -- The course runs along z, so z is drawn across and x down.
        spanAcross =
            max 1 (preview.maxZ - preview.minZ)

        spanDown =
            max 1 (preview.maxX - preview.minX)

        widthUnits =
            300

        scale =
            -- Fit across; a course is far longer than it is wide, but cap the
            -- height in case a wide one ever comes along.
            min (toFloat widthUnits / spanAcross) (60 / spanDown)

        padding =
            6

        heightUnits =
            spanDown * scale + 2 * padding

        project point =
            ( padding + (point.z - preview.minZ) * scale
            , padding + (point.x - preview.minX) * scale
            )

        segments =
            List.map2 Tuple.pair preview.points (List.drop 1 preview.points)
                |> List.map (\( a, b ) -> segment (project a) (project b) a.surface)

        endDot colour point =
            let
                ( cx, cy ) =
                    project point
            in
            Svg.circle
                [ SvgA.cx (String.fromFloat cx)
                , SvgA.cy (String.fromFloat cy)
                , SvgA.r "3.5"
                , SvgA.fill colour
                ]
                []

        dots =
            List.filterMap identity
                [ List.head preview.points |> Maybe.map (endDot Brand.legoWhite)
                , List.reverse preview.points |> List.head |> Maybe.map (endDot Brand.legoYellow)
                ]
    in
    Svg.svg
        [ SvgA.viewBox ("0 0 " ++ String.fromInt (widthUnits + 2 * padding) ++ " " ++ String.fromFloat heightUnits)
        , SvgA.width "100%"
        , SvgA.preserveAspectRatio "xMidYMid meet"
        , SvgA.class "racer-course-preview"
        , Html.Attributes.attribute "role" "img"
        , Html.Attributes.attribute "aria-label" label
        ]
        (Svg.title [] [ Svg.text label ] :: segments ++ dots)


segment : ( Float, Float ) -> ( Float, Float ) -> SurfaceType -> Svg msg
segment ( x1, y1 ) ( x2, y2 ) surface =
    Svg.line
        [ SvgA.x1 (String.fromFloat x1)
        , SvgA.y1 (String.fromFloat y1)
        , SvgA.x2 (String.fromFloat x2)
        , SvgA.y2 (String.fromFloat y2)
        , SvgA.stroke (cssColour (TerrainMesh.roadColors surface).road)
        , SvgA.strokeWidth "4"
        , SvgA.strokeLinecap "round"
        ]
        []


cssColour : Vec3 -> String
cssColour colour =
    let
        channel value =
            String.fromInt (round (255 * clamp 0 1 value))
    in
    "rgb(" ++ channel (Vec3.getX colour) ++ "," ++ channel (Vec3.getY colour) ++ "," ++ channel (Vec3.getZ colour) ++ ")"
