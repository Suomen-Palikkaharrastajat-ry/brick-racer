module CoursePreviewTest exposing (suite)

{-| The garage course preview (`Racer.CoursePreview`) keeps every tenth track
sample plus the finish, and its bounds hold the whole course.
-}

import Array
import Expect
import Game.MapParams as Map
import Math.Vector3 as Vec3
import Racer.CoursePreview as CoursePreview
import Racer.Track as Track
import Test exposing (Test, describe, test)


codes : List String
codes =
    [ "farmland-summer-clear-1", "desert-spring-storm-99" ]


suite : Test
suite =
    describe "Racer.CoursePreview"
        (List.concatMap
            (\code ->
                case Map.fromCode code of
                    Just map ->
                        let
                            track =
                                Track.generate (Track.trackConfigFor map)

                            preview =
                                CoursePreview.fromTrack track

                            sampleCount =
                                Array.length track.samples

                            lastIndex =
                                sampleCount - 1

                            expectedPoints =
                                -- every tenth sample from 0, plus the last one
                                -- unless it already falls on the stride
                                (lastIndex // CoursePreview.sampleStride)
                                    + 1
                                    + (if modBy CoursePreview.sampleStride lastIndex == 0 then
                                        0

                                       else
                                        1
                                      )

                            inside sample =
                                let
                                    x =
                                        Vec3.getX sample.center

                                    z =
                                        Vec3.getZ sample.center
                                in
                                x >= preview.minX && x <= preview.maxX && z >= preview.minZ && z <= preview.maxZ
                        in
                        [ test (code ++ ": one point per tenth sample and the finish") <|
                            \_ ->
                                Expect.equal expectedPoints (List.length preview.points)
                        , test (code ++ ": the bounds contain every sample and every point") <|
                            \_ ->
                                Expect.equal ( True, True )
                                    ( Array.toList track.samples |> List.all inside
                                    , preview.points
                                        |> List.all (\p -> p.x >= preview.minX && p.x <= preview.maxX && p.z >= preview.minZ && p.z <= preview.maxZ)
                                    )
                        , test (code ++ ": the bounds are not degenerate") <|
                            \_ ->
                                Expect.equal True (preview.maxZ - preview.minZ > 500 && preview.maxX > preview.minX)
                        ]

                    Nothing ->
                        [ test code <| \_ -> Expect.fail "bad map code" ]
            )
            codes
        )
