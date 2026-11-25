module Racer.Course exposing
    ( CourseParams
    , courseParams
    , generateKnots
    )

{-| Seeded course blueprints for a map.

A map (`Game.MapParams`) draws its course shape from its biome: length,
drop, road half-width, how far the course swings sideways, how many wash
and launch-ramp features it has, and which road surfaces it uses. Those
draws come from `Game.MapParams.streamSeed`, so the same map code always gives
the same course.

`generateKnots` turns a `CourseParams` into the `BlueprintKnot` list that
`Racer.Track` already knows how to turn into control points, so the spline,
checkpoints, ramps, loops and scenery stay as they are:

  - the run-in and the finish are pinned: the first knot is at the start
    with no offset, the last is at the finish and slightly wider;
  - forward progress and the drop only increase, so the course never doubles
    back on itself;
  - the sideways walk alternates bend direction (with a 0.75 chance of
    turning back) and never steps further than 0.55 m sideways per metre
    forward;
  - surfaces change in runs of two knots;
  - washes and launch ramps sit on middle knots, never next to each other.

-}

import Array exposing (Array)
import Game.MapParams as Map exposing (Biome(..), MapParams)
import Racer.Types exposing (BlueprintKnot, SegmentFeature(..), SurfaceType(..))
import Random exposing (Seed)


{-| The drawn shape of one course.
-}
type alias CourseParams =
    { lengthM : Float
    , dropM : Float
    , halfWidthM : Float
    , swingM : Float
    , knotCount : Int
    , washes : Int
    , ramps : Int
    , surfaces : List ( SurfaceType, Float )
    }


type alias Shape =
    { lengthM : ( Float, Float )
    , dropM : ( Float, Float )
    , halfWidthM : ( Float, Float )
    , swingM : Float
    , washes : Int
    , ramps : Int
    , surfaces : List ( SurfaceType, Float )
    }


shapeOf : Biome -> Shape
shapeOf biome =
    case biome of
        Farmland ->
            { lengthM = ( 880, 1000 )
            , dropM = ( 280, 320 )
            , halfWidthM = ( 4.2, 4.6 )
            , swingM = 56
            , washes = 1
            , ramps = 0
            , surfaces = [ ( DirtRoad, 0.6 ), ( GravelRoad, 0.4 ) ]
            }

        Boreal ->
            { lengthM = ( 980, 1100 )
            , dropM = ( 320, 360 )
            , halfWidthM = ( 3.8, 4.1 )
            , swingM = 36
            , washes = 1
            , ramps = 1
            , surfaces = [ ( ForestTrack, 0.7 ), ( GravelRoad, 0.3 ) ]
            }

        Fell ->
            { lengthM = ( 820, 960 )
            , dropM = ( 330, 380 )
            , halfWidthM = ( 3.3, 3.8 )
            , swingM = 30
            , washes = 1
            , ramps = 1
            , surfaces = [ ( GrassPath, 0.7 ), ( DirtRoad, 0.3 ) ]
            }

        Desert ->
            { lengthM = ( 900, 1040 )
            , dropM = ( 290, 330 )
            , halfWidthM = ( 3.9, 4.4 )
            , swingM = 48
            , washes = 0
            , ramps = 2
            , surfaces = [ ( GravelRoad, 0.6 ), ( DirtRoad, 0.4 ) ]
            }


{-| The course shape of a map, drawn from its seed.
-}
courseParams : MapParams -> CourseParams
courseParams map =
    let
        shape =
            shapeOf map.biome

        seed0 =
            Random.initialSeed (Map.streamSeed "shape" map)

        ( lengthM, seed1 ) =
            draw shape.lengthM seed0

        ( dropM, seed2 ) =
            draw shape.dropM seed1

        ( halfWidthM, seed3 ) =
            draw shape.halfWidthM seed2

        ( knotCount, _ ) =
            Random.step (Random.int 8 9) seed3
    in
    { lengthM = lengthM
    , dropM = dropM
    , halfWidthM = halfWidthM
    , swingM = shape.swingM
    , knotCount = knotCount
    , washes = shape.washes
    , ramps = shape.ramps
    , surfaces = shape.surfaces
    }


{-| The blueprint knots of a course, in order from the start to the finish.
`attempt` picks one of several seeded versions of the same map, so that a
course which fails validation can be replaced by the next attempt.
-}
generateKnots : Int -> MapParams -> CourseParams -> List BlueprintKnot
generateKnots attempt map cp =
    let
        n =
            max 6 cp.knotCount

        seed0 =
            Random.initialSeed (Map.streamSeed ("knots/" ++ String.fromInt attempt) map)

        -- Forward progress: n - 1 random gaps. The total is scaled below so
        -- that the course, with its drop and sideways swing, has its length.
        ( zGaps, seed1 ) =
            randomsIn (n - 1) ( 0.7, 1.3 ) seed0

        zShares =
            shareCumulative zGaps

        -- Drop: gaps of random size, the first capped and one middle gap steeper.
        ( dropGaps, seed2 ) =
            randomsIn (n - 1) ( 0.6, 1.5 ) seed1

        ( steepU, seed3 ) =
            draw ( 0, 1 ) seed2

        steepIndex =
            1 + floor (steepU * toFloat (n - 4))

        adjustedDrop =
            List.indexedMap
                (\i g ->
                    if i == 0 then
                        min g 0.6

                    else if i == steepIndex then
                        g * 1.4

                    else
                        g
                )
                dropGaps

        yArr =
            Array.fromList (shareCumulative adjustedDrop)

        -- Sideways: interior knots get a random sign and size.
        ( sJitter, seed4 ) =
            randomsIn (n - 2) ( -0.02, 0.02 ) seed3

        ( ampU, seed5 ) =
            randomsIn (n - 2) ( 0.45, 1.0 ) seed4

        ( flipU, seed6 ) =
            randomsIn (n - 2) ( 0, 1 ) seed5

        ( startU, seed7 ) =
            draw ( 0, 1 ) seed6

        ( widthJitter, seed8 ) =
            randomsIn n ( -0.2, 0.2 ) seed7

        ( surfaceU, seed9 ) =
            randomsIn (n // 2 + 1) ( 0, 1 ) seed8

        ( featureU, seed10 ) =
            randomsIn n ( 0, 1 ) seed9

        ( priorityU, _ ) =
            randomsIn n ( 0, 1 ) seed10

        ampArr =
            Array.fromList ampU

        flipArr =
            Array.fromList flipU

        jitterArr =
            Array.fromList sJitter

        widthArr =
            Array.fromList widthJitter

        surfaceArr =
            Array.fromList surfaceU

        featureArr =
            Array.fromList featureU

        priorityArr =
            Array.fromList priorityU

        sArr =
            Array.fromList
                (List.map
                    (\i ->
                        if i == 0 then
                            0

                        else if i == n - 1 then
                            1

                        else
                            toFloat i / toFloat (n - 1) + at jitterArr (i - 1)
                    )
                    (List.range 0 (n - 1))
                )

        startDir =
            if startU < 0.5 then
                1.0

            else
                -1.0

        walkSideways zA =
            let
                walked =
                    List.foldl
                        (\i acc ->
                            let
                                dir =
                                    if at flipArr (i - 1) < 0.75 then
                                        -acc.dir

                                    else
                                        acc.dir

                                target =
                                    dir * at ampArr (i - 1) * cp.swingM

                                x =
                                    stepSideways cp.lengthM (at zA (i - 1)) (at zA i) acc.x target
                            in
                            { x = x, dir = dir, xs = acc.xs ++ [ x ] }
                        )
                        { x = 0, dir = startDir, xs = [ 0 ] }
                        (List.range 1 (n - 2))

                lastX =
                    stepSideways cp.lengthM (at zA (n - 2)) (at zA (n - 1)) walked.x (0.4 * walked.x)
            in
            Array.fromList (walked.xs ++ [ lastX ])

        zFor scale =
            Array.fromList (List.map (\c -> scale * c) zShares)

        -- Two rounds: walk sideways, then scale the forward progress so the
        -- polyline through the knots is 0.99 of the course length.
        scale1 =
            forwardScale cp (zFor 0.95) yArr (walkSideways (zFor 0.95))

        scale2 =
            forwardScale cp (zFor scale1) yArr (walkSideways (zFor scale1))

        zArr =
            zFor scale2

        xArr =
            walkSideways zArr

        -- Features on middle knots, at least one knot apart.
        candidates =
            List.range 1 (n - 2)
                |> List.filter
                    (\i ->
                        let
                            s =
                                at sArr i
                        in
                        s >= 0.2 && s <= 0.9
                    )
                |> List.sortBy (\i -> at priorityArr i)

        chosen =
            pickApart candidates (cp.washes + cp.ramps) []

        washIndices =
            List.take cp.washes chosen

        rampIndices =
            List.drop cp.washes chosen

        featureAt i =
            if List.member i washIndices then
                Just (RoughWash { amplitudeM = 0.06 + 0.02 * at featureArr i, wavelengthM = 2.2 + 0.8 * at featureArr i })

            else if List.member i rampIndices then
                Just (LaunchRamp { lengthM = 8.0 + at featureArr i, lipHeightM = 0.5 + 0.05 * at featureArr i })

            else
                Nothing

        halfWidthAt i =
            if i == 0 then
                cp.halfWidthM

            else if i == n - 1 then
                cp.halfWidthM + 0.4

            else
                cp.halfWidthM + at widthArr i
    in
    List.map
        (\i ->
            { sFrac = at sArr i
            , xOffset = at xArr i
            , yDropFrac = at yArr i
            , zProgress = at zArr i
            , halfWidth = halfWidthAt i
            , surface = pickSurface cp.surfaces (at surfaceArr (i // 2))
            , feature = featureAt i
            }
        )
        (List.range 0 (n - 1))


{-| The share of the course length that the forward progress should reach
so that the polyline through the knots (forward, drop and sideways) is 0.99
of the course length, found by bisection between 0.5 and 1.0.
-}
forwardScale : CourseParams -> Array Float -> Array Float -> Array Float -> Float
forwardScale cp zA yA xA =
    let
        count =
            Array.length zA

        lengthAt scale =
            List.range 1 (count - 1)
                |> List.map
                    (\i ->
                        let
                            dz =
                                (at zA i - at zA (i - 1)) / max 1.0e-6 (at zA (count - 1)) * scale * cp.lengthM

                            dy =
                                (at yA i - at yA (i - 1)) * cp.dropM

                            dx =
                                at xA i - at xA (i - 1)
                        in
                        sqrt (dz * dz + dy * dy + dx * dx)
                    )
                |> List.sum

        target =
            0.99 * cp.lengthM

        bisect lo hi steps =
            let
                mid =
                    (lo + hi) / 2
            in
            if steps <= 0 then
                mid

            else if lengthAt mid > target then
                bisect lo mid (steps - 1)

            else
                bisect mid hi (steps - 1)
    in
    bisect 0.5 1.0 24


{-| Move sideways from `prev` towards `target`, never more than 0.55 m per
metre of forward travel between the two progress values.
-}
stepSideways : Float -> Float -> Float -> Float -> Float -> Float
stepSideways lengthM zPrev zNext prev target =
    let
        maxStep =
            0.55 * (zNext - zPrev) * lengthM
    in
    prev + clamp -maxStep maxStep (target - prev)


{-| Greedily take the candidates in order, skipping any next to one already
taken, until `count` are chosen.
-}
pickApart : List Int -> Int -> List Int -> List Int
pickApart candidates count chosen =
    case candidates of
        [] ->
            chosen

        c :: rest ->
            if List.length chosen >= count then
                chosen

            else if List.any (\j -> abs (j - c) <= 1) chosen then
                pickApart rest count chosen

            else
                pickApart rest count (chosen ++ [ c ])


{-| Cumulative shares of the gaps, from 0 to 1 (one more value than gaps).
-}
shareCumulative : List Float -> List Float
shareCumulative gaps =
    let
        total =
            List.sum gaps

        cumulative =
            List.foldl (\g acc -> (g + Maybe.withDefault 0 (List.head acc)) :: acc) [ 0 ] gaps
                |> List.reverse
    in
    List.map
        (\c ->
            if total > 0 then
                c / total

            else
                0
        )
        cumulative


pickSurface : List ( SurfaceType, Float ) -> Float -> SurfaceType
pickSurface options u =
    let
        total =
            List.sum (List.map Tuple.second options)

        target =
            u * total

        go items acc =
            case items of
                [] ->
                    List.head options |> Maybe.map Tuple.first |> Maybe.withDefault DirtRoad

                ( surface, weight ) :: rest ->
                    if target < acc + weight then
                        surface

                    else
                        go rest (acc + weight)
    in
    go options 0


draw : ( Float, Float ) -> Seed -> ( Float, Seed )
draw ( lo, hi ) seed =
    Random.step (Random.float lo hi) seed


randomsIn : Int -> ( Float, Float ) -> Seed -> ( List Float, Seed )
randomsIn count range seed0 =
    let
        step _ ( acc, seed ) =
            let
                ( value, next ) =
                    draw range seed
            in
            ( value :: acc, next )

        ( reversed, seedEnd ) =
            List.foldl step ( [], seed0 ) (List.range 1 count)
    in
    ( List.reverse reversed, seedEnd )


at : Array Float -> Int -> Float
at arr i =
    Array.get i arr |> Maybe.withDefault 0
