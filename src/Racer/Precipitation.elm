module Racer.Precipitation exposing (entities)

{-| Rain and snow falling around the camera on a race.

The streak placement comes from `Game.Precipitation` (the same as Brick
Sails), and what falls and how hard from `Racer.World`. Here the streaks
are drawn: a box of thin vertical quads, scattered at fixed places, moved
down and slanted by the wind of the car's speed.

The streaks fall on the race clock (seconds since the start), so they keep
falling when the car stops and a replay of the same run looks the same.

-}

import Game.MapParams exposing (MapParams)
import Game.Precipitation as Falling
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Racer.World as World exposing (Fall(..))
import Render.Camera as Camera exposing (Camera)
import WebGL
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias Vertex =
    { position : Vec3 }


type alias Uniforms =
    { u_viewProj : Mat4
    , u_model : Mat4
    , u_color : Vec4
    }


{-| The streaks in one box, `boxWidthM` by `Falling.streakHeight` by `boxWidthM`
metres around the origin.
-}
boxWidthM : Float
boxWidthM =
    70


streakCount : Int
streakCount =
    90


streakMesh : WebGL.Mesh Vertex
streakMesh =
    WebGL.triangles (List.concatMap streakTriangles (List.range 0 (streakCount - 1)))


{-| Two triangles for one streak: a thin quad, 0.9 m tall.
-}
streakTriangles : Int -> List ( Vertex, Vertex, Vertex )
streakTriangles i =
    let
        x =
            (pick i 1 - 0.5) * boxWidthM

        y =
            (pick i 2 - 0.5) * Falling.streakHeight

        z =
            (pick i 3 - 0.5) * boxWidthM

        w =
            0.035

        h =
            0.9

        p0 =
            { position = vec3 (x - w) y z }

        p1 =
            { position = vec3 (x + w) y z }

        p2 =
            { position = vec3 (x + w) (y + h) z }

        p3 =
            { position = vec3 (x - w) (y + h) z }
    in
    [ ( p0, p1, p2 ), ( p0, p2, p3 ) ]


pick : Int -> Int -> Float
pick i k =
    fract (sin (toFloat i * 12.9898 + toFloat k * 78.233) * 43758.5453)


fract : Float -> Float
fract x =
    x - toFloat (floor x)


vertexShader : WebGL.Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform mat4 u_viewProj;
        uniform mat4 u_model;

        void main () {
            gl_Position = u_viewProj * u_model * vec4(position, 1.0);
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec4 u_color;

        void main () {
            gl_FragColor = u_color;
        }
    |]


{-| The streaks falling around the camera for a map, the race clock in
seconds (`timeS`) and the car's velocity, or no entities when nothing falls.
-}
entities : Camera -> Int -> Int -> MapParams -> { timeS : Float, velocity : Vec3 } -> List WebGL.Entity
entities camera width height map car =
    case World.precipitation map of
        Nothing ->
            []

        Just { fall, intensity } ->
            let
                field =
                    { eye = Camera.position camera
                    , time = car.timeS
                    , windSpeed = Vec3.length car.velocity
                    , windDirection = windDegrees car.velocity
                    }

                ( colour, alpha ) =
                    case fall of
                        RainFall ->
                            ( vec3 0.8 0.86 0.94, 0.5 )

                        SnowFall ->
                            ( vec3 0.97 0.98 1.0, 0.85 )

                tint =
                    vec4 (Vec3.getX colour) (Vec3.getY colour) (Vec3.getZ colour) (alpha * intensity)

                viewProj =
                    viewProjection camera width height
            in
            Falling.transforms intensity field
                |> List.map
                    (\model ->
                        WebGL.entityWith
                            [ DepthTest.lessOrEqual { write = False, near = 0.0, far = 1.0 }
                            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                            ]
                            vertexShader
                            fragmentShader
                            streakMesh
                            { u_viewProj = viewProj, u_model = model, u_color = tint }
                    )


{-| Direction the air rushes past the car: the reverse of its heading, in
degrees, as `Game.Precipitation` expects.
-}
windDegrees : Vec3 -> Float
windDegrees velocity =
    atan2 (Vec3.getX velocity) (Vec3.getZ velocity) * 180 / pi + 180


viewProjection : Camera -> Int -> Int -> Mat4
viewProjection camera width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)
    in
    Mat4.mul
        (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
        (Camera.viewMatrix camera)
