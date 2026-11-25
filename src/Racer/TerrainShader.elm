module Racer.TerrainShader exposing
    ( DebrisPiece
    , TerrainVertex
    , buildShadowQuadMesh
    , groundEntity
    , paletteFor
    , propView
    , shadowEntity
    , skyEntity
    , terrainEntity
    )

{-| WebGL shaders for the countryside: the ground in the shared brick look
(`Game.BrickLook`: studded baseplates and tiles), the brick sky, the car's
drop shadow, and the camera, sun and fog the brick-built props are drawn
with (`propView`).

Distance fog fades everything smoothly into the horizon colour of the sky.

-}

import Game.BrickLook as BrickLook
import Game.BrickProps as BrickProps
import Game.MapParams as Map exposing (MapParams)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.World as World
import Render.Camera as Camera exposing (Camera)
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


{-| Ground vertex: position, normal, colour, and `studs`: 1 where the
ground is studded plates (fields, verges), 0 where it is smooth tiles (the
road).
-}
type alias TerrainVertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    , studs : Float
    }


{-| Ballistic brick particle knocked off the racer or out of smashed scenery.

`groundS` is the track arc length near which the piece lands on the terrain
(a negative value means it never lands and just falls).

-}
type alias DebrisPiece =
    { position : Vec3
    , velocity : Vec3
    , rotation : Float
    , spinRate : Float
    , size : Vec3
    , color : Vec3
    , age : Float
    , lifetime : Float
    , groundS : Float
    }


{-| Biome sky/fog/sun palette parameters per track.
-}
type alias TrackPalette =
    { zenithColor : Vec3
    , horizonColor : Vec3
    , fogColor : Vec3
    , sunDir : Vec3
    , ambientGround : Vec3
    }


{-| The sky, fog and sun palette of a map: its biome, with the season's sun
height and the weather's sky colour.
-}
paletteFor : MapParams -> TrackPalette
paletteFor map =
    weatherTint map.weather (seasonTint map.season (biomePalette map.biome))


biomePalette : Map.Biome -> TrackPalette
biomePalette biome =
    case biome of
        Map.Farmland ->
            { zenithColor = vec3 0.26 0.55 0.9
            , horizonColor = vec3 0.83 0.9 0.94
            , fogColor = vec3 0.83 0.9 0.94
            , sunDir = Vec3.normalize (vec3 0.45 0.9 0.35)
            , ambientGround = vec3 0.32 0.38 0.22
            }

        Map.Boreal ->
            { zenithColor = vec3 0.3 0.52 0.78
            , horizonColor = vec3 0.76 0.85 0.8
            , fogColor = vec3 0.72 0.82 0.76
            , sunDir = Vec3.normalize (vec3 -0.4 0.86 0.3)
            , ambientGround = vec3 0.24 0.32 0.2
            }

        Map.Fell ->
            { zenithColor = vec3 0.22 0.5 0.88
            , horizonColor = vec3 0.9 0.92 0.86
            , fogColor = vec3 0.9 0.92 0.86
            , sunDir = Vec3.normalize (vec3 0.3 0.92 0.45)
            , ambientGround = vec3 0.38 0.42 0.24
            }

        Map.Desert ->
            { zenithColor = vec3 0.3 0.55 0.85
            , horizonColor = vec3 0.93 0.86 0.7
            , fogColor = vec3 0.93 0.86 0.7
            , sunDir = Vec3.normalize (vec3 0.2 0.95 0.3)
            , ambientGround = vec3 0.45 0.4 0.26
            }


{-| Autumn and winter suns sit lower in the sky.
-}
seasonTint : Map.Season -> TrackPalette -> TrackPalette
seasonTint season pal =
    let
        lowerSun factor =
            let
                d =
                    Vec3.toRecord pal.sunDir
            in
            { pal | sunDir = Vec3.normalize (vec3 d.x (d.y * factor) d.z) }
    in
    case season of
        Map.Autumn ->
            lowerSun 0.85

        Map.Winter ->
            lowerSun 0.7

        _ ->
            pal


{-| Overcast, rain, storms, fog and snow grey the sky, and fog makes the
horizon the fog colour.
-}
weatherTint : Map.WeatherKind -> TrackPalette -> TrackPalette
weatherTint weather pal =
    let
        greyUp grey amount =
            { pal
                | zenithColor = mixColor pal.zenithColor grey amount
                , horizonColor = mixColor pal.horizonColor grey (amount * 0.5)
            }
    in
    case weather of
        Map.Clear ->
            pal

        Map.Overcast ->
            greyUp (vec3 0.55 0.58 0.62) 0.55

        Map.Rain ->
            greyUp (vec3 0.4 0.43 0.47) 0.6

        Map.Storm ->
            let
                dark =
                    greyUp (vec3 0.25 0.27 0.31) 0.75
            in
            { dark | horizonColor = mixColor dark.horizonColor (vec3 0.4 0.42 0.45) 0.5, fogColor = mixColor dark.fogColor (vec3 0.4 0.42 0.45) 0.5 }

        Map.Fog ->
            let
                greyed =
                    greyUp (vec3 0.7 0.72 0.74) 0.4
            in
            { greyed | horizonColor = greyed.fogColor, fogColor = mixColor greyed.fogColor (vec3 0.82 0.84 0.86) 0.5 }

        Map.Snow ->
            let
                white =
                    greyUp (vec3 0.8 0.84 0.9) 0.5
            in
            { white | horizonColor = mixColor white.horizonColor (vec3 0.92 0.94 0.96) 0.5, fogColor = mixColor white.fogColor (vec3 0.92 0.94 0.96) 0.5 }


mixColor : Vec3 -> Vec3 -> Float -> Vec3
mixColor a b t =
    Vec3.add (Vec3.scale (1 - t) a) (Vec3.scale t b)


{-| Render the sky: the shared brick sky (`Game.BrickLook.skyEntity`) in
the course's colours, with a sun disc of yellow bricks and brick clouds.
-}
skyEntity : Camera -> MapParams -> Int -> Int -> WebGL.Entity
skyEntity camera map width height =
    let
        pal =
            paletteFor map
    in
    BrickLook.skyEntity
        { viewProjection = viewProjection camera width height
        , eye = Camera.position camera
        , zenith = pal.zenithColor
        , horizon = pal.horizonColor
        , sunDir = pal.sunDir
        , time = 0
        }


viewProjection : Camera -> Int -> Int -> Mat4
viewProjection camera width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)
    in
    Mat4.mul
        (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
        (Camera.viewMatrix camera)


{-| The camera, sun and fog of the course for the brick-built props
(`Game.BrickProps`), matching the fog of the ground.
-}
propView : Camera -> MapParams -> Int -> Int -> BrickProps.View
propView camera map width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        pal =
            paletteFor map

        fog =
            World.fogRange map
    in
    { view = Camera.viewMatrix camera
    , projection = Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
    , eye = Camera.position camera
    , sunDir = pal.sunDir
    , fogColor = pal.fogColor
    , fogNear = fog.near
    , fogFar = fog.far
    , fogMax = 0.92
    , width = max 1 width
    , height = max 1 height
    }


{-| Render the garage plinth (or any terrain mesh) with a model matrix in the
brick look of the ground.
-}
terrainEntity : Mat4 -> WebGL.Mesh TerrainVertex -> MapParams -> Camera -> Int -> Int -> WebGL.Entity
terrainEntity modelMat mesh map camera width height =
    surfaceEntity modelMat mesh map camera width height


{-| Render a chunk of countryside ground as LEGO plates: studded green
baseplates for the fields and verges and smooth tiles for the unpaved road
(`Game.BrickLook.surfaceFragmentShader`).
-}
groundEntity : WebGL.Mesh TerrainVertex -> MapParams -> Camera -> Int -> Int -> WebGL.Entity
groundEntity mesh map camera width height =
    surfaceEntity Mat4.identity mesh map camera width height


surfaceEntity : Mat4 -> WebGL.Mesh TerrainVertex -> MapParams -> Camera -> Int -> Int -> WebGL.Entity
surfaceEntity modelMat mesh map camera width height =
    let
        pal =
            paletteFor map

        fog =
            World.fogRange map
    in
    WebGL.entityWith
        [ DepthTest.default
        , WebGL.Settings.cullFace WebGL.Settings.back
        ]
        terrainVertexShader
        BrickLook.surfaceFragmentShader
        mesh
        { u_model = modelMat
        , u_viewProj = viewProjection camera width height
        , u_eyePos = Camera.position camera
        , u_sunDir = pal.sunDir
        , u_fogColor = pal.fogColor
        , u_fogNear = fog.near
        , u_fogFar = fog.far
        , u_fogMax = 0.92
        , u_skyAmbient = pal.horizonColor
        , u_groundAmbient = pal.ambientGround
        , u_studPitch = 0.125
        , u_plateStuds = 16
        , u_gloss = 0.05
        }


{-| Render a soft elliptical ground shadow beneath the soapbox.
-}
shadowEntity : Mat4 -> WebGL.Mesh TerrainVertex -> Camera -> Int -> Int -> Float -> WebGL.Entity
shadowEntity modelMat mesh camera width height heightAboveGround =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        viewProj =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
                (Camera.viewMatrix camera)

        alpha =
            clamp 0.05 0.42 (0.4 - heightAboveGround * 0.14)
    in
    WebGL.entityWith
        [ DepthTest.lessOrEqual { write = False, near = 0.0, far = 1.0 }
        , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
        ]
        shadowVertexShader
        shadowFragmentShader
        mesh
        { u_model = modelMat
        , u_viewProj = viewProj
        , u_alpha = alpha
        }


{-| Flat disc-approximated dark shadow quad centered at origin (`1 m × 1 m` before scaling).
-}
buildShadowQuadMesh : WebGL.Mesh TerrainVertex
buildShadowQuadMesh =
    let
        segments =
            16

        c =
            { position = vec3 0 0 0, normal = vec3 0 1 0, color = vec3 0 0 0, studs = 0 }

        rimColor =
            vec3 1 1 1

        step i =
            let
                a0 =
                    2.0 * pi * toFloat i / toFloat segments

                a1 =
                    2.0 * pi * toFloat (i + 1) / toFloat segments

                v0 =
                    { position = vec3 (cos a0) 0 (sin a0), normal = vec3 0 1 0, color = rimColor, studs = 0 }

                v1 =
                    { position = vec3 (cos a1) 0 (sin a1), normal = vec3 0 1 0, color = rimColor, studs = 0 }
            in
            ( c, v1, v0 )
    in
    WebGL.triangles (List.map step (List.range 0 (segments - 1)))



-- ── GLSL Shaders ──────────────────────────────────────────────────────────────


type alias SurfaceUniforms =
    { u_model : Mat4
    , u_viewProj : Mat4
    , u_eyePos : Vec3
    , u_sunDir : Vec3
    , u_fogColor : Vec3
    , u_fogNear : Float
    , u_fogFar : Float
    , u_fogMax : Float
    , u_skyAmbient : Vec3
    , u_groundAmbient : Vec3
    , u_studPitch : Float
    , u_plateStuds : Float
    , u_gloss : Float
    }


terrainVertexShader :
    WebGL.Shader
        TerrainVertex
        SurfaceUniforms
        BrickLook.SurfaceVaryings
terrainVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        attribute float studs;

        uniform mat4 u_model;
        uniform mat4 u_viewProj;

        varying vec3 v_normal;
        varying vec3 v_color;
        varying vec3 v_worldPos;
        varying float v_studs;

        void main() {
            vec4 worldPos = u_model * vec4(position, 1.0);
            v_worldPos = worldPos.xyz;
            v_normal = normalize((u_model * vec4(normal, 0.0)).xyz);
            v_color = color;
            v_studs = studs;
            gl_Position = u_viewProj * worldPos;
        }
    |]


shadowVertexShader : WebGL.Shader TerrainVertex { u_model : Mat4, u_viewProj : Mat4, u_alpha : Float } { v_edge : Float }
shadowVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;

        uniform mat4 u_model;
        uniform mat4 u_viewProj;

        varying float v_edge;

        void main() {
            v_edge = color.r;
            gl_Position = u_viewProj * (u_model * vec4(position, 1.0));
        }
    |]


shadowFragmentShader : WebGL.Shader {} { u_model : Mat4, u_viewProj : Mat4, u_alpha : Float } { v_edge : Float }
shadowFragmentShader =
    [glsl|
        precision mediump float;

        uniform float u_alpha;
        varying float v_edge;

        void main() {
            float softness = 1.0 - smoothstep(0.15, 1.0, v_edge);
            gl_FragColor = vec4(0.05, 0.06, 0.08, softness * u_alpha);
        }
    |]
