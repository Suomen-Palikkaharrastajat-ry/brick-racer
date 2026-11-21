module Racer.TerrainShader exposing
    ( DebrisPiece
    , TerrainVertex
    , buildShadowQuadMesh
    , buildUnitCubeMesh
    , debrisEntity
    , paletteForTrack
    , shadowEntity
    , skyEntity
    , terrainEntity
    )

{-| Custom WebGL vertex/fragment shaders and sky dome for mountain terrain,
track markings, low-poly boulders/pines, car drop-shadow, and brick debris.

Designed to match `Render.Shader`'s crisp daylight aesthetic while adding
horizon distance fog fading smoothly into the sky gradient dome.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Racer.Types exposing (TrackId(..))
import Render.Camera as Camera exposing (Camera)
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


{-| Standard terrain and obstacle vertex attribute record.
-}
type alias TerrainVertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


{-| Ballistic brick particle detached from damaged or loose parts.
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


{-| Select sky, fog, and sun lighting palette matching each mountain course.
-}
paletteForTrack : TrackId -> TrackPalette
paletteForTrack trackId =
    case trackId of
        PineRidge ->
            { zenithColor = vec3 0.24 0.56 0.88
            , horizonColor = vec3 0.78 0.89 0.96
            , fogColor = vec3 0.78 0.89 0.96
            , sunDir = Vec3.normalize (vec3 0.45 0.9 0.35)
            , ambientGround = vec3 0.28 0.33 0.24
            }

        QuarrySwitchbacks ->
            { zenithColor = vec3 0.32 0.52 0.82
            , horizonColor = vec3 0.89 0.84 0.74
            , fogColor = vec3 0.89 0.84 0.74
            , sunDir = Vec3.normalize (vec3 0.55 0.82 0.28)
            , ambientGround = vec3 0.36 0.32 0.27
            }

        AlpineGlacier ->
            { zenithColor = vec3 0.18 0.43 0.78
            , horizonColor = vec3 0.82 0.91 0.98
            , fogColor = vec3 0.82 0.91 0.98
            , sunDir = Vec3.normalize (vec3 -0.35 0.88 0.42)
            , ambientGround = vec3 0.36 0.42 0.5
            }


{-| Render gradient sky dome centered on camera eye with depth-write disabled.
-}
skyEntity : Camera -> TrackId -> Int -> Int -> WebGL.Entity
skyEntity camera trackId width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        eye =
            Camera.position camera

        viewProj =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
                (Camera.viewMatrix camera)

        pal =
            paletteForTrack trackId
    in
    WebGL.entityWith
        [ DepthTest.always { write = False, near = 0.0, far = 1.0 } ]
        skyVertexShader
        skyFragmentShader
        skyDomeMesh
        { u_viewProj = viewProj
        , u_eyePos = eye
        , u_zenith = pal.zenithColor
        , u_horizon = pal.horizonColor
        , u_sunDir = pal.sunDir
        }


{-| Render a compiled terrain/track chunk or scenery mesh with fog and daylight shading.
-}
terrainEntity : Mat4 -> WebGL.Mesh TerrainVertex -> TrackId -> Camera -> Int -> Int -> WebGL.Entity
terrainEntity modelMat mesh trackId camera width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        eye =
            Camera.position camera

        viewProj =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
                (Camera.viewMatrix camera)

        pal =
            paletteForTrack trackId
    in
    WebGL.entityWith
        [ DepthTest.default
        , WebGL.Settings.cullFace WebGL.Settings.back
        ]
        terrainVertexShader
        terrainFragmentShader
        mesh
        { u_model = modelMat
        , u_viewProj = viewProj
        , u_eyePos = eye
        , u_sunDir = pal.sunDir
        , u_fogColor = pal.fogColor
        , u_skyAmbient = pal.horizonColor
        , u_groundAmbient = pal.ambientGround
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


{-| Render one tumbling LEGO debris brick particle.
-}
debrisEntity : WebGL.Mesh TerrainVertex -> TrackId -> Camera -> Int -> Int -> DebrisPiece -> WebGL.Entity
debrisEntity cubeMesh trackId camera width height piece =
    let
        rotMat =
            Mat4.mul
                (Mat4.makeRotate piece.rotation (vec3 1 0.7 0.4))
                (Mat4.makeScale piece.size)

        modelMat =
            Mat4.mul (Mat4.makeTranslate piece.position) rotMat

        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        eye =
            Camera.position camera

        viewProj =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
                (Camera.viewMatrix camera)

        pal =
            paletteForTrack trackId
    in
    WebGL.entityWith
        [ DepthTest.default
        , WebGL.Settings.cullFace WebGL.Settings.back
        ]
        debrisVertexShader
        terrainFragmentShader
        cubeMesh
        { u_model = modelMat
        , u_viewProj = viewProj
        , u_eyePos = eye
        , u_sunDir = pal.sunDir
        , u_fogColor = pal.fogColor
        , u_skyAmbient = pal.horizonColor
        , u_groundAmbient = pal.ambientGround
        , u_tint = piece.color
        }


{-| Unit cube mesh (`[-0.5, 0.5]^3`) shared by tumbling brick debris particles.
-}
buildUnitCubeMesh : WebGL.Mesh TerrainVertex
buildUnitCubeMesh =
    let
        w =
            vec3 1 1 1

        quad p0 p1 p2 p3 n =
            [ ( { position = p0, normal = n, color = w }
              , { position = p1, normal = n, color = w }
              , { position = p2, normal = n, color = w }
              )
            , ( { position = p0, normal = n, color = w }
              , { position = p2, normal = n, color = w }
              , { position = p3, normal = n, color = w }
              )
            ]

        n00 =
            vec3 -0.5 -0.5 0.5

        n10 =
            vec3 0.5 -0.5 0.5

        n11 =
            vec3 0.5 0.5 0.5

        n01 =
            vec3 -0.5 0.5 0.5

        f00 =
            vec3 -0.5 -0.5 -0.5

        f10 =
            vec3 0.5 -0.5 -0.5

        f11 =
            vec3 0.5 0.5 -0.5

        f01 =
            vec3 -0.5 0.5 -0.5
    in
    WebGL.triangles
        (quad n00 n10 n11 n01 (vec3 0 0 1)
            ++ quad f10 f00 f01 f11 (vec3 0 0 -1)
            ++ quad n01 n11 f11 f01 (vec3 0 1 0)
            ++ quad f00 f10 n10 n00 (vec3 0 -1 0)
            ++ quad n10 f10 f11 n11 (vec3 1 0 0)
            ++ quad f00 n00 n01 f01 (vec3 -1 0 0)
        )


{-| Flat disc-approximated dark shadow quad centered at origin (`1 m × 1 m` before scaling).
-}
buildShadowQuadMesh : WebGL.Mesh TerrainVertex
buildShadowQuadMesh =
    let
        segments =
            16

        c =
            { position = vec3 0 0 0, normal = vec3 0 1 0, color = vec3 0 0 0 }

        rimColor =
            vec3 1 1 1

        step i =
            let
                a0 =
                    2.0 * pi * toFloat i / toFloat segments

                a1 =
                    2.0 * pi * toFloat (i + 1) / toFloat segments

                v0 =
                    { position = vec3 (cos a0) 0 (sin a0), normal = vec3 0 1 0, color = rimColor }

                v1 =
                    { position = vec3 (cos a1) 0 (sin a1), normal = vec3 0 1 0, color = rimColor }
            in
            ( c, v1, v0 )
    in
    WebGL.triangles (List.map step (List.range 0 (segments - 1)))



-- ── Sky Dome Geometry ─────────────────────────────────────────────────────────


skyDomeMesh : WebGL.Mesh { position : Vec3 }
skyDomeMesh =
    let
        rings =
            8

        slices =
            16

        pt ring slice =
            let
                phi =
                    pi * (toFloat ring / toFloat rings - 0.5)

                theta =
                    2.0 * pi * toFloat slice / toFloat slices

                cPhi =
                    cos phi
            in
            vec3 (cPhi * cos theta) (sin phi) (cPhi * sin theta)

        cell r s =
            let
                p00 =
                    { position = pt r s }

                p10 =
                    { position = pt (r + 1) s }

                p01 =
                    { position = pt r (s + 1) }

                p11 =
                    { position = pt (r + 1) (s + 1) }
            in
            [ ( p00, p01, p11 ), ( p00, p11, p10 ) ]
    in
    List.range 0 (rings - 1)
        |> List.concatMap (\r -> List.concatMap (cell r) (List.range 0 (slices - 1)))
        |> WebGL.triangles



-- ── GLSL Shaders ──────────────────────────────────────────────────────────────


skyVertexShader : WebGL.Shader { position : Vec3 } { u_viewProj : Mat4, u_eyePos : Vec3, u_zenith : Vec3, u_horizon : Vec3, u_sunDir : Vec3 } { v_dir : Vec3 }
skyVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        uniform mat4 u_viewProj;
        uniform vec3 u_eyePos;
        varying vec3 v_dir;

        void main() {
            v_dir = normalize(position);
            vec3 worldPos = v_dir * 80.0 + u_eyePos;
            gl_Position = u_viewProj * vec4(worldPos, 1.0);
        }
    |]


skyFragmentShader : WebGL.Shader {} { u_viewProj : Mat4, u_eyePos : Vec3, u_zenith : Vec3, u_horizon : Vec3, u_sunDir : Vec3 } { v_dir : Vec3 }
skyFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 u_zenith;
        uniform vec3 u_horizon;
        uniform vec3 u_sunDir;
        varying vec3 v_dir;

        void main() {
            vec3 dir = normalize(v_dir);
            float h = clamp(dir.y, 0.0, 1.0);
            vec3 col = mix(u_horizon, u_zenith, pow(h, 0.65));
            float sunDot = max(dot(dir, normalize(u_sunDir)), 0.0);
            col += vec3(1.0, 0.96, 0.85) * (pow(sunDot, 24.0) * 0.35 + pow(sunDot, 160.0) * 0.7);
            gl_FragColor = vec4(col, 1.0);
        }
    |]


terrainVertexShader :
    WebGL.Shader
        TerrainVertex
        { u_model : Mat4
        , u_viewProj : Mat4
        , u_eyePos : Vec3
        , u_sunDir : Vec3
        , u_fogColor : Vec3
        , u_skyAmbient : Vec3
        , u_groundAmbient : Vec3
        }
        { v_normal : Vec3, v_color : Vec3, v_worldPos : Vec3 }
terrainVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;

        uniform mat4 u_model;
        uniform mat4 u_viewProj;

        varying vec3 v_normal;
        varying vec3 v_color;
        varying vec3 v_worldPos;

        void main() {
            vec4 worldPos = u_model * vec4(position, 1.0);
            v_worldPos = worldPos.xyz;
            v_normal = normalize((u_model * vec4(normal, 0.0)).xyz);
            v_color = color;
            gl_Position = u_viewProj * worldPos;
        }
    |]


debrisVertexShader :
    WebGL.Shader
        TerrainVertex
        { u_model : Mat4
        , u_viewProj : Mat4
        , u_eyePos : Vec3
        , u_sunDir : Vec3
        , u_fogColor : Vec3
        , u_skyAmbient : Vec3
        , u_groundAmbient : Vec3
        , u_tint : Vec3
        }
        { v_normal : Vec3, v_color : Vec3, v_worldPos : Vec3 }
debrisVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;

        uniform mat4 u_model;
        uniform mat4 u_viewProj;
        uniform vec3 u_tint;

        varying vec3 v_normal;
        varying vec3 v_color;
        varying vec3 v_worldPos;

        void main() {
            vec4 worldPos = u_model * vec4(position, 1.0);
            v_worldPos = worldPos.xyz;
            v_normal = normalize((u_model * vec4(normal, 0.0)).xyz);
            v_color = color * u_tint;
            gl_Position = u_viewProj * worldPos;
        }
    |]


terrainFragmentShader :
    WebGL.Shader
        {}
        { u
            | u_sunDir : Vec3
            , u_fogColor : Vec3
            , u_skyAmbient : Vec3
            , u_groundAmbient : Vec3
            , u_eyePos : Vec3
        }
        { v_normal : Vec3, v_color : Vec3, v_worldPos : Vec3 }
terrainFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 u_sunDir;
        uniform vec3 u_fogColor;
        uniform vec3 u_skyAmbient;
        uniform vec3 u_groundAmbient;
        uniform vec3 u_eyePos;

        varying vec3 v_normal;
        varying vec3 v_color;
        varying vec3 v_worldPos;

        void main() {
            float v_dist = length(v_worldPos - u_eyePos);
            vec3 rawN = normalize(v_normal);
            vec3 n = rawN;
            vec3 col = v_color;

            // Surface classification flags
            bool isFlatTop = rawN.y > 0.55;
            bool isRoad = (v_color.r < 0.35 && v_color.g < 0.35 && v_color.b < 0.38)
                       || (v_color.r > 0.82 && v_color.g > 0.72 && v_color.b < 0.38);
            bool isCurb = (v_color.r > 0.70 && v_color.g < 0.28 && v_color.b < 0.28)
                       || (v_color.r > 0.85 && v_color.g > 0.85 && v_color.b > 0.85);
            bool isShoulder = (v_color.r > 0.45 && v_color.r < 0.70 && v_color.g > 0.40 && v_color.g < 0.65 && v_color.b < 0.55);
            bool isSlope = rawN.y < 0.88 && !isRoad && !isCurb;

            float studFade = 1.0 - smoothstep(16.0, 50.0, v_dist);
            float ao = 1.0;
            float isPlastic = 0.85;

            // 1. Brick / Plate Seam Lines
            if (isRoad && !isCurb) {
                vec2 plateUV = fract(v_worldPos.xz / 1.0) - 0.5;
                float distToPlateSeam = (0.5 - max(abs(plateUV.x), abs(plateUV.y))) * 1.0;
                float seamFade = 1.0 - smoothstep(22.0, 65.0, v_dist);
                if (distToPlateSeam < 0.014 && seamFade > 0.0) {
                    float sAlpha = smoothstep(0.002, 0.014, distToPlateSeam);
                    col *= mix(0.70, 1.0, mix(1.0, sAlpha, seamFade));
                }
            } else if (isShoulder) {
                float row = floor(v_worldPos.z / 0.125);
                float shift = mod(row, 2.0) * 0.125;
                vec2 cobbleUV = fract(vec2(v_worldPos.x + shift, v_worldPos.z) / vec2(0.25, 0.125)) - 0.5;
                float distToCobble = min((0.5 - abs(cobbleUV.x)) * 0.25, (0.5 - abs(cobbleUV.y)) * 0.125);
                float cobbleFade = 1.0 - smoothstep(18.0, 55.0, v_dist);
                if (distToCobble < 0.010 && cobbleFade > 0.0) {
                    float cAlpha = smoothstep(0.002, 0.010, distToCobble);
                    col *= mix(0.58, 1.0, mix(1.0, cAlpha, cobbleFade));
                }
                vec2 brickCoord = floor(vec2(v_worldPos.x + shift, v_worldPos.z) / vec2(0.25, 0.125));
                float brickRand = fract(sin(dot(brickCoord, vec2(127.1, 311.7))) * 43758.5453);
                col *= (0.94 + brickRand * 0.12);
            }

            if (isCurb && isFlatTop) {
                float curbSeam = abs(fract(v_worldPos.z / 0.25) - 0.5) * 0.25;
                if (curbSeam < 0.012) {
                    float csAlpha = smoothstep(0.002, 0.012, curbSeam);
                    col *= mix(0.72, 1.0, csAlpha);
                }
            }

            // 2. Procedural LEGO Stud Grid (Cylindrical Highlights / Normals)
            if (isFlatTop && studFade > 0.0) {
                float studPitch = 0.125;
                vec2 uv = v_worldPos.xz / studPitch;
                vec2 cellPos = fract(uv) - 0.5;
                float d = length(cellPos);

                float studR = 0.30;

                if (d > studR && d < 0.39) {
                    float ringShadow = smoothstep(studR, 0.37, d);
                    ao = mix(0.68, 1.0, ringShadow);
                }

                if (d <= studR) {
                    vec2 dir = (d > 0.0001) ? (cellPos / d) : vec2(0.0, 1.0);
                    float rimT = smoothstep(0.18, studR, d);
                    float innerHollow = smoothstep(0.03, 0.0, abs(d - 0.11));

                    vec2 normTilt = dir * (rimT * 1.5 - innerHollow * 0.35);
                    vec3 studNorm = normalize(vec3(
                        rawN.x + normTilt.x,
                        max(0.2, rawN.y - rimT * 0.45),
                        rawN.z + normTilt.y
                    ));

                    n = normalize(mix(rawN, studNorm, studFade));
                    ao = mix(0.94, 1.08, rimT);
                } else {
                    ao = mix(1.0, ao, studFade);
                }
            }

            // 3. Stepped Terracing & Brick Layer Contours on Slopes
            if (isSlope) {
                float plateH = 0.05;
                float brickH = 0.15;

                float fracP = fract(v_worldPos.y / plateH);
                float distP = min(fracP, 1.0 - fracP) * plateH;

                float fracB = fract(v_worldPos.y / brickH);
                float distB = min(fracB, 1.0 - fracB) * brickH;

                float terrFade = 1.0 - smoothstep(30.0, 160.0, v_dist);

                float pGroove = smoothstep(0.002, 0.010, distP);
                float bGroove = smoothstep(0.003, 0.016, distB);
                float seamDark = mix(0.70, 1.0, pGroove * 0.65 + bGroove * 0.35);

                float stepT = smoothstep(0.09, 0.26, fracP);
                vec3 stepN = normalize(mix(vec3(0.0, 1.0, 0.0), rawN, stepT));
                n = normalize(mix(rawN, stepN, terrFade * 0.8));

                float ledgeLight = smoothstep(0.89, 0.96, fracP) * (1.0 - smoothstep(0.96, 1.0, fracP));
                col = col * mix(1.0, seamDark, terrFade) + vec3(0.08) * ledgeLight * terrFade;

                float layerIdx = floor(v_worldPos.y / brickH);
                float layerR = fract(sin(layerIdx * 149.3) * 43758.5453);
                col *= (0.95 + layerR * 0.08);
            }

            // 4. Lighting & Specular Plastic Finish
            vec3 sun = normalize(u_sunDir);
            vec3 viewDir = normalize(u_eyePos - v_worldPos);
            vec3 halfDir = normalize(sun + viewDir);

            float hemi = clamp(n.y * 0.5 + 0.5, 0.0, 1.0);
            vec3 amb = mix(u_groundAmbient, u_skyAmbient, hemi) * 0.44;
            float diff = max(dot(n, sun), 0.0) * 0.72;

            float spec = pow(max(dot(n, halfDir), 0.0), 32.0) * 0.38 * isPlastic;
            vec3 specCol = vec3(1.0, 0.97, 0.90) * spec;

            vec3 lit = (col * (amb + vec3(0.32 + diff))) * ao + specCol;

            float fog = smoothstep(140.0, 620.0, v_dist);
            vec3 finalCol = mix(lit, u_fogColor, fog * 0.92);

            gl_FragColor = vec4(finalCol, 1.0);
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
