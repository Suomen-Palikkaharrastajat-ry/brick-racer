module Racer.GarageFloor exposing
    ( Vertex
    , entities
    )

{-| Dedicated 3D garage workshop environment for the Brick Racer car builder.

Features:

  - Clean polished workshop concrete slab floor.
  - Checkered racing pit stall with alternating charcoal and steel tiles.
  - High-visibility yellow and black diagonal hazard perimeter stripes.
  - Centerline and pit stop tire alignment markings.
  - Rear industrial workshop wall with baseboard kick-plate and
    3D steel structural I-beam columns.
  - Soft multi-ring contact shadow directly beneath the car.
  - Entire environment is offset by `floorY` so car tires sit directly ON TOP
    of the floor surface without sinking into it.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


{-| Vertex with 3D position and RGBA color.
-}
type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


type alias Uniforms =
    { viewMatrix : Mat4
    , projectionMatrix : Mat4
    , modelMatrix : Mat4
    }


vertexShader : Shader Vertex Uniforms { vColor : Vec4 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec4 color;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform mat4 modelMatrix;

        varying vec4 vColor;

        void main() {
            vColor = color;
            gl_Position = projectionMatrix * viewMatrix * (modelMatrix * vec4(position, 1.0));
        }
    |]


fragmentShader : Shader {} Uniforms { vColor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec4 vColor;

        void main() {
            gl_FragColor = vColor;
        }
    |]



-- ── Geometry Helpers ─────────────────────────────────────────────────────────


quad : Vertex -> Vertex -> Vertex -> Vertex -> List ( Vertex, Vertex, Vertex )
quad v1 v2 v3 v4 =
    [ ( v1, v2, v3 )
    , ( v1, v3, v4 )
    ]


horizontalQuad : Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
horizontalQuad y x1 x2 z1 z2 col =
    let
        v1 =
            { position = vec3 x1 y z1, color = col }

        v2 =
            { position = vec3 x2 y z1, color = col }

        v3 =
            { position = vec3 x2 y z2, color = col }

        v4 =
            { position = vec3 x1 y z2, color = col }
    in
    quad v1 v2 v3 v4


verticalQuadZ : Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
verticalQuadZ z x1 x2 y1 y2 col =
    let
        v1 =
            { position = vec3 x1 y1 z, color = col }

        v2 =
            { position = vec3 x2 y1 z, color = col }

        v3 =
            { position = vec3 x2 y2 z, color = col }

        v4 =
            { position = vec3 x1 y2 z, color = col }
    in
    quad v1 v2 v3 v4


verticalQuadX : Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
verticalQuadX x z1 z2 y1 y2 col =
    let
        v1 =
            { position = vec3 x y1 z1, color = col }

        v2 =
            { position = vec3 x y1 z2, color = col }

        v3 =
            { position = vec3 x y2 z2, color = col }

        v4 =
            { position = vec3 x y2 z1, color = col }
    in
    quad v1 v2 v3 v4



-- ── Concrete Floor & Checkered Pit Bay ────────────────────────────────────────


concreteColor : Vec4
concreteColor =
    vec4 0.15 0.17 0.21 1.0


concreteJointColor : Vec4
concreteJointColor =
    vec4 0.08 0.09 0.11 1.0


tileDark : Vec4
tileDark =
    vec4 0.11 0.13 0.16 1.0


tileLight : Vec4
tileLight =
    vec4 0.23 0.26 0.31 1.0


hazardYellow : Vec4
hazardYellow =
    vec4 0.98 0.78 0.04 1.0


hazardBlack : Vec4
hazardBlack =
    vec4 0.07 0.07 0.09 1.0


markingWhite : Vec4
markingWhite =
    vec4 0.92 0.94 0.98 0.88


markingWhiteFaded : Vec4
markingWhiteFaded =
    vec4 0.92 0.94 0.98 0.45


buildFloorTriangles : List ( Vertex, Vertex, Vertex )
buildFloorTriangles =
    let
        floorY =
            0.0

        -- Main concrete base slabs (large surrounding shop floor)
        baseSlabs =
            horizontalQuad floorY -480 480 -480 340 concreteColor

        -- Expansion joint lines
        joints =
            List.concat
                [ horizontalQuad (floorY + 0.01) -480 480 -200 -198 concreteJointColor
                , horizontalQuad (floorY + 0.01) -480 480 0 2 concreteJointColor
                , horizontalQuad (floorY + 0.01) -480 480 180 182 concreteJointColor
                , horizontalQuad (floorY + 0.01) -200 -198 -480 340 concreteJointColor
                , horizontalQuad (floorY + 0.01) 198 200 -480 340 concreteJointColor
                ]

        -- Checkered Pit Stall: 8 columns x 10 rows (X: -100..100, Z: -140..140)
        colWidth =
            25.0

        rowLength =
            28.0

        cols =
            List.range 0 7

        rows =
            List.range 0 9

        tileY =
            floorY + 0.02

        checkerTiles =
            List.concatMap
                (\c ->
                    let
                        x1 =
                            -100.0 + (toFloat c * colWidth)

                        x2 =
                            x1 + colWidth
                    in
                    List.concatMap
                        (\r ->
                            let
                                z1 =
                                    -140.0 + (toFloat r * rowLength)

                                z2 =
                                    z1 + rowLength

                                col =
                                    if modBy 2 (c + r) == 0 then
                                        tileDark

                                    else
                                        tileLight
                            in
                            horizontalQuad tileY x1 x2 z1 z2 col
                        )
                        rows
                )
                cols

        -- Hazard Stripes along Pit Stall Border (width = 12 LDU)
        hazardY =
            floorY + 0.03

        borderWidth =
            12.0

        stripeSize =
            14.0

        -- Top border (Z = -152 to -140, X = -112 to 112)
        topStripes =
            List.range 0 15
                |> List.concatMap
                    (\i ->
                        let
                            x1 =
                                -112.0 + (toFloat i * stripeSize)

                            x2 =
                                min 112.0 (x1 + stripeSize)

                            col =
                                if modBy 2 i == 0 then
                                    hazardYellow

                                else
                                    hazardBlack
                        in
                        horizontalQuad hazardY x1 x2 (-140.0 - borderWidth) -140.0 col
                    )

        -- Bottom border (Z = 140 to 152, X = -112 to 112)
        bottomStripes =
            List.range 0 15
                |> List.concatMap
                    (\i ->
                        let
                            x1 =
                                -112.0 + (toFloat i * stripeSize)

                            x2 =
                                min 112.0 (x1 + stripeSize)

                            col =
                                if modBy 2 i == 0 then
                                    hazardYellow

                                else
                                    hazardBlack
                        in
                        horizontalQuad hazardY x1 x2 140.0 (140.0 + borderWidth) col
                    )

        -- Left border (X = -112 to -100, Z = -140 to 140)
        leftStripes =
            List.range 0 19
                |> List.concatMap
                    (\i ->
                        let
                            z1 =
                                -140.0 + (toFloat i * stripeSize)

                            z2 =
                                min 140.0 (z1 + stripeSize)

                            col =
                                if modBy 2 i == 0 then
                                    hazardYellow

                                else
                                    hazardBlack
                        in
                        horizontalQuad hazardY (-100.0 - borderWidth) -100.0 z1 z2 col
                    )

        -- Right border (X = 100 to 112, Z = -140 to 140)
        rightStripes =
            List.range 0 19
                |> List.concatMap
                    (\i ->
                        let
                            z1 =
                                -140.0 + (toFloat i * stripeSize)

                            z2 =
                                min 140.0 (z1 + stripeSize)

                            col =
                                if modBy 2 i == 0 then
                                    hazardYellow

                                else
                                    hazardBlack
                        in
                        horizontalQuad hazardY 100.0 (100.0 + borderWidth) z1 z2 col
                    )

        -- Center tire alignment guide line and front stop bar
        markingY =
            floorY + 0.04

        markings =
            List.concat
                [ -- Front T-stop bar
                  horizontalQuad markingY -45.0 45.0 -114.0 -108.0 markingWhite
                , -- Center longitudinal alignment line
                  horizontalQuad markingY -2.0 2.0 -108.0 100.0 markingWhiteFaded
                , -- Corner L-brackets
                  horizontalQuad markingY -80.0 -60.0 -125.0 -122.0 markingWhite
                , horizontalQuad markingY 60.0 80.0 -125.0 -122.0 markingWhite
                , horizontalQuad markingY -80.0 -60.0 122.0 125.0 markingWhite
                , horizontalQuad markingY 60.0 80.0 122.0 125.0 markingWhite
                ]
    in
    List.concat
        [ baseSlabs
        , joints
        , checkerTiles
        , topStripes
        , bottomStripes
        , leftStripes
        , rightStripes
        , markings
        ]


floorMesh : Mesh Vertex
floorMesh =
    WebGL.triangles buildFloorTriangles



-- ── Workshop Rear Wall & Structure ────────────────────────────────────────────


baseboardColor : Vec4
baseboardColor =
    vec4 0.14 0.16 0.2 1.0


pillarRed : Vec4
pillarRed =
    vec4 0.84 0.22 0.18 1.0


pillarDarkRed : Vec4
pillarDarkRed =
    vec4 0.52 0.12 0.1 1.0


girderColor : Vec4
girderColor =
    vec4 0.16 0.18 0.23 1.0


buildWallTriangles : List ( Vertex, Vertex, Vertex )
buildWallTriangles =
    let
        wallZ =
            260.0

        minX =
            -420.0

        maxX =
            420.0

        -- Baseboard / Kick-plate (Y = 0 to 22)
        baseboard =
            List.concat
                [ verticalQuadZ wallZ minX maxX 0.0 18.0 baseboardColor
                , verticalQuadZ wallZ minX maxX 18.0 22.0 hazardYellow
                ]

        -- Top heavy industrial overhead girder (Y = 227 to 252)
        topGirder =
            List.concat
                [ verticalQuadZ wallZ minX maxX 227.0 252.0 girderColor
                , verticalQuadZ wallZ minX maxX 238.0 241.0 hazardYellow
                ]

        -- 4 Vertical 3D I-Beam Pillars with protruding front flange
        pillarPositions =
            [ -280.0, -95.0, 95.0, 280.0 ]

        pillarHalfW =
            12.0

        pillarDepth =
            8.0

        frontZ =
            wallZ - pillarDepth

        pillars =
            pillarPositions
                |> List.concatMap
                    (\px ->
                        let
                            x1 =
                                px - pillarHalfW

                            x2 =
                                px + pillarHalfW
                        in
                        List.concat
                            [ -- Front flange face
                              verticalQuadZ frontZ x1 x2 0.0 252.0 pillarRed
                            , -- Left side face
                              verticalQuadX x1 frontZ wallZ 0.0 252.0 pillarDarkRed
                            , -- Right side face
                              verticalQuadX x2 wallZ frontZ 0.0 252.0 pillarDarkRed
                            ]
                    )
    in
    List.concat
        [ baseboard
        , topGirder
        , pillars
        ]


wallMesh : Mesh Vertex
wallMesh =
    WebGL.triangles buildWallTriangles



-- ── Soft Contact Shadow ───────────────────────────────────────────────────────


buildShadowTriangles : List ( Vertex, Vertex, Vertex )
buildShadowTriangles =
    let
        shadowY =
            0.08

        segments =
            24

        step =
            (2.0 * pi) / toFloat segments

        -- 3 Concentric elliptical rings
        r1X =
            45.0

        r1Z =
            65.0

        col1 =
            vec4 0.0 0.0 0.0 0.65

        r2X =
            80.0

        r2Z =
            110.0

        col2 =
            vec4 0.0 0.0 0.0 0.35

        r3X =
            118.0

        r3Z =
            158.0

        col3 =
            vec4 0.0 0.0 0.0 0.0

        center =
            { position = vec3 0.0 shadowY 0.0, color = vec4 0.0 0.0 0.0 0.72 }

        ringPoint rx rz col angle =
            { position = vec3 (rx * cos angle) shadowY (rz * sin angle), color = col }

        buildFan i =
            let
                a1 =
                    toFloat i * step

                a2 =
                    toFloat (i + 1) * step

                p1Inner =
                    ringPoint r1X r1Z col1 a1

                p2Inner =
                    ringPoint r1X r1Z col1 a2

                p1Mid =
                    ringPoint r2X r2Z col2 a1

                p2Mid =
                    ringPoint r2X r2Z col2 a2

                p1Outer =
                    ringPoint r3X r3Z col3 a1

                p2Outer =
                    ringPoint r3X r3Z col3 a2
            in
            List.concat
                [ -- Center triangle
                  [ ( center, p2Inner, p1Inner ) ]
                , -- Inner to mid quad
                  quad p1Inner p2Inner p2Mid p1Mid
                , -- Mid to outer quad
                  quad p1Mid p2Mid p2Outer p1Outer
                ]
    in
    List.concatMap buildFan (List.range 0 (segments - 1))


shadowMesh : Mesh Vertex
shadowMesh =
    WebGL.triangles buildShadowTriangles



-- ── Entities API ─────────────────────────────────────────────────────────────


{-| WebGL entities for the complete garage workshop environment:

  - Checkered floor & pit stall
  - Rear industrial workshop wall & pillars
  - Soft contact shadow under vehicle

Offset by `floorY` so the garage sits directly at the lowest contact point of the car.

-}
entities : Float -> Camera -> Float -> List Entity
entities floorY camera aspect =
    let
        modelMatrix =
            Mat4.makeTranslate (vec3 0.0 floorY 0.0)

        uniforms =
            { viewMatrix = Camera.viewMatrix camera
            , projectionMatrix = Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
            , modelMatrix = modelMatrix
            }

        settings =
            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            ]

        floorEntity =
            WebGL.entityWith settings vertexShader fragmentShader floorMesh uniforms

        wallEntity =
            WebGL.entityWith settings vertexShader fragmentShader wallMesh uniforms

        shadowEntity =
            WebGL.entityWith settings vertexShader fragmentShader shadowMesh uniforms
    in
    [ floorEntity
    , wallEntity
    , shadowEntity
    ]
