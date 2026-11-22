# AGENTS.md — Brick Racer

3D gravity / soapbox racing game for Brick Frame builds (Elm 0.19.1 + WebGL + Tailwind CSS v4).

## Architecture

- **Ingest & Validation (`src/Racer/Ingest.elm`, `src/Racer/Analyze.elm`):** Decodes `#b/<base64url>` and `#model=...` URL hashes or `.ldr`/`.mpd`/`.io` files, resolves LDraw geometry/catalog weights, pairs wheels & tires, detects turntable/knuckle steering mechanisms, computes mass/CoM/drag, and partitions meshes into chassis, steering bogie, and rotating wheels.
- **Physics (`src/Racer/Physics.elm`):** Fixed-timestep `1/60 s` integration down 3D spline slopes with 4-corner spring-damper suspension, surface-specific tire grip, aero drag, lean weight transfer, scenery impacts and smashes, structural integrity, and checkpoint respawn.
- **Tracks & WebGL (`src/Racer/Track.elm`, `src/Racer/TerrainMesh.elm`, `src/Racer/TerrainShader.elm`, `src/Racer/Render.elm`):** Three short, steep seeded Catmull-Rom countryside courses on unpaved roads (dirt, gravel, forest track, grass footpath) blended into chunked rolling-field terrain, lined with brick-built scenery (trees, bushes, fences, rocks, signposts, hay bales, cottages, barns) kept in a per-chunk mesh. Scenery shatters into short-lived debris bricks when hit (`ScenerySmashed` event, capped at 72 pieces), rendered alongside `Render.Shader` / `Render.EdgeShader` car meshes.
- **UI & Audio (`src/Main.elm`, `src/Racer/HUD.elm`, `src/Racer/Camera.elm`, `audio.js`, `main.js`):** TEA page state machine (`Menu`, `Garage`, `Race`, `Results`), Chase/Hood/Orbit cameras, keyboard/touch/tilt controls, and Web Audio procedural wind/rumble/impact synthesis.

## Commands

Run within `devenv shell --`:

```bash
devenv shell -- make test
devenv shell -- make check
devenv shell -- make build
```
