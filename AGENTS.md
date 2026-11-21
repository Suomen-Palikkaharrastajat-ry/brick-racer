# AGENTS.md — Brick Racer

3D gravity / soapbox racing game for Brick Frame builds (Elm 0.19.1 + WebGL + Tailwind CSS v4).

## Architecture

- **Ingest & Validation (`src/Racer/Ingest.elm`, `src/Racer/Analyze.elm`):** Decodes `#b/<base64url>` and `#model=...` URL hashes or `.ldr`/`.mpd`/`.io` files, resolves LDraw geometry/catalog weights, pairs wheels & tires, detects turntable/knuckle steering mechanisms, computes mass/CoM/drag, and partitions meshes into chassis, steering bogie, and rotating wheels.
- **Physics (`src/Racer/Physics.elm`):** Fixed-timestep `1/60 s` integration down 3D spline slopes with 4-corner spring-damper suspension, surface-specific tire grip, aero drag, lean weight transfer, obstacle impacts, structural integrity, and checkpoint respawn.
- **Tracks & WebGL (`src/Racer/Track.elm`, `src/Racer/TerrainMesh.elm`, `src/Racer/TerrainShader.elm`, `src/Racer/Render.elm`):** Seeded Catmull-Rom mountain courses blended into chunked heightmap terrain, rendered alongside `Render.Shader` / `Render.EdgeShader` car meshes.
- **UI & Audio (`src/Main.elm`, `src/Racer/HUD.elm`, `src/Racer/Camera.elm`, `audio.js`, `main.js`):** TEA page state machine (`Menu`, `Garage`, `Race`, `Results`), Chase/Hood/Orbit cameras, keyboard/touch/tilt controls, and Web Audio procedural wind/rumble/impact synthesis.

## Commands

Run within `devenv shell --`:

```bash
devenv shell -- make test
devenv shell -- make check
devenv shell -- make build
```
