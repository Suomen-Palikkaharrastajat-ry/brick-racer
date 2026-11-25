# AGENTS.md — Brick Racer

3D gravity / soapbox racing game for Brick Frame builds (Elm 0.19.1 + WebGL + Tailwind CSS v4).

## Architecture

- **Ingest & Validation (`src/Racer/Ingest.elm`, `src/Racer/Analyze.elm`):** Decodes `#b/<base64url>` and `#model=...` URL hashes or `.ldr`/`.mpd`/`.io` files, resolves LDraw geometry/catalog weights, pairs wheels & tires, detects turntable/knuckle steering mechanisms, computes mass/CoM/drag, and partitions meshes into chassis, steering bogie, and rotating wheels.
- **Physics (`src/Racer/Physics.elm`):** Fixed-timestep `1/60 s` integration down 3D spline slopes with 4-corner spring-damper suspension, surface-specific tire grip, aero drag, lean weight transfer, scenery impacts and smashes, structural integrity, and checkpoint respawn.
- **Tracks & WebGL (`src/Racer/Track.elm`, `src/Racer/TerrainMesh.elm`, `src/Racer/TerrainShader.elm`, `src/Racer/Render.elm`):** Short, steep Catmull-Rom courses generated from a map (`Game.MapParams`: biome, season, weather and seed; code `biome-season-weather-seed`, picked in the garage with `Game.MapPicker`, carried in the URL hash as `#map=<code>&b/...`, leaderboards keyed by `Game.MapParams.leaderboardKey`) on unpaved roads (dirt, gravel, forest track, grass footpath) blended into chunked rolling-field terrain, lined with scenery (trees, bushes, fences, rocks, signposts, hay bales, cottages, barns, crates, barrels, garden gnomes, brick walls, brick pallets, tile stacks, brick gates, slope blocks) and start/checkpoint/finish arches built from catalog parts (`Racer.Scenery`: LDraw models from `Game.BrickModels` and, for the crashable crates, barrels, gnomes, walls, pallets, tile stacks, gates and slope blocks, `Racer.SceneryModels`, built once with `Game.BrickProps` and drawn per piece; the collision cylinders stay in `Racer.Track`), plus seeded one-lane brick launch ramps (`Track.brickRampLift`) and rounded quarter-pipe side ramps at the road edges (`Track.sideRampLift`), both added to the physics wheel probes, not the ground heightfield, and built into the chunk ground mesh as smooth slope bricks, and one seeded brick loop-the-loop per course (`Track.loopFrame`; the car rides it off the heightfield, as `PhysicsState.loopRide`, in `Physics.stepOnLoop`). Ground, sky and sun have the shared brick look of `Game.BrickLook` (studded grass plates, tiled roads, a brick sun and brick clouds). Scenery shatters into short-lived debris bricks (catalog 1 x 2 bricks) when hit (`ScenerySmashed` event, capped at 72 pieces), rendered alongside `Render.Shader` / `Render.EdgeShader` car meshes.
- **Car editor (`src/Pages/Build.elm`, `src/Racer/Editor.elm`):** `BuildPage` wraps `Studio.Core` (as Brick Play does) with one fixed dock: 8 colours and 49 parts. A car starts from a starter base (`starters/racer/`, embedded in `Racer.StarterBases`), kept as one grouped submodel: studio edits that would split it are dropped (`Editor.baseIntact`). New parts go on free studs (`Editor.dropMatrix`). "Garage" loads the build through `loadDocumentIntoGarage`; the garage's "Edit car" reopens a build that stands on a starter base.
- **UI & Audio (`src/Main.elm`, `src/Racer/HUD.elm`, `src/Racer/Camera.elm`, `audio.js`, `main.js`):** TEA page state machine (`Menu`, `Garage`, `Race`, `Results`), Chase/Hood/Orbit cameras, keyboard/touch/tilt controls, and Web Audio procedural wind/rumble/impact synthesis.
- **Shared layer (`vendor/packages/brick-frame-game`, `vendor/packages/brick-frame-ui`):** shared with Brick Sails through the `vendor/brick-studio` submodule. `main.js` only calls `bootGame` (`runtime/game-boot.js`) with the racer's storage prefix (`racer`) and synth; `src/Ports.elm` keeps only the racer's audio ports and every other port is in `Game.Ports`. Part fetching (`Game.PartLoading`), leaderboard helpers (`Game.Leaderboard`), multiplayer (`Game.Multiplayer`, `Game.MultiplayerUI`), the base synth (`runtime/audio-engine.js`), the game shell CSS (`game.css`) and the make targets (`Makefile.game`) live there too. Gameplay (physics, tracks, terrain, analysis, scoring) stays in `src/Racer/`.

## Commands

Run within `devenv shell --`:

```bash
devenv shell -- make test
devenv shell -- make check
devenv shell -- make build
```
