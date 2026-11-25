# Procedural maps for Brick Racer and Brick Sails: implementation plan

Scope: `brick-racer` and `brick-sails` (both on `claude/laughing-einstein-qmwb2i`), plus the shared
`brick-studio` repository that both vendor as the `vendor/brick-studio` submodule (pinned at
`1a0af19` in racer; the submodule is not initialised in either checkout here, the sibling clone
`/home/user/brick-studio` was read instead).

Request: replace the hand-made maps in both games with seeded procedural maps that reuse the
existing map components. Four biomes, four seasons, a set of weather conditions. Few options on the
main screen, plus a way to reuse the seed from the previous game. Old maps do not need to keep
working.

---

## 1. Goals, non-goals, assumptions

### Goals

- One **map code** names every map in both games, for example `boreal-winter-snow-48213`. The code
  sets the biome, season, weather and seed. Generation is pure and deterministic: the same code
  gives the same map on every device and in every multiplayer seat.
- Racer: a seeded course generator replaces the three blueprints (`farmLaneKnots`,
  `forestTrailKnots`, `meadowPathKnots`). Everything downstream of the control points stays: the
  Catmull-Rom spline, resampling, banking, checkpoints, loops, brick and side ramps, scenery,
  chunked terrain, smash debris.
- Sails: a seeded archipelago generator replaces the six fixed islands, the three hand-ordered
  `routes`, the fixed targets, towns, treasure, reefs and wreck, and the fixed arena.
- Biome, season and weather are three separate axes, each with its own parameter record. They
  control palette, scenery weights, terrain, grip (racer), wind and sea (sails), and visibility.
- Main screen: four small pickers (biome, season, weather, each with "Random") and a seed field
  with a dice button. A **Previous map** button reuses the last map played. Results pages get
  **Same map** and **New map**.
- No backwards compatibility. Old hashes, old course ids and old best times are dropped (see §6).

### Non-goals

- No hand-authored "featured" maps or daily seed in this refactor (they could be added later as
  fixed map codes).
- No weather that changes during a racer run. Racer weather is fixed for the run because runs are
  short (about 60 s). Sails keeps its dynamic spells.
- No crosswind force in racer physics. Weather changes racer grip, rolling resistance and
  visibility only.
- No new physics models: no new snow or ice `SurfaceType`. Seasons and weather scale the existing
  per-surface tables.
- No server-side or global leaderboards. Both games keep the local `localStorage` boards.
- No map editor.

### Decisions (my recommendations; the user can override any of them)

| # | Decision | Recommendation | Why |
|---|----------|----------------|-----|
| D1 | Biomes | **Farmland** (fi: Pelto), **Boreal** (Metsä / taiga and lake skerries), **Fell** (Tunturi / alpine-arctic upland), **Tropical** (Trooppinen / pirate coast) | Farmland and Boreal reuse almost all racer scenery (FarmLane/MeadowPath and ForestTrail weights). Tropical reuses all current sails content (palms, fort, reef, wreck, pirate towns). Fell adds steep, sparse, rocky maps for both games and fits the Finnish club. A desert biome would need new props in both games and has no current assets. |
| D2 | Seasons | Spring, Summer, Autumn, Winter in every biome. In Tropical they read as wet/dry variations (no snow) | Keeps the axes orthogonal. The compatibility table (§2.4) handles snow in Tropical. |
| D3 | Weather set | `Clear`, `Overcast`, `Rain`, `Storm`, `Fog`, `Snow`. All six apply to both games | Sails already has squall/thunderstorm/lull spells; racer gets static versions of the same six. |
| D4 | Map code | `<biome>-<season>-<weather>-<seed>`, lower case, seed `1..999999` (the current racer clamp in `loadDocumentIntoGarage`) | Readable, works in URL hashes (no `&`/`=`/`/`), can be typed, and **works in both games**, so a code can be carried from racer to sails and back. |
| D5 | "Random" picks | Resolved to concrete values **when the game starts**, derived from the seed, so the stored code is always concrete | Codes are always shareable and reproducible. A leaderboard never holds a "random" key. |
| D6 | Where shared code lives | New pure module `Game.MapParams` (types, codec, compatibility, seeded helpers) and `Game.MapPicker` (picker view with labels passed in) in `brick-studio/packages/brick-frame-game`. Game-specific tables stay in each game. | Both games already take `Game.*` from there (`Game.BrickLook`, `Game.Leaderboard`, `Game.Multiplayer`). See D7 for the fallback. |
| D7 | Fallback if brick-studio cannot change in step | Copy `MapParams` into `src/Racer/Map.elm` and `src/Sails/Map.elm` with identical code and a test-checked codec. Move it to brick-studio later. | Lets the game phases ship without a submodule bump. |
| D8 | Persistence of the current/previous map | **Both games: the URL hash** (`#map=<code>&b/...`), plus `lastMap` in memory. Sails also reads the most recent leaderboard entry's `courseId` at boot. | Racer already round-trips `#track=&seed=` through `Ingest.encodeRaceHash`. Sails already puts `#b/` and `#room=` in the hash. `game-runtime.js` (brick-studio) only stores the leaderboard, mute and language keys. A new `localStorage` key would need a brick-studio port change; the leaderboard entry already has `courseId` and `isoDate`. |
| D9 | Leaderboard key | `"g1:" ++ mapCode`. `g1` is the generator version; bump it whenever generator output changes. | Weather and season change grip and wind, so times are only comparable on the exact same code. The version stops stale records surviving a generator change. |
| D10 | Old data | Drop it. No namespacing of old records. | The user asked for no backwards compatibility (§6). |
| D11 | Multiplayer map sync | Small brick-studio change: `multiplayer.js` keeps the `courseId` it already receives (it currently ignores it) and broadcasts it; `Game.Multiplayer.RoomState` gets `courseId : String` (optional field, default `""`). | Right now joiners in both games race whatever map they had selected. That gets worse once there are a million maps. |
| D12 | Racer weather over time | Static per run, with visual-only animation (rain streaks, snowfall, lightning flash in Storm) | Fair timing over a short run, and simpler physics. |

---

## 2. Shared model

### 2.1 Module layout

```
brick-studio/packages/brick-frame-game/src/Game/
  MapParams.elm     -- Biome, Season, WeatherKind, MapParams, codec, compatibility, resolve, seeded rng helpers
  MapPicker.elm     -- view : Labels -> Config msg -> Selection -> Html msg  (chips + seed input + dice + previous)
  Precipitation.elm -- rain / snow streak entity, lifted from Sails.Render.rainEntities (see §3.6)
```

Only the types, codec and compatibility rules are shared. Each game keeps its own tables that
turn the params into gameplay numbers:

```
brick-racer/src/Racer/World.elm   -- MapParams -> RacerWorld (palette, scenery theme, grip, fog, course shape ranges)
brick-sails/src/Sails/World.elm   -- MapParams -> SailsWorld (palette, island style, wind, weather profile, fog)
```

### 2.2 Type sketches (`Game.MapParams`)

```elm
type Biome = Farmland | Boreal | Fell | Tropical
type Season = Spring | Summer | Autumn | Winter
type WeatherKind = Clear | Overcast | Rain | Storm | Fog | Snow

type alias MapParams =
    { biome : Biome
    , season : Season
    , weather : WeatherKind
    , seed : Int              -- 1..999999
    }

-- What the player picked on the main screen, before resolve.
type Pick a = Any | Exactly a

type alias Selection =
    { biome : Pick Biome
    , season : Pick Season
    , weather : Pick WeatherKind
    , seed : Int
    }

allBiomes : List Biome
allSeasons : List Season
allWeathers : List WeatherKind

-- Codec (map code)
toCode : MapParams -> String          -- "boreal-winter-snow-48213"
fromCode : String -> Maybe MapParams  -- trims, lower-cases, checks compatibility, clamps seed
biomeToString / biomeFromString, seasonToString / ..., weatherToString / ...

-- Resolve picks. "Any" choices come from the seed through a hash with fixed salts,
-- so the same Selection always gives the same MapParams.
resolve : Selection -> MapParams

-- Compatibility (see 2.4). Coerces an incompatible weather to the closest allowed one.
allowedWeathers : Biome -> Season -> List WeatherKind
coerce : MapParams -> MapParams

-- Seeds for each sub-generator, so adding a stream does not shift the others.
subSeed : String -> MapParams -> Random.Seed   -- e.g. subSeed "course" p, subSeed "scenery" p
leaderboardKey : MapParams -> String           -- "g1:" ++ toCode p
generatorVersion : String                      -- "g1"
randomSeed : Int -> Int                        -- next dice value (moved from Main.RandomSeed's LCG)
```

`subSeed` hashes `name ++ toCode p` with the same string fold `Sails.Weather.seedFrom` already
uses, into `Random.initialSeed`. Racer already uses `elm/random` (`Random.initialSeed (config.seed
* 7919 + 13)` in `buildControlPoints`), so the generators use `elm/random`. The Park-Miller stepper
in `Sails.Weather` stays as it is; it only needs a seed `Int`.

### 2.3 Axis parameter records (one record per axis in each game's `World` module)

Each axis has its own record, and the game combines them in one function. The axes stay
independent, and each table is small enough to tune by hand.

```elm
-- Racer.World (sketch)
type alias BiomeParams =
    { courseShape : { lengthM : ( Float, Float ), dropM : ( Float, Float ), halfWidthM : ( Float, Float )
                    , lateralSwingM : Float, knotCount : ( Int, Int ) }
    , surfaces : List ( SurfaceType, Float )          -- weights for knot surfaces
    , features : { washes : ( Int, Int ), launchRamps : ( Int, Int ) }
    , scenery : SceneryTheme                          -- the existing record: fenceChance, density, weights
    , ground : GroundPalette                          -- verge / near / far LDraw colours (today's fieldColors)
    , sky : SkyPalette                                -- zenith, horizon, fog, sunDir, ambientGround (today's TrackPalette)
    , undulation : Float                              -- amplitude scale for fieldUndulation
    }

type alias SeasonParams =
    { groundTint : GroundPalette -> GroundPalette     -- e.g. Winter -> white/light-grey field, Autumn -> tan/orange
    , skyTint : SkyPalette -> SkyPalette
    , sceneryWeightScale : ObstacleKind -> Float      -- Winter: HayBale 0, LeafyTree 0.4, PineTree 1.4
    , densityScale : Float
    , undulationScale : Float                         -- Winter 0.8 (snow smooths the fields)
    , grip : Float                                    -- Spring 0.95, Summer 1.0, Autumn 0.93, Winter 0.80
    , rollingResist : Float                           -- Spring 1.15 (mud), Winter 1.25 (snow)
    }

type alias WeatherParams =
    { skyTint : SkyPalette -> SkyPalette              -- Overcast/Storm: grey zenith, Fog: horizon == fog colour
    , fogNearM : Float, fogFarM : Float               -- Clear 140/620 (today's values), Fog 35/170, Storm 80/320, Snow 70/300
    , precipitation : Maybe { kind : Precip, intensity : Float }   -- Rain 0.6, Storm 1.0, Snow 0.7
    , grip : Float                                    -- Clear/Overcast 1.0, Fog 0.97, Rain 0.86, Storm 0.80, Snow 0.78
    , rollingResist : Float
    , lightning : Bool                                -- Storm only
    , sunStrength : Float                             -- light/ambient on Style (Render.carStyleForTrack)
    }

type alias RacerWorld =
    { params : MapParams, shape : ..., scenery : SceneryTheme, ground : GroundPalette
    , sky : SkyPalette, fog : { near : Float, far : Float }, precipitation : ..., env : Environment }

type alias Environment = { gripScale : Float, rollingResistScale : Float }   -- product of season x weather, clamped to [0.6, 1.0] / [1.0, 1.5]

world : MapParams -> RacerWorld
```

Sails has the same three records, but with island style, wind and a `Sails.Weather.Profile`
instead of grip (§4.3).

### 2.4 Compatibility table (in `Game.MapParams.allowedWeathers`)

| Biome \ Season | Spring | Summer | Autumn | Winter |
|---|---|---|---|---|
| Farmland | Clear, Overcast, Rain, Fog | Clear, Overcast, Rain, Storm | Clear, Overcast, Rain, Storm, Fog | Clear, Overcast, Fog, Snow |
| Boreal | Clear, Overcast, Rain, Fog | Clear, Overcast, Rain, Storm | Overcast, Rain, Storm, Fog | Clear, Overcast, Fog, Snow, Storm (blizzard look) |
| Fell | Clear, Overcast, Fog, Snow | Clear, Overcast, Rain, Fog | Overcast, Rain, Fog, Snow | Clear, Overcast, Snow, Storm |
| Tropical | Clear, Rain, Storm | Clear, Overcast, Storm | Clear, Rain, Storm, Overcast | Clear, Overcast, Fog |

`coerce` maps a disallowed choice to the nearest allowed one: Snow -> Rain -> Overcast -> Clear,
Storm -> Rain, Fog -> Overcast. The picker greys out weather chips that the chosen biome and
season do not allow. `fromCode` coerces too, so an edited URL cannot produce an impossible map.

---

## 3. Racer refactor

### 3.1 Types (`src/Racer/Types.elm`)

- Delete `TrackId`, `allTrackIds`, `trackIdToString`, `trackIdFromString` (lines 103-159),
  including the retired mountain-id aliases.
- `TrackConfig` becomes:
  ```elm
  type alias TrackConfig =
      { map : MapParams
      , seed : Int                 -- kept: = map.seed, read by debris and scenery seeds in Main
      , lengthM : Float
      , dropM : Float
      , defaultHalfWidthM : Float
      , env : Environment          -- grip / rolling-resistance scales, read by Physics
      }
  ```
  `GeneratedTrack.config` already carries the config into physics and rendering. Putting `env`
  there means no physics function signatures change.
- `LeaderboardEntry.trackId : TrackId` becomes `mapCode : String`. `seed` is dropped because it is
  inside the code.
- Keep `SurfaceType` and `SegmentFeature` unchanged.

### 3.2 Course generator (`src/Racer/Track.elm`, new section or new `src/Racer/Course.elm`)

The generator replaces the blueprints with **seeded blueprint knots** in the same `BlueprintKnot`
form (`sFrac, xOffset, yDropFrac, zProgress, halfWidth, surface, feature`). The existing jitter
fold in `buildControlPoints` and everything after it run unchanged. This is the smallest change
that keeps the current geometry and feel, which `TrackTest` and `PlayFeelTest` check.

`trackConfigFor : MapParams -> TrackConfig` picks `lengthM`, `dropM` and `defaultHalfWidthM` from
the biome ranges with `subSeed "shape"`. The current presets give the starting ranges:

| Biome | lengthM | dropM | halfWidthM | lateral swing | notes |
|---|---|---|---|---|---|
| Farmland | 880-1000 | 280-320 | 4.2-4.6 | ±56 m | wide, gentle sweeps (FarmLane) |
| Boreal | 980-1100 | 320-360 | 3.8-4.1 | ±36 m | tighter, more washes (ForestTrail) |
| Fell | 820-960 | 330-380 | 3.3-3.8 | ±30 m | narrow and steepest (MeadowPath+); rock-heavy |
| Tropical | 900-1040 | 290-330 | 3.9-4.4 | ±48 m | sandy GravelRoad/DirtRoad mix |

`generateKnots : MapParams -> TrackConfig -> List BlueprintKnot`:

1. `n = knotCount` (8 or 9, as in the blueprints). `sFrac` and `zProgress` are spread evenly with
   ±0.02 jitter. The first two knots and the last knot are pinned like today: straight run-in at
   `xOffset 0`, slightly wider finish (`halfWidth + 0.4`), `zProgress 0.94..0.96` at the end.
2. **Lateral walk**: alternate the sign of the bend with a probability of 0.75. The bend is drawn
   from `[0.45, 1.0] * lateralSwing`. Clamp `|Δx| / Δz ≤ 0.55` between neighbouring knots, so the
   spline never doubles back. Monotonic `zProgress` already guarantees the course cannot cross
   itself or come back within `recoveryCorridorM` of an earlier leg.
3. **Drop profile**: segment grades are drawn so the cumulative `yDropFrac` is monotonic and ends
   at 1.0. Each segment's share is in `[0.6, 1.5] × average`. The first segment is capped at 0.6×
   (start apron), and one random middle segment is marked "steepest pitch" at 1.3-1.5×, like the
   farm-lane design.
4. **Surfaces** come from the biome weights with runs of 2-3 knots, so there is no flicker
   between knots.
5. **Features**: put `washes` RoughWash knots and `launchRamps` LaunchRamp knots on middle knots
   (`sFrac` 0.3-0.85), never next to each other. Amplitudes come from today's ranges
   (0.06-0.08 m, wavelength 2.2-3.0; lip 0.5-0.55 m, length 8-9 m). Spring and Rain add one wash
   (mud ruts).
6. Delete `farmLaneKnots`, `forestTrailKnots`, `meadowPathKnots`. `buildControlPoints` takes the
   generated knots instead of `case config.id`.

**Validation and retry** (deterministic): `generate` becomes
`generateValid : TrackConfig -> GeneratedTrack`. It tries attempts `0..7`, each with
`subSeed ("course/" ++ String.fromInt attempt)`. It runs the **cheap checks first**, on the
samples before scenery:

- total length within ±8 % of `lengthM`; average grade 25-40 %; no 20 m window steeper than 65 %;
- minimum horizontal curvature radius ≥ 18 m (from `computeCurvatureAndBanking`);
- clearance between legs: any two samples with `|Δs| > 80 m` are ≥ `2 * (halfWidth + recoveryCorridorM)` apart in xz;
- `placeLoops` returns exactly one loop (the tests expect one loop per course); `placeBrickRamps` ≥ 1;
- `buildCheckpoints` gives ≥ 4 gates with strictly increasing `s`, and the finish sits at `finishS`.

After the first passing attempt it runs `generateScenery` once. If all 8 attempts fail, it falls
back to the best-scoring attempt and the tests (§7) ensure this never happens in the seed sweep.

### 3.3 Reuse list (racer)

| Keep as is | Change signature or input | Delete | New |
|---|---|---|---|
| `evaluateCatmullRomSeries`, `catmullRomVec3`, `resampleUniform`, `accumulateArcLength`, `findBracketSample`, `computeCurvatureAndBanking`, `extractFeatureZones`, `buildCheckpoints`, `placeLoops`, `loopFrame`, `loopDrift`, `loopLaneHalfWidth`, `placeBrickRamps`, `brickRampLift`, `placeSideRamps`, `sideRampLift`, `sideRampProfile`, `bucketObstacles`, `removeObstacles`, all `sampleAt*`/projection helpers, `scenerySpec`, `pickWeighted`, `dockBuiltScenery`, `Racer.Scenery`, `Racer.SceneryModels`, `Game.BrickProps`, debris (`spawnSceneryDebris`, `sceneryDebrisStyle`), `TerrainMesh.buildChunk` / `rebuildChunkScenery` / ramp and loop tris | `trackConfigFor` (MapParams), `buildControlPoints` (knots argument), `generate` -> `generateValid`, `sceneryTheme` (-> `World.sceneryTheme : MapParams -> SceneryTheme`, biome table × season scales), `fieldUndulation` (extra amplitude argument from biome × season), `TerrainShader.paletteForTrack` -> `World.skyPalette`, `skyEntity`/`propView`/`terrainEntity`/`groundEntity`/`surfaceEntity` (take `RacerWorld` instead of `TrackId`; fog near/far from the world instead of the hard-coded 140/620 at TerrainShader lines 153-154 and 196-197), `TerrainMesh.fieldColors`/`bandColor`/`stitchCrossSections` (GroundPalette), `TerrainMesh.roadColors` (season tint: snow on the verge bands in Winter), `Render.carStyleForTrack` -> `carStyleFor RacerWorld`, `Physics.surfaceGripMultiplier` call site (Physics.elm ~line 444: `* track.config.env.gripScale`), `surfaceRollingResist` call site (`* env.rollingResistScale`) | `TrackId` and its helpers, three knot blueprints, the `case trackId` branches in Track, TerrainShader, TerrainMesh, Render, I18n `trackTitle`/`trackDescription`, `leaderboard-migration.js` + `leaderboard-migration.test.mjs` + its call in `main.js` | `generateKnots`, `validateCourse`, `Racer.World` tables, weather overlay entities (`Game.Precipitation`), course preview SVG made from the samples (replaces the per-track preview art in `Pages/Garage.viewCourseSelection`) |

### 3.4 Biome -> scenery

Start from the current themes so the look stays the same:

- **Farmland** = today's FarmLane weights (`HayBale`, `Barn`, `Cottage`, fences 0.06).
- **Boreal** = today's ForestTrail weights (PineTree 38, Rock 12, density 0.34, fences 0.015).
- **Fell** = new: Rock 30, PineTree 10 (scrub), Bush 8, Chalet 6, Signpost 6, BrickWall 4,
  SlopeBlock 4, CountdownBoard 3, no HayBale or Barn, density 0.14, fences 0.02.
- **Tropical** = new: `PalmTree` (new ObstacleKind; see below), Bush 18, Rock 10, Crate 10,
  Barrel 10, Cottage 4, Signpost 4, no PineTree or HayBale, density 0.22.

The brick-built kinds (Crate, Barrel, GardenGnome, BrickWall, BrickPallet, TileStack, BrickGate,
SlopeBlock, CountdownBoard, Chalet, PitGarage) stay in every biome at today's low weights. They
are the smashable game props, not the biome dressing.

`PalmTree`: reuse the sails palm. `Sails.Scenery.palmTree` is an LDraw string. Move it to
`Game.BrickModels.palmTree` (brick-studio), then add a `PalmTree` branch to `scenerySpec`,
`Racer.Scenery` and `sceneryDebrisStyle`. Fallback if brick-studio cannot change: copy the string
into `Racer.SceneryModels`.

Season colour variants for trees (bare or orange `LeafyTree` in Autumn, snow-capped `PineTree` in
Winter) need colour-parameterised models: `BrickModels.leafyTreeIn : { leaves : Int }` and
`pineTreeIn : { needles : Int, cap : Maybe Int }`. That is a brick-studio change, so I put it in
**P5 polish**. Until then, seasons change only the weights (Winter: LeafyTree × 0.4, HayBale × 0,
PineTree × 1.4; Autumn: HayBale × 1.5) and the ground colours.

### 3.5 Season and weather in racer

- **Ground palette** (`fieldColors` today, by LDraw code): Summer = today's biome colours. Spring:
  bright green 10 near, green 2 far, with light-green 326 strips in Farmland. Autumn: tan 19 and
  dark orange 484 strips, verge dark tan 28. Winter: white 15 near and far, verge light bluish grey
  71, roads with white edge bands (`roadColors` edge = 15). Tropical: tan 19 verge and dark tan 28
  sand strips in every season. Its "Winter" is a dry season with yellowed grass (yellowish green
  326).
- **Sky**: season tints the sun direction height and the zenith (lower sun in Autumn and Winter).
  Weather tints overcast grey (`zenith` mixed 60 % toward `vec3 0.55 0.58 0.62`). Fog sets
  horizon = fog colour. Storm darkens like sails' `gloom`. The shared `BrickLook.skyEntity`
  already takes zenith, horizon and sun. Cloud cover needs a `SkyParams.cloudCover` field in
  brick-studio; that is optional (P5), and overcast reads well enough from colour alone.
- **Fog**: `WeatherParams.fogNearM` and `fogFarM` go into `propView` and the terrain uniforms.
  `Camera.farPlane` stays. Fog hides chunks that are not drawn, so short fog may let chunk culling
  draw fewer chunks (optional optimisation).
- **Precipitation**: `Game.Precipitation.entity` (lifted from `Sails.Render.rainEntities`, which
  already draws two layers of wind-slanted streaks around the camera). Mode `Rain` or `Snow`
  (slower, white, short). Racer passes the car's velocity as "wind" so streaks slant at speed.
- **Lightning** in Storm: a flash uniform in the sky and props (the sails `flash` term,
  `OceanShader.flashLight`). Its timing is seeded from `subSeed "weather"` and the race clock,
  which keeps replays and ghosts deterministic.
- **Grip**: `Environment.gripScale = season.grip × weather.grip`, clamped to `[0.6, 1.0]`. It is
  applied to the one `surfaceGrip` binding in `Physics.step` (Physics.elm line 444), which every
  tyre-force term uses (lines 571-591). Rolling resistance scales the same way. The neutral
  environment (1.0 / 1.0) is the default in tests, so existing physics tests do not change.
- **HUD**: add a small weather and season badge next to the course title in `Racer.HUD`.

### 3.6 Racer file touch estimate

| File | Change |
|---|---|
| `src/Racer/Types.elm` | remove TrackId; TrackConfig, LeaderboardEntry |
| `src/Racer/Track.elm` | generator, validation, theme moves out; ~ -120 / +350 lines |
| `src/Racer/World.elm` (new) | biome/season/weather tables, `world`; ~300 lines |
| `src/Racer/TerrainShader.elm` | palette/fog from world |
| `src/Racer/TerrainMesh.elm` | ground/road palette from world (`TrackRenderBundle.trackId` -> `mapCode`) |
| `src/Racer/Render.elm` | car style, precipitation and flash entities |
| `src/Racer/Physics.elm` | two multipliers |
| `src/Racer/Scenery.elm`, `SceneryModels.elm` | PalmTree |
| `src/Racer/Ingest.elm` | `#map=` hash (encode, parse, strip in `hashWithoutTrackParams`) |
| `src/Racer/Leaderboard.elm` | key by `leaderboardKey`, strict decoder, cap |
| `src/Racer/I18n.elm` | biome/season/weather labels; remove track titles |
| `src/Racer/HUD.elm` | map badge |
| `src/Main.elm` | `trackId/seed` -> `selection`, `map`, `lastMap`; msgs; ensureTrackBuilt cache key |
| `src/Pages/Garage.elm` | `viewCourseSelection` -> `MapPicker` + preview |
| `src/Pages/Results.elm`, `src/Pages/Race.elm` | Same map / New map; title |
| `main.js`, `leaderboard-migration*.js` | delete migration |
| tests: `TrackTest`, `IngestTest`, `Stage5Test`, `SceneryModelsTest`, `PlayFeelTest`, `PhysicsTest`, `EditorTest` | replace TrackId fixtures with fixed map codes; new generator tests |
| `docs/brick-racer-design.md` §5, `AGENTS.md`, `README.md` | describe procedural maps |

---

## 4. Sails refactor

### 4.1 Types (`src/Sails/Types.elm`)

```elm
type IslandRole = Hub | HarbourIsle | TreasureIsle | Skerry

type alias Island =
    { x : Float, z : Float, radius : Float, height : Float
    , role : IslandRole                 -- replaces "island 0 is the lighthouse island" in Render
    }

type alias Decor = { kind : DecorKind, x : Float, z : Float, yaw : Float }
type DecorKind = Reef | Wreck

type alias Course =
    { id : String                       -- = leaderboardKey map (used by Leaderboard.bestForCourse)
    , map : MapParams
    , title : String                    -- generated, e.g. "Boreal winter · 48213"
    , description : String
    , waypoints : List Waypoint
    , targets : List Target
    , islands : List Island
    , towns : List Town
    , treasures : List Treasure
    , decor : List Decor                -- replaces the fixed reefs and wreck in Render.reefEntities
    , wind : Wind
    , arena : { x : Float, z : Float, radius : Float }   -- replaces Physics.arenaCenter / arenaRadius
    , startPosition : Vec3
    , startHeading : Float
    , targetSeconds : Float
    }
```

`Town`, `Treasure`, `Target` and `Waypoint` stay unchanged. Adding `role` to `Island` changes
`Island` equality, which `Render.dockIsland` and the town matching (`town.island == isl`) rely on.
This still works because the whole record is copied, but tests should cover it.

### 4.2 Archipelago generator (new `src/Sails/Archipelago.elm`)

```elm
type alias Archipelago =
    { map : MapParams
    , islands : List Island
    , hub : Island
    , harbourIsles : List Island          -- 4, in ring order
    , routes : List Route                 -- 1..3 valid harbour orders, seat i uses routes[i mod n]
    , towns : List Town
    , decor : List Decor
    , wind : Wind
    , arena : { x : Float, z : Float, radius : Float }
    }

type alias Route =   -- same shape as today's Physics.Route
    { harbours : List ( Island, Float ), startHeading : Float, treasureBearing : Float
    , treasure : Island, targets : List ( Float, Float ), lengthM : Float }

generate : MapParams -> Archipelago
courseFor : Archipelago -> Int -> Course     -- replaces Physics.courseForSeat
```

Algorithm. It is deterministic, uses `subSeed "archipelago"`, and keeps today's distances
(routes of 921-993 m):

1. **Hub** (lighthouse/fort island, role `Hub`): radius 20-26, placed 90-110 m from the start
   along a seeded bearing `θ0`. The start stays at the origin, as today.
2. **Ring**: 4 harbour isles at angles `θ0 + 90° k + jitter(±25°)` around the hub, at distance
   `R_k ∈ [120, 220]`, radius 35-50 (biome scale; see §4.4), height 7-12.
3. **Spacing check**: island-to-island gap ≥ 30 m (so a boat fits between them with 8 m
   clearance each side and room to turn). On failure, push the islands outward along their ring
   ray in 10 m steps, up to 5 steps.
4. **Docks**: each harbour isle docks on the shore facing the ring centroid ± 35° (seeded), using
   the existing `harbour i kind island bearing`, `harbourOffshore` 3 m and `harbourRadius` 10 m.
   The final harbour is on the hub's shore facing the start, like `harbour n FinalHarbour
   lighthouseIsland 215` today.
5. **Routes**: candidate orders are the ring order clockwise and counter-clockwise, with the hub
   call inserted at position 0, 1 or 2 (today's three routes follow this pattern). Each candidate
   docks the hub on the shore facing its incoming leg. A candidate is valid when:
   - every leg (start -> h0 -> ... -> final) is ≥ `radius + 8 m` from every island, except the
     12 m into and out of a harbour on its own island (the rule written above `routes` in
     Physics.elm);
   - the turn at each harbour is ≤ 150° (no swinging back onto the rocks);
   - the total length is within 850-1050 m.
   Keep up to 3 valid candidates in a fixed order of preference. At least one is required.
6. **Repair loop**: if no candidate is valid, rotate dock bearings ±15°, then shrink the offending
   island's radius by 10 % (down to a minimum of 30), then retry with `attempt + 1` in the sub-seed.
   At most 12 attempts. The seed sweep test makes sure the fallback is never reached.
7. **Treasure isle** (radius 8-12, role `TreasureIsle`): beside the middle leg of route 0, offset
   25-40 m to one side, with the same clearance check against every leg of every kept route.
   `treasureBearing` faces that leg, like `route.treasureBearing` today. Seat routes that do not
   pass near it still get the same chest. It is a detour, the same as today for routes 1 and 2.
8. **Targets**: 7 per route at leg fractions 0.35-0.65, offset 12-20 m to the side away from the
   nearest island, at least `radius + 6 m` from every island.
9. **Towns**: one town on each harbour isle, at the dock bearing ± 40-70°. Names come from a biome
   name list in `Sails.I18n` (Tropical keeps "Tortuga Cove", "Port Royal Brick"..., Boreal uses
   Finnish archipelago-style names, and so on). The hub always gets the fort (`Render` currently
   draws `fort` for island index 0; it will check `role == Hub` instead).
10. **Skerries and decor**: 2-5 `Skerry` islands (radius 6-14) outside `courseFreeRange` + 10 m
    of every leg, inside the arena. 2-4 `Reef` decor items and 0-1 `Wreck` (Tropical and Boreal),
    with the same exclusion.
11. **Arena**: centre = centroid of every route point; radius = max distance + 60 m (today 240 m
    around (-7, 95)). `Physics.keepInArena` reads `course.arena`.
12. **targetSeconds** = `lengthM / 8.6` rounded (today 110 s for about 950 m).
13. **Wind** (§4.3): speed from season and weather, direction seeded, but rotated so that no
    route's first leg points within 30° of dead upwind. `windFactor` never reaches zero, so every
    route can be sailed. This keeps the start fair.

### 4.3 Season and weather in sails

- **Base wind**: `speed = biome.windBase (Tropical 7.0, Boreal 6.0, Fell 8.0, Farmland 6.5) ×
  season (Spring 1.0, Summer 0.9, Autumn 1.2, Winter 1.15) × weather.windScale`. Today's map uses
  7.0 m/s.
- **`Sails.Weather` gets a profile**. The spell machine (`nextSpell`, `step`, `conditions`) stays,
  and a new record parameterises it:
  ```elm
  type alias Profile =
      { firstFair : ( Float, Float )          -- today (10, 20)
      , weights : { squall : Float, thunder : Float, lull : Float, fairOnly : Bool }
      , fairDuration : ( Float, Float )       -- today (14, 30)
      , baseRain : Float, baseGloom : Float, baseSea : Float
      , windScale : Float, fogScale : Float
      , precip : Precip                       -- Rain | Snow (Render picks streaks or flakes)
      }
  init : Profile -> Int -> Weather
  conditions : Profile -> Wind -> Weather -> Conditions   -- adds `fog : Float` and `precip`
  ```
  Profiles: **Clear** = `fairOnly`, the plain course wind. **Overcast** = baseGloom 0.3, rare
  squalls. **Rain** = baseRain 0.45, squall weight 0.7. **Storm** = today's weights (0.4 / 0.35 /
  0.25), which keeps the current gameplay. **Fog** = fairOnly, windScale 0.7, fogScale 0.3, sea
  0.8. **Snow** = baseRain 0.5 with `precip = Snow`, gloom 0.2, occasional squalls, no thunder.
- **`OceanShader`**: `skyColour`, `stormColour`, `u_deep`, `u_shallow` and `u_cap` become fields
  of a `SeaPalette` taken from the biome and season. Tropical keeps today's values (colour 321 /
  11 and `vec3 0.8 0.92 0.95`). Boreal uses darker blue-green. Fell uses steel blue. Winter
  lightens the caps and greys the sky. `fogNear`/`fogFar` multiply by `conditions.fog`. `Ctx`
  gains `fog` and `palette` next to `sea`, `gloom` and `flash` (`withWeather` already threads
  weather into Ctx).
- **Rain**: `Render.rainEntities` moves to `Game.Precipitation` (shared with racer) with a Snow
  mode.
- **`weatherSeed`** in `Pages/Sail.elm` stays as `Weather.seedFrom (course.id ++ "#" ++ roomId)`
  in rooms and `++ "/" ++ run` when sailing alone. `course.id` is the map key now, so storms
  differ between maps but are shared within a room, as they are today.

### 4.4 Biome -> islands

| Biome | Island scale | Dressing (existing pieces) | Sea / shore |
|---|---|---|---|
| Tropical | radius ×1.0, height ×1.0 | `palmTree`, `fort` on hub, `houses`/`tavern`/`townHall`, `pier`, `seaArch`, `chest`, `reef`, `wreck`, `pirateFlag`, `seabird` (today's set) | turquoise, sand beach |
| Boreal | radius ×0.85, more skerries (4-5) | `Game.BrickModels.pineTree`, `rock`, `cottage` (red), `pier`, lighthouse (`BrickModels.lighthouse`) on hub | dark teal; grey granite beach tier |
| Farmland | radius ×1.0 | `leafyTree`, `bush`, `barn`, `hayBale`, `cottage`, `fence`, `pier`, `lighthouse` on hub | mid blue; green shores |
| Fell | radius ×0.9, height ×1.8 (steep fjord-like) | `rock`, `pineTree` (sparse), `cottage`, `pier`, cairn (stacked `rock`) on hub | steel blue; grey and, in Winter, white tiers |

Island tier colours: `Game.BrickModels.island` is a fixed four-tier model. It needs a parameterised
`islandIn : { beach : Int, grass : Int, rock : Int }` in brick-studio. Fallback: a local copy in
`Sails.Scenery` (the island is a short LDraw string, so a local copy is cheap). The current
`Sails.Render.islandEntities` already places palms by index and the fort on island 0. It becomes a
`case biome` dressing table that keys on `island.role`. The lighthouse beam
(`lighthouseBeamEntity`) and seabirds (`seabirdFlockEntities`) look for the `Hub` island instead
of the head of the list.

Season on islands: Winter adds white top tiers in Boreal, Fell and Farmland, and no hay. Autumn
gives Farmland orange-tan grass. Tropical changes only the sea colour and rain.

### 4.5 Reuse list (sails)

| Keep | Change | Delete | New |
|---|---|---|---|
| `harbour`, `shorePoint`, `beachHeight`, `grassHeight`, `harbourOffshore`, `harbourRadius`, `dockSpeed`, `dockSeconds`, `stepBoat`, `pushOutOfIsland`, `rockImpact`, `windFactor`, sail levels, `courseRoute`, `nearestCoursePoint`, `courseCurrent`, `courseFreeRange`, `stepProgress`, `harbourThrottle`, `reachedTreasure`, `starRating`, cannons and targets, `Weather.step`/`nextSpell` machinery, all `Sails.Scenery` pieces, ShipMesh, Hull, Boats | `initSailingState`/`initProgress` (unchanged signature), `keepInArena` (course arena), `Weather.init`/`conditions` (profile), `OceanShader` palette and fog, `Render.islandEntities`/`reefEntities`/`lighthouseBeamEntity`/`seabirdFlockEntities`/`townEntities` (biome and role driven), `Pages/Sail.routeCourse` (uses archipelago, no `defaultCourse.id` check) | `defaultCourse`, `courseForSeat`, `routes`, `routeCount`, `routeFor`, `waypointsFor`, the six island constants, `arenaCenter`, `arenaRadius`, the fixed target and reef coordinates, the "harbour-loop" id | `Sails.Archipelago`, `Sails.World`, `Weather.Profile`, `Game.Precipitation` |

### 4.6 Sails file touch estimate

`src/Sails/Types.elm`, `src/Sails/Physics.elm` (~ -170 lines of course data), `src/Sails/Archipelago.elm`
(new, ~400), `src/Sails/World.elm` (new, ~200), `src/Sails/Weather.elm`, `src/Sails/OceanShader.elm`,
`src/Sails/Render.elm`, `src/Sails/Scenery.elm` (island variants, town name lists if kept there),
`src/Sails/HUD.elm` (map badge), `src/Sails/I18n.elm`, `src/Sails/Ingest.elm` (`map=` in hash next to
`#b/`), `src/Pages/Sail.elm`, `src/Pages/Shipyard.elm` (picker), `src/Pages/Results.elm` (Same map /
New map), `src/Main.elm`, tests `SailsTest.elm` and `GameplayTest.elm` (`seatCourses` built from a fixed
map list instead of `routeCount`).

---

## 5. Menu and state changes

### 5.1 Shared picker (`Game.MapPicker`)

One compact row of controls, the same in both games:

```
[Biome: Random | Farmland | Boreal | Fell | Tropical]
[Season: Random | Spring | Summer | Autumn | Winter]
[Weather: Random | Clear | Overcast | Rain | Storm | Fog | Snow]   (disallowed chips greyed)
[Seed: 48213] [dice]   [Previous map: boreal-winter-snow-48213]  (shown only when lastMap exists)
```

The view takes `Selection`, a `Maybe MapParams` for the previous map, a labels record (each game
passes its own I18n strings), and these messages: `onPickBiome (Pick Biome)`, `onPickSeason`,
`onPickWeather`, `onSeedInput String`, `onRandomSeed`, `onUsePrevious`. Pasting a full map code
into the seed field is accepted. It sets all four values (`fromCode`), which is how a code from the
other game is used. The view has no state of its own.

### 5.2 Racer data flow (`src/Main.elm`)

- Model: replace `trackId : TrackId` and `seed : Int` (lines 185-186, 248-249) with
  `selection : Selection`, `map : MapParams` (the resolved current map) and
  `lastMap : Maybe MapParams`. `CachedTrack` (line 195) is keyed by `mapCode : String`.
- Msgs: replace `SelectTrack TrackId`, `ChangeSeed String` and `RandomSeed` (lines 347-349) with
  `PickBiome`, `PickSeason`, `PickWeather`, `ChangeSeed String`, `RandomSeed` and `UsePreviousMap`.
  Each one recomputes `map = resolve selection` and writes the hash (today `SelectTrack` writes it
  at line 1333). `ChangeSeed` also tries `fromCode`.
- Hash: `Ingest.encodeRaceHash : MapParams -> Document -> String` produces
  `#map=<code>&b/...`. `encodeGarageHash` produces `#garage&map=<code>&b/...`. `parseHash` reads
  `map=`, and `hashWithoutTrackParams` strips `map=` (and no longer `track=`/`seed=`). A hash with
  the old `track=` is simply ignored: the map falls back to the default selection
  (all Random, seed 1). `DecodedRaceInput.trackId/seed` become `map : Maybe MapParams`.
- Init (lines 448-491): `map = parsedInit.map |> Maybe.withDefault (resolve defaultSelection)`.
  The selection is set from the map with every field `Exactly`, so a shared link shows its
  concrete values.
- Starting a race: `ensureTrackBuilt model.map` (line 3039) → `Track.trackConfigFor map |>
  Track.generateValid`. On finish: `lastMap = Just race.track.config.map` (around line 2654,
  where ResultsState is built).
- Results (`Pages/Results.elm`): `onRaceAgain` keeps its meaning (same map). Add `onNewMap`, which
  sets `selection.seed = randomSeed`, re-resolves with the same picks and goes back to the garage.
  The share link already carries the map.
- "Previous game": on the main screen `lastMap` is in memory, and after a reload it comes from the
  hash (the hash is written on every change). No new storage.

### 5.3 Sails data flow (`src/Main.elm`, `src/Pages/Shipyard.elm`, `src/Pages/Sail.elm`)

- Model: `course : Course` (line 100, today `Physics.defaultCourse`) becomes `selection`,
  `map : MapParams`, `archipelago : Archipelago` (cached, rebuilt when `map` changes),
  `lastMap : Maybe MapParams`. `course` is derived from the archipelago with
  `Archipelago.courseFor archipelago seat` when sailing starts.
- Msgs: add the six picker messages to `Msg` (line 118). `StartSailing` stays.
- Hash: `#map=<code>` on its own, or `#map=<code>&b/<payload>` when a ship is loaded. `#room=`
  stays on its own. `Ingest.hasShipPayload` and the ship decoding have to strip `map=` first (the
  same approach as racer's `hashWithoutTrackParams`). The `HashChanged` handler (line 269) also
  applies `map=`. Writing the hash in `encodeShipLink` call sites (line 456) and in
  `GamePorts.setUrlHash ("#b/" ++ b64)` (line 252) prefixes the map.
- Boot: `map` = hash `map=` if present, otherwise the `courseId` of the most recent leaderboard
  entry (entries have `isoDate`) as `lastMap`, otherwise `resolve defaultSelection`. This gives
  "previous game" after a reload with no new storage key.
- `Pages/Sail.routeCourse` (line 201) becomes `Archipelago.courseFor model.archipelago seat`.
  The `defaultCourse.id` guard goes away because every course is generated.
- Results: `onSailAgain` (same map, new weather run as today), plus `onNewMap`.

### 5.4 Default selection

`{ biome = Any, season = Any, weather = Any, seed = 1 }` on first visit. "Random" lets first-time
players see variety. The seed is shown, so a good map can be shared.

---

## 6. Persistence, leaderboards, multiplayer

### 6.1 What happens to existing data (no backwards compatibility)

- **Racer leaderboard** (`palikkakehys.racer.leaderboard`): the entry decoder becomes strict: it
  requires `mapCode` and a `g1:` prefix. Old entries (with `trackId`) fail to decode and are
  dropped. Today an unknown id falls back to `FarmLane` (Leaderboard.elm line 133); that fallback
  goes. The list decoder becomes "decode each, keep successes" so one bad row cannot wipe the
  board. The next save writes the cleaned list. `leaderboard-migration.js`, its test and the call
  in `main.js` are deleted.
- **Sails leaderboard**: `Game.Leaderboard` is shared and keyed by a `courseId` string. After
  `Leaderboard.fromJson`, Sails Main filters out entries whose `courseId` does not start with `g1:`
  (this removes `"harbour-loop"`).
- **Shared URLs**: old racer links `#track=farm-lane&seed=42&b/...` still load the **car** (the
  `b/` payload decodes as before). The track part is ignored, and the default map is used. Old
  sails `#b/` links load the ship as today. Nothing old resolves to a specific new map.
- **Storage growth**: map keys are effectively unlimited, so cap the boards. Racer:
  `keepTopPerTrack 6` per key (unchanged), plus at most 60 distinct keys, dropping the oldest by
  list position (new entries are prepended). Sails: the same cap by `isoDate`. Write the cap once
  as `Game.Leaderboard.capKeys` (brick-studio) or locally in each game.

### 6.2 Leaderboard key

`leaderboardKey map = "g1:" ++ toCode map` (D9). The personal best shown in the picker and HUD is
for the exact map. Any change to generator output, physics constants that affect times, or the
`World` tables bumps `generatorVersion` to `g2`, and the strict decoder then drops old records
automatically. A unit test pins a few map codes to a golden checksum (§7), so an accidental change
to generator output fails CI and forces a conscious version bump.

### 6.3 Multiplayer

- **brick-studio change (D11)**: in `packages/brick-frame-game/runtime/multiplayer.js`,
  `startHost(requestedRoomId, courseId, ...)` (line 438) stores `courseId` and adds it to the
  room-state broadcast. `Game.Multiplayer.RoomState` gets `courseId : String` with
  `optionalField "courseId" Decode.string ""` in `decodeRoomState` (it is `Decode.map8` today,
  so add one `andMap` step or nest the decoder). Both games then send `courseId = toCode map`
  (racer Main line 900, sails Main line 285).
- **Joiners**: when `RoomState.courseId` parses with `fromCode` and differs from the joiner's map,
  the joiner switches to it before the countdown (racer: rebuild through `ensureTrackBuilt`; sails:
  rebuild the archipelago). The picker shows the host's map read-only while in a room.
- **Fallback without the brick-studio change**: the host's vehicle hash already carries the map
  (racer's `setMultiplayerVehicle (Ingest.encodeRaceHash ...)`; sails' `sharedShip` would get the
  `map=` prefix). The joiner could take the map from the slot-1 player's hash. This works, but it
  is fragile, so it is only a fallback.
- **Sails seats**: `courseFor archipelago (mySlot - 1)` uses `routes[seat mod length routes]`,
  the same wrap-around as today's `modBy routeCount`. Every seat sees the same islands, harbours,
  towns, treasure and weather. Only the harbour order differs, as today. Progress is reported as
  the share of harbours docked, which already works for any order.
- **Racer**: every seat races the same generated track (as today with one shared track).

---

## 7. Testing strategy

Toolchain note: neither `elm`, `elm-test` nor `devenv` is installed in this environment, and
`vendor/brick-studio` is not initialised in either checkout. Run `make vendor` and then
`devenv shell -- make test`, `make check` and `make build` locally or in CI. `make check` includes
elm-review. `make vendor` symlinks `vendor/packages` to `brick-studio/packages`, which is how the
`elm.json` source directories resolve.

### 7.1 Shared (`brick-studio/packages/brick-frame-game/tests`)

- `toCode >> fromCode` round-trips for every biome × season × allowed weather and seeds
  {1, 2, 999999}. `fromCode` rejects junk, clamps the seed, coerces disallowed weather and is
  case-insensitive.
- `resolve` is deterministic. `Exactly` choices are kept. Over 2,000 seeds, `Any` produces every
  value with roughly even frequency (within ±30 %). `resolve` never produces a disallowed weather.
- `subSeed` streams differ by name.

### 7.2 Racer (`tests/TrackTest.elm` rewritten, new `tests/CourseGenTest.elm`)

- **Determinism**: the same code gives identical samples, obstacles, ramps and loop
  (generalises today's "Deterministic generation" test).
- **Golden checksums**: 8 fixed codes (two per biome) give a pinned `totalLengthM`, sample count
  and obstacle count. A failure means "bump `g1`".
- **Seed sweep** (property style, `Fuzz.intRange 1 999999` with a fixed run count, plus an
  explicit list of 25 seeds per biome so CI time stays bounded): `validateCourse` passes on the
  first 8 attempts; length and grade are within bounds; clearance between legs holds; exactly one
  loop; ≥ 1 brick ramp; checkpoints strictly increasing and the finish at `finishS`.
- **Generalised existing invariants** (today run on the three presets in `TrackTest` lines
  153-161): scenery stays off the roadbed and clear of the start; launch ramps leave a free lane;
  side ramps stay on the road edge; the loop is clear of gates, ramps and scenery; the loop lane is
  continuous. Run them over the 8 golden codes.
- **Environment**: `gripScale` is within [0.6, 1.0], and Summer + Clear = 1.0 exactly. A physics
  test: the same car on the same code is slower or equal in Winter + Snow than in Summer + Clear
  (`Stage5Test` style straight-line run).
- **Ingest**: `#map=` encode/parse round-trip; an old `#track=` hash loads the car and ignores the
  track; `#garage&map=`.
- **Leaderboard**: the strict decoder drops old-format entries and keeps new ones; the key cap
  works.
- **Performance smoke**: generating and validating the 8 golden codes stays within a time budget
  (elm-test has no timer, so measure in the browser check below and log it in the PR).

### 7.3 Sails (`tests/SailsTest.elm`, `tests/GameplayTest.elm`, new `tests/ArchipelagoTest.elm`)

- **Determinism** and **golden checksums** (island count, total route length per code).
- **Seed sweep, 100 seeds per biome**: every route leg is ≥ radius + 8 m from every island
  (except 12 m at its own harbour); island gaps ≥ 30 m; turn at harbour ≤ 150°; route length
  850-1050 m; ≥ 1 route; treasure and targets clear of islands; every waypoint is inside the arena
  with ≥ 40 m margin; the start is not inside any island; wind is not dead upwind on the first leg.
- **Solvability**: the existing test `autopilot` (SailsTest.elm, "Simple autopilot round a seat's
  course") already sails a course with the brake assist. Run it on every route of 12 fixed codes
  (3 per biome) with the default boat and require `finished` within `2 × targetSeconds`. Today's
  `seatCourses` (built from `routeCount`) becomes a list from those codes.
- **Weather profiles**: Clear and Fog never leave `Fair`; Storm keeps today's spell frequencies
  (seeded run of 600 s with counts within a band); Snow gives `precip = Snow`; `conditions` with
  the Clear profile equals today's `fair course.wind`.
- **Rendering pure helpers**: `islandEntities` count per biome on a mock `Draw` (the pattern
  `Render.sprayEntities` tests already use with `mockDraw`).

### 7.4 Visual and manual checks

- Use the `run` skill (Vite dev server, `make dev` / `watch`) with a headless browser to take
  screenshots of a fixed grid of codes in each game: 4 biomes × {Summer Clear, Winter Snow, Autumn
  Fog, Storm}. That is 16 screenshots per game, attached to the PR. Check: fog distance, ground and
  sea colours, precipitation, scenery mix, readable HUD badge.
- Manually play three random seeds per biome on a desktop and on a phone (touch and tilt).
  Record frame time when the race starts (chunk build) against the current presets.

---

## 8. Phased milestones

Each phase ends with `make test`, `make check` and `make build` green in every repo it touches, and
is deployable on its own.

### P0: Shared types and tables (brick-studio, then a submodule bump in both games)

- Deliverable: `Game.MapParams` (types, codec, compatibility, resolve, subSeed, leaderboardKey),
  `Game.MapPicker`, `Game.Precipitation` (lifted from `Sails.Render.rainEntities`),
  `BrickModels.palmTree`, `RoomState.courseId` + `multiplayer.js` broadcast, with tests.
  Optional: `islandIn`, `leafyTreeIn`, `pineTreeIn`.
- Risk: brick-studio is shared with other apps (brick-play and so on), and a submodule bump drags
  in unrelated changes. Mitigation: every change is additive (new modules, optional field), so
  nothing else breaks.
- Done when: brick-studio tests pass, both games build against the bumped submodule with **no
  behaviour change**.
- If brick-studio is blocked, use D7 (local copies) and continue. Multiplayer sync then uses the
  §6.3 fallback.

### P1: Racer generator behind the old menu (racer)

- Deliverable: `Racer.World`, `generateKnots`, `validateCourse`, `generateValid`,
  `TrackConfig.map/env`, palette, fog and grip from the world, precipitation and flash
  entities, PalmTree. The three old `TrackId`s temporarily map to fixed codes
  (`farmland-summer-clear-<seed>`, `boreal-summer-clear-<seed>`, `fell-summer-clear-<seed>`), so
  the existing menu works and tests can be ported step by step.
- Risk: the generated courses feel worse than the tuned blueprints. Mitigation: keep the
  blueprint value ranges, compare feel against the old presets in the browser, and add the seed
  sweep before tuning.
- Done when: the sweep and generalised invariant tests pass, the visual grid has been checked, and
  frame time at race start is within 20 % of today.

### P2: Racer menu, hash, leaderboard; remove old tracks (racer)

- Deliverable: picker in `Pages/Garage`, `#map=` hash, Same map / New map on Results, previous
  map, strict leaderboard with cap, HUD badge, I18n, `courseId = toCode map` + joiner switch. Delete
  `TrackId`, the blueprints and the migration JS. Update the docs (`docs/brick-racer-design.md` §5
  and §8.2, `AGENTS.md`).
- Risk: hash parsing regressions for car links. Mitigation: IngestTest cases for every hash shape
  (`#b/`, `#model=`, `#garage&...`, `#room=`, old `#track=`).
- Done when: there are no `TrackId` references (`grep -r TrackId src tests` is empty), and a
  manual check of share link → reload → same map works in two browsers.

### P3: Sails archipelago generator (sails)

- Deliverable: `Sails.Archipelago`, `Sails.World`, `Course` changes, `Weather.Profile`, sea
  palette and fog, biome island dressing, decor from the course, arena from the course. The
  existing Shipyard still starts a fixed code (`tropical-summer-storm-1`), so the look and gameplay
  stay close to today while the generator matures.
- Risk: generated islands block routes or make harbours hard to dock. Mitigation: clearance and
  solvability sweeps with the autopilot, and the repair loop.
- Done when: ArchipelagoTest and the autopilot over 12 codes pass, and the 16-shot visual grid has
  been checked.

### P4: Sails menu, hash, leaderboard, multiplayer (sails)

- Deliverable: picker in `Pages/Shipyard`, `#map=` (+ `&b/`), previous map from the hash or the
  latest leaderboard entry, Same map / New map in Results, leaderboard filter and cap,
  `courseId = toCode map` + joiner switch, HUD badge, I18n. Delete `defaultCourse`, `routes`, the
  island constants and `arenaCenter`/`arenaRadius`.
- Done when: a 2-player room (host + joiner in two tabs) gets the same islands with different
  harbour orders, and a map code copied from racer opens the same biome, season and weather in
  sails.

### P5: Cleanup and polish (both, plus optional brick-studio)

- Seasonal tree colour variants (`leafyTreeIn`, `pineTreeIn`), parameterised island tiers if not
  done in P0, `SkyParams.cloudCover` for Overcast and Storm, optional fog-based chunk culling,
  tuning passes on grip and wind tables, final golden checksum and `g1` freeze, README updates,
  removal of the D7 fallback copies if they were used.

Ordering note: P1 and P3 are independent once P0 lands, so they can run in parallel. P2 depends
on P1, and P4 on P3.

---

## 9. Risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| **Chunk/terrain build cost per new seed.** Today a track is built once per (track, seed) and cached in `CachedTrack`. With "New map" players will build many. | Hitch at race start, mainly on phones | Course size stays in today's range, so the cost per build is the same as now. The validation retries run only the cheap checks; scenery and chunks are built once, after a pass. Measure in P1. If needed, build chunks progressively (the first N chunks before the countdown, the rest during it). |
| **Fairness and solvability of random racer courses** (unreachable loop exit, a ramp onto a bend, a blind drop) | Frustration; broken PBs | Monotonic z progress, grade and curvature bounds, the existing placement rules in `placeLoops`/`placeBrickRamps`/`placeSideRamps` (which already avoid gates and bends), the seed sweep, golden codes, and the recovery corridor and tow that already exist. |
| **Sails islands blocking routes** or harbours facing the wrong way | Impossible docking | 8 m leg clearance, 30 m island gaps, ≤ 150° turn rule, repair loop, autopilot solvability sweep, and wind kept off dead upwind on the first leg. |
| **WebGL shader changes for weather** (new uniforms for fog, flash and palette; snow mode) on older mobile GPUs | Visual bugs, lower frame rate | Reuse existing uniforms (`u_fogNear`/`u_fogFar` already exist in TerrainShader; sails `Ctx` already has gloom and flash). Precipitation is a few instanced streak meshes (as in sails today). No new render passes. Check on a phone in P1 and P3. |
| **brick-studio submodule changes** affect other apps and need a separate PR/review | Schedule slip | Additive changes only. D7 fallback copies. Bump the submodule in each game in its own commit. Flag ownership early (open question 1). |
| **Leaderboard churn**: every code is new, so PBs are rarely beaten | Less motivation to replay | "Same map" button, previous map shortcut, map code shown on Results and in the share link so friends race the same map. A biome-level summary could come later. |
| **Generator drift invalidates boards silently** | Unfair records | Golden checksums in tests force a conscious `generatorVersion` bump. |
| **Season and weather tables explode in tuning effort** (4 × 4 × 6) | Inconsistent look | Orthogonal records (biome base × season tint × weather overlay) and a 16-shot visual grid per game instead of 96 combinations. |
| **Physics tests depend on preset tracks** (`TrackTest` 32 references, `PhysicsTest`, `Stage5Test`, `PlayFeelTest`) | Big test rewrite | P1 keeps a temporary mapping from TrackId to fixed codes, so tests port one at a time. `Environment` is neutral by default. |

---

## 10. Open questions for the user

1. **brick-studio ownership**: may the shared changes (`Game.MapParams`, `Game.MapPicker`,
   `Game.Precipitation`, `RoomState.courseId`, `BrickModels.palmTree`, and parameterised
   island/tree models) land in brick-studio first? If not, the plan uses local copies (D7) and the
   vehicle-hash multiplayer fallback.
2. **Biome set**: OK with Farmland / Boreal / Fell / Tropical (D1), or do you prefer a desert/arid
   biome instead of Fell or Tropical (needs new props in both games)?
3. **Cross-game codes**: should one map code mean "the same biome, season, weather and seed" in both
   games (D4: a racer code can be pasted into sails)? Or did "pass seed from previous game" mean
   only "replay my last map in the same game"? The plan supports both; the question is how much
   to advertise it in the UI.
4. **Leaderboard granularity**: per exact map code (D9, recommended), or per biome + season +
   weather ignoring the seed, which gives more comparable boards but mixes different courses?
5. **Racer weather**: static per run (D12), or should racer also have sails-style changing spells,
   for example rain starting halfway down?
6. **Default for first-time players**: all "Random" (recommended), or a fixed friendly start such
   as `farmland-summer-clear-1`?
