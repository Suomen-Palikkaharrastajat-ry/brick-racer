# Starter car bases

Four alternative car undercarriages a player can start a Brick Racer build
from. Each is a chassis underside: a plate frame, four rim + tyre wheels on
two axles, and studs on top to build the body on. The car editor (`Pages.Build`,
"Build your own car" in the menu) starts every car from one of them.

| File | Name | Parts | Wheels | Steering found by `Racer.Analyze` | Bounds in LDU (x / y / z) |
|---|---|---|---|---|---|
| `racer/long-plank.mpd` | Long Plank Racer Base | 35 | 4: `4624` rim + `3641` tyre | fixed axles (weight shift) | -58..58 / -44..23 / -180..180 (116 x 67 x 360) |
| `racer/compact-kart.mpd` | Compact Kart Base | 33 | 4: `42610` rim + `50951` tyre | fixed axles (weight shift) | -62..62 / -60..24 / -80..80 (124 x 84 x 160) |
| `racer/wide-offroad.mpd` | Wide Off-Road Base | 46 | 4: `55982` rim + `56891` tyre | fixed axles (weight shift) | -128..128 / -54..46 / -160..160 (256 x 100 x 320) |
| `racer/turntable-kart.mpd` | Turntable Steering Kart Base | 35 | 4: `42610` rim + `50951` tyre | turntable bogie (`3680` + `3679`) | -62..62 / -60..24 / -160..160 (124 x 84 x 320) |

Bounds are from the real catalog geometry, in LDraw axes: -Y is up, the front
of every car points to -Z, and the wheels sit on the ground at the largest y.

## Layout of a file

Each `.mpd` holds two LDraw files:

1. The main model (`<name>.ldr`): header comments (name, description, part
   count, wheel count, bounds) and a single type-1 line that places the base.
2. The base submodel (`<name>-base.ldr`): every part of the chassis.

Because the whole chassis is one submodel reference, the editors (Brick
Studio, Brick Play) select and move it as one piece, and "Ungroup" is needed
before a part of it can be pulled off by accident. The game flattens the
submodel (`LDraw.Document.flattenedPlacements`), so the analyzer sees every
part. The reference is not glued (`0 !PALIKKA GLUED`), so the base can still
be moved as a whole.

## The designs

- **Long Plank**: a 4 x 18 stud two-layer plate frame with side sills and 45°
  nose and tail slopes. Small wheels run just outside the frame on pairs of
  `4488` single-pin holders, which give a wider track (100 LDU) than one
  `4600` and keep the frame low.
- **Compact Kart**: a 6 x 8 stud floor on two plate spacers over 42610/50951
  wheels (wheelbase 120 LDU), with slope bumpers and side pods.
- **Wide Off-Road**: two `3703` Technic 1 x 16 rails with 10L axles (`3737`)
  through their holes, `55982` axle-hole rims with the hub against the rail,
  and `56891` off-road tyres; an 8 x 16 two-layer deck. Highest clearance.
- **Turntable Kart**: a 2-stud-wide spine so the front wheels can steer, side
  pods and cross plates between the axles. The front axle is two `4488`
  holders joined by a 2 x 2 turntable (`3680` base below, `3679` top above),
  which `Racer.Analyze` detects as a `TurntableBogie`.

Every rim and its tyre share one origin, which is how Brick Studio's
`Studio.Health` expects a tyre mounted on a rim.

## Checks

- `tests/StarterBasesTest.elm` (run by `make test`) parses each file and
  checks: one grouped submodel; the header part count; `Racer.Analyze` finds
  4 wheels, all with rubber tyres, 2 at the front, each a rim + tyre pair; the
  car is drivable with no blocker or warning (no loose bricks, no belly scrape,
  CoM not too high); the turntable kart gets `TurntableBogie` and the others
  `FixedAxleFallback`; `Studio.Health` finds no misaligned or overlapping
  part; every studded part sits on studs. The test runs the analyzer offline,
  with its procedural fallback meshes for the wheels.
- `src/Racer/StarterBases.elm` embeds the files for the car editor and the
  tests. Regenerate it after changing a file: `node starters/embed-starters.mjs`.
- With the real catalog parts, each sample was loaded into the game's garage
  (`#model=` link): all four show "4 (4 rubber)" wheels, no warnings, and the
  turntable kart shows "Turntable Bogie". With real part bounds, the rim and
  tyre centres of each wheel are 0 LDU apart (7 LDU for 55982/56891, inside
  the analyzer's 8 LDU pairing limit).

## In the car editor

The editor (`src/Pages/Build.elm`, rules in `src/Racer/Editor.elm`) opens a
starter as it is: the base stays one grouped submodel. The player adds parts
from a fixed list of 49 parts in 8 colours on top of it; the base can be
moved and turned as a whole but never ungrouped, deleted, recoloured or
copied (every edit that would split it is dropped). "Garage" loads the build
like any other model, so its `#b/...` link opens the garage, and "Edit car"
there opens it in the editor again. `tests/EditorTest.elm` and
`tests/BuildPageTest.elm` check this.

## Regenerating

`starters/tools/build_starters.py` is the script that wrote these files (and
the Brick Sails hulls). It measures real part bounds from the catalog's MPD
bundles (`https://catalog.palikkaharrastajat.fi/bundles/`, cached in the
system temp directory) and needs Python 3 with numpy:

```bash
python3 starters/tools/build_starters.py starters/racer /tmp/sails-hulls
node starters/embed-starters.mjs
```
