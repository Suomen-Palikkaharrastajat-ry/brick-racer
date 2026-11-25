module Racer.Types exposing
    ( BrickRamp
    , CameraMode(..)
    , Checkpoint
    , ControlPoint
    , CrashReason(..)
    , Fix(..)
    , GeneratedTrack
    , InputState
    , LeaderboardEntry
    , ObstacleKind(..)
    , PhysicsEvent(..)
    , PhysicsState
    , PresetBuild
    , RaceMode(..)
    , RampZone
    , SegmentFeature(..)
    , SteeringMechanism(..)
    , SurfaceType(..)
    , TerrainSample
    , TrackConfig
    , TrackId(..)
    , TrackObstacle
    , TrackSample
    , ValidationCode(..)
    , ValidationIssue
    , ValidationSeverity(..)
    , VehicleSpec
    , WashZone
    , WheelSpec
    , allTrackIds
    , fixedDt
    , lduPerMeter
    , lduToMeters
    , metersToLdu
    , trackIdFromString
    , trackIdToString
    , vec3LduToMeters
    , vec3MetersToLdu
    )

{-| Shared domain types and unit conversions for Brick Racer.

Bridges LDraw model units (`1 stud = 20 LDU`), WebGL render space (`1 unit = 1 LDU`
or scaled `1 unit = 1 m`), and downhill soapbox physics (`160 LDU = 1.0 m`,
right-handed `+Y` up, `+Z` forward).

-}

import Array exposing (Array)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


{-| Scale conversion factor from LDraw units to soapbox world meters.
160 LDU = 1.0 m (8 studs = 1 m, 1 LDU = 0.00625 m).
-}
lduPerMeter : Float
lduPerMeter =
    160.0


{-| Fixed physics simulation timestep in seconds (60 Hz).
-}
fixedDt : Float
fixedDt =
    1.0 / 60.0


{-| Convert scalar distance from LDU to meters.
-}
lduToMeters : Float -> Float
lduToMeters ldu =
    ldu / lduPerMeter


{-| Convert scalar distance from meters to LDU.
-}
metersToLdu : Float -> Float
metersToLdu meters =
    meters * lduPerMeter


{-| Convert a 3D vector from LDU to meters.
-}
vec3LduToMeters : Vec3 -> Vec3
vec3LduToMeters v =
    Vec3.scale (1.0 / lduPerMeter) v


{-| Convert a 3D vector from meters to LDU.
-}
vec3MetersToLdu : Vec3 -> Vec3
vec3MetersToLdu v =
    Vec3.scale lduPerMeter v


{-| Available countryside downhill courses.
-}
type TrackId
    = FarmLane
    | ForestTrail
    | MeadowPath


{-| Every selectable course, in menu order (easiest first).
-}
allTrackIds : List TrackId
allTrackIds =
    [ FarmLane, ForestTrail, MeadowPath ]


{-| Serialize a `TrackId` for URL hashes and storage.
-}
trackIdToString : TrackId -> String
trackIdToString trackId =
    case trackId of
        FarmLane ->
            "farm-lane"

        ForestTrail ->
            "forest-trail"

        MeadowPath ->
            "meadow-path"


{-| Parse a `TrackId` from URL query/hash segments.

Ids of the retired long mountain courses still resolve to the countryside course
that replaced them, so old share links and stored best times keep working.

-}
trackIdFromString : String -> Maybe TrackId
trackIdFromString raw =
    case String.toLower (String.trim raw) of
        "farm-lane" ->
            Just FarmLane

        "forest-trail" ->
            Just ForestTrail

        "meadow-path" ->
            Just MeadowPath

        "pine-ridge" ->
            Just FarmLane

        "quarry-switchbacks" ->
            Just ForestTrail

        "alpine-glacier" ->
            Just MeadowPath

        _ ->
            Nothing


{-| Unpaved surface beneath a wheel or track segment.

`RoughVerge` is the tufty strip just outside the roadbed and `MeadowGrass` the
open field beyond it.

-}
type SurfaceType
    = DirtRoad
    | GravelRoad
    | ForestTrack
    | GrassPath
    | RoughVerge
    | MeadowGrass


{-| Special track segments placed along spline control points.
-}
type SegmentFeature
    = LaunchRamp { lengthM : Float, lipHeightM : Float }
    | RoughWash { amplitudeM : Float, wavelengthM : Float }


{-| Control knot along a countryside track spline.
-}
type alias ControlPoint =
    { position : Vec3
    , roadHalfWidth : Float
    , bankingRad : Float
    , surface : SurfaceType
    , feature : Maybe SegmentFeature
    }


{-| Course configuration descriptor shown in menus and generator.
-}
type alias TrackConfig =
    { id : TrackId
    , seed : Int
    , lengthM : Float
    , dropM : Float
    , defaultHalfWidthM : Float
    }


{-| Precomputed arc-length spline sample at regular 2 m intervals.
-}
type alias TrackSample =
    { s : Float
    , center : Vec3
    , tangent : Vec3
    , right : Vec3
    , normal : Vec3
    , halfWidth : Float
    , bankingRad : Float
    , surface : SurfaceType
    }


{-| Checkpoint gate along the course.
-}
type alias Checkpoint =
    { index : Int
    , s : Float
    , center : Vec3
    , tangent : Vec3
    , halfWidth : Float
    , isFinish : Bool
    }


{-| Kind of brick-built roadside scenery. Every kind can be smashed apart.

`Crate`, `Barrel`, `GardenGnome`, `BrickWall`, `BrickPallet`, `TileStack`,
`BrickGate` and `SlopeBlock` stand close to the road edge as things to crash
into: each breaks into debris and costs some speed and, for the heavy ones,
some structural integrity.

-}
type ObstacleKind
    = LeafyTree
    | PineTree
    | Bush
    | FenceSection
    | Rock
    | Signpost
    | HayBale
    | Cottage
    | Barn
    | Crate
    | Barrel
    | GardenGnome
    | BrickWall
    | BrickPallet
    | TileStack
    | BrickGate
    | SlopeBlock
    | Bleachers


{-| Cylinder-approximated roadside scenery piece. `yaw` orients fences, signs and
buildings relative to the road (radians around +Y).
-}
type alias TrackObstacle =
    { id : Int
    , kind : ObstacleKind
    , s : Float
    , position : Vec3
    , radiusM : Float
    , heightM : Float
    , yaw : Float
    }


{-| Raised launch ramp blended into heightmap queries.
-}
type alias RampZone =
    { startS : Float
    , endS : Float
    , lipHeightM : Float
    , halfWidthM : Float
    }


{-| Brick-built launch ramp of slope bricks standing on one lane of the road.

It rises in a straight line from the road surface at `startS` to `lipHeightM` at
`endS`, where it ends in a vertical back face, so a car taken over it at speed
leaves the ground. `centerOffsetM` is its lateral offset from the road center
(signed like `roadOffsetM`); it only covers part of the road, so the other lane
stays free and the ramp never blocks the route. Its outer quarter on each side
slopes down to the road like a row of side slopes.

-}
type alias BrickRamp =
    { startS : Float
    , endS : Float
    , lipHeightM : Float
    , centerOffsetM : Float
    , halfWidthM : Float
    }


{-| Washboard / cobble ripple zone.
-}
type alias WashZone =
    { startS : Float
    , endS : Float
    , amplitudeM : Float
    , wavelengthM : Float
    }


{-| Complete deterministic generated countryside course.
-}
type alias GeneratedTrack =
    { config : TrackConfig
    , samples : Array TrackSample
    , sampleSpacingM : Float
    , totalLengthM : Float
    , totalDropM : Float
    , startPosition : Vec3
    , startYaw : Float
    , finishS : Float
    , checkpoints : List Checkpoint
    , obstacles : List TrackObstacle
    , obstacleBuckets : Array (List TrackObstacle)
    , bucketSizeM : Float
    , ramps : List RampZone
    , brickRamps : List BrickRamp
    , washes : List WashZone
    }


{-| Continuous terrain heightfield query result at world (x, z).
-}
type alias TerrainSample =
    { height : Float
    , normal : Vec3
    , surface : SurfaceType
    , roadOffsetM : Float
    , nearestS : Float
    , nearestCenter : Vec3
    , nearestTangent : Vec3
    , halfWidthM : Float
    }


{-| Extracted wheel or paired rim+tire assembly in normalized Car Space.
-}
type alias WheelSpec =
    { id : Int
    , placementIds : List Int
    , localHubPosLdu : Vec3
    , localHubPosM : Vec3
    , spinAxis : Vec3
    , radiusLdu : Float
    , radiusM : Float
    , widthLdu : Float
    , widthM : Float
    , isFront : Bool
    , isSteerable : Bool
    , hasRubberTire : Bool
    }


{-| Detected steering geometry on the player's LDraw vehicle.
-}
type SteeringMechanism
    = TurntableBogie
        { pivotPosLdu : Vec3
        , pivotPosM : Vec3
        , bogiePlacementIds : List Int
        }
    | TwinKnuckle
        { leftPivotLdu : Vec3
        , rightPivotLdu : Vec3
        , leftPlacementIds : List Int
        , rightPlacementIds : List Int
        }
    | FixedAxleFallback


{-| Validation finding severity level.
-}
type ValidationSeverity
    = Blocker
    | Warning
    | Info
    | Bonus


{-| Machine-readable diagnostic codes for Garage badges and localization.

The cleanup codes carry the flattened placement ids they name (`LooseParts`,
`DuplicateParts`, `FarParts`, `OffGrid`); `ExtraWheels` carries the mass in
grams that removing the surplus wheels would save. `TooFewWheels` and
`WheelCountNotEven` carry the number of grounded wheels: a car needs an even
number of them, at least two.

-}
type ValidationCode
    = EmptyModel
    | TooFewWheels Int
    | WheelCountNotEven Int
    | CollinearStance
    | BellyScrape Float
    | HighCenterOfMass Float
    | LooseParts (List Int)
    | FixedAxleTip
    | MissingPartGeometry Int
    | RubberGripBonus
    | AeroWedgeBonus
    | RealSteeringBonus
    | WheelOffGround Int
    | NoseOverRisk Float
    | CoMOutsideSupport
    | SteeringNotLinked
    | DuplicateParts (List Int)
    | FarParts (List Int)
    | OffGrid (List Int)
    | ExtraWheels Int


{-| An action the car editor can offer to resolve a `ValidationIssue`.

`AddParts` lists part files to add one after another, `Select`, `Remove` and
`Snap` name placement ids (flattened, as `Racer.Analyze` numbers them). `Snap`
moves each named part onto the brick grid (`Racer.Analyze.snapMatrix`); the
analyzer offers it only when `Racer.Analyze.snapIsSafe` holds.

-}
type Fix
    = AddParts (List String)
    | Select (List Int)
    | Remove (List Int)
    | Snap (List Int)


{-| One structured validation diagnostic produced by `Racer.Analyze`.
-}
type alias ValidationIssue =
    { severity : ValidationSeverity
    , code : ValidationCode
    , detail : String
    , fix : Maybe Fix
    }


{-| Analyzed rolling soapbox parameters and normalized coordinate mapping.
-}
type alias VehicleSpec =
    { name : String
    , placementCount : Int
    , brickMassGrams : Float
    , totalMassKg : Float
    , centerOfMassLdu : Vec3
    , centerOfMassM : Vec3
    , boundsMinLdu : Vec3
    , boundsMaxLdu : Vec3
    , wheelBaseM : Float
    , trackWidthM : Float
    , groundClearanceLdu : Float
    , groundClearanceM : Float
    , wheelContactYLdu : Float
    , frontalAreaM2 : Float
    , dragCoeff : Float
    , inertia : { yaw : Float, roll : Float, pitch : Float }
    , wheels : List WheelSpec
    , steering : SteeringMechanism
    , hasSteeringWheelIndicator : Bool
    , steeringWheelPivotLdu : Maybe Vec3
    , steeringWheelIds : List Int
    , loosePlacementIds : List Int
    , issues : List ValidationIssue
    , isDrivable : Bool
    , modelToCarMatrix : Mat4
    }


{-| Player control inputs sampled per fixed physics tick.
-}
type alias InputState =
    { steer : Float
    , brake : Float
    , handbrake : Bool
    , aeroTuck : Float
    , leanX : Float
    }


{-| Reason why physics entered `CrashRecovery`.
-}
type CrashReason
    = StructuralCollapse
    | RolloverTumble
    | OffMountainDrop


{-| Discrete gameplay & audio events emitted by fixed-step physics.
-}
type PhysicsEvent
    = Impact { impulse : Float, scrape : Bool }
    | Landing { impulse : Float }
    | CheckpointCrossed { index : Int, splitTimeS : Float }
    | BrickDetached { remainingIntegrity : Float }
    | ScenerySmashed { obstacleId : Int, impactSpeed : Float, direction : Vec3 }
    | Crashed CrashReason
    | FinishedRace


{-| Fixed-timestep downhill rigid-body vehicle state.
-}
type alias PhysicsState =
    { position : Vec3
    , velocity : Vec3
    , yaw : Float
    , yawRate : Float
    , pitch : Float
    , pitchRate : Float
    , roll : Float
    , rollRate : Float
    , steerAngle : Float
    , leanInput : { x : Float, y : Float }
    , wheelSpinAngles : List Float
    , suspensionCompressions : List Float
    , groundedWheels : Int
    , airborneTime : Float
    , surfaceUnderCar : SurfaceType
    , integrity : Float
    , chassisScraping : Bool
    , offTrack : Bool
    , highSpeedBonus : Bool
    , rolloverTimer : Float
    , progressS : Float
    , checkpointIndex : Int
    , checkpointAnchor : { s : Float, position : Vec3, yaw : Float }
    , lastImpactCooldown : Float
    , nextPopThreshold : Float
    , gradeRatio : Float
    }


{-| Active camera view mode.
-}
type CameraMode
    = ChaseCam
    | OrbitCam


{-| Racing gameplay mode: single player or two-player split-screen.
-}
type RaceMode
    = SinglePlayer
    | SplitScreenTwoPlayer


{-| Built-in starter soapbox racer preset.
-}
type alias PresetBuild =
    { id : String
    , nameEn : String
    , nameFi : String
    , summaryEn : String
    , summaryFi : String
    , ldrText : String
    }


{-| Local personal-best leaderboard record.
-}
type alias LeaderboardEntry =
    { trackId : TrackId
    , vehicleName : String
    , timeSeconds : Float
    , topSpeedKmh : Float
    , wheelCount : Int
    , steeringLabel : String
    , seed : Int
    }
