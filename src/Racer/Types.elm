module Racer.Types exposing
    ( CameraMode(..)
    , Checkpoint
    , ControlPoint
    , CrashReason(..)
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


{-| Available mountain downhill courses.
-}
type TrackId
    = PineRidge
    | QuarrySwitchbacks
    | AlpineGlacier


{-| Serialize a `TrackId` for URL hashes and storage.
-}
trackIdToString : TrackId -> String
trackIdToString trackId =
    case trackId of
        PineRidge ->
            "pine-ridge"

        QuarrySwitchbacks ->
            "quarry-switchbacks"

        AlpineGlacier ->
            "alpine-glacier"


{-| Parse a `TrackId` from URL query/hash segments.
-}
trackIdFromString : String -> Maybe TrackId
trackIdFromString raw =
    case String.toLower (String.trim raw) of
        "pine-ridge" ->
            Just PineRidge

        "quarry-switchbacks" ->
            Just QuarrySwitchbacks

        "alpine-glacier" ->
            Just AlpineGlacier

        _ ->
            Nothing


{-| Surface material beneath a wheel or track segment.
-}
type SurfaceType
    = AsphaltTrack
    | CobbleStone
    | GravelShoulder
    | AlpineGrass


{-| Special track segments placed along spline control points.
-}
type SegmentFeature
    = LaunchRamp { lengthM : Float, lipHeightM : Float }
    | Chicane { sideOffsetM : Float, barrierCount : Int }
    | RoughWash { amplitudeM : Float, wavelengthM : Float }
    | SpeedGate


{-| Control knot along a mountain track spline.
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


{-| Kind of roadside or trackside obstacle.
-}
type ObstacleKind
    = PineTree
    | Boulder
    | ChicaneBlock
    | BarrierPost


{-| Solid cylindrical/AABB-approximated track obstacle.
-}
type alias TrackObstacle =
    { id : Int
    , kind : ObstacleKind
    , s : Float
    , position : Vec3
    , radiusM : Float
    , heightM : Float
    }


{-| Raised launch ramp blended into heightmap queries.
-}
type alias RampZone =
    { startS : Float
    , endS : Float
    , lipHeightM : Float
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


{-| Complete deterministic generated mountain course.
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
-}
type ValidationCode
    = EmptyModel
    | TooFewWheels Int
    | CollinearStance
    | BellyScrape Float
    | HighCenterOfMass Float
    | DisconnectedBricks Int
    | FixedAxleTip
    | MissingPartGeometry Int
    | RubberGripBonus
    | AeroWedgeBonus
    | RealSteeringBonus


{-| One structured validation diagnostic produced by `Racer.Analyze`.
-}
type alias ValidationIssue =
    { severity : ValidationSeverity
    , code : ValidationCode
    , detail : String
    }


{-| Analyzed rolling soapbox parameters and normalized coordinate mapping.
-}
type alias VehicleSpec =
    { name : String
    , placementCount : Int
    , legoMassGrams : Float
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
    | HoodCam
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
