port module Ports exposing
    ( binaryHashDecoded
    , copyShareUrl
    , decodeBinaryHash
    , fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , hashChanged
    , playSfx
    , requestFileUpload
    , requestGeometryFlatten
    , requestTiltPermission
    , saveLeaderboard
    , setAudioMuted
    , setUrlHash
    , storeLanguage
    , tiltOrientationReceived
    , updateAudioState
    , viewportResized
    )

{-| Ports for Brick Racer: runtime asset/file bridges (`initBricksRuntime`),
binary hash decoding, procedural Web Audio synthesis, device tilt controls, and
localStorage persistence.
-}

import Json.Decode


{-| Request the browser file picker for `.ldr`, `.mpd`, `.dat`, or `.io`.
-}
port requestFileUpload : () -> Cmd msg


{-| Raw LDraw file text delivered from upload, drag-and-drop, or `.io` extraction.
-}
port fileContentReceived : (String -> msg) -> Sub msg


{-| File load error string from the JS runtime.
-}
port fileLoadError : (String -> msg) -> Sub msg


{-| Geometry flattening worker port required by `initBricksRuntime`.
-}
port requestGeometryFlatten : String -> Cmd msg


{-| Flattened geometry response from `initBricksRuntime`.
-}
port geometryFlattened : (String -> msg) -> Sub msg


{-| Geometry worker failure from `initBricksRuntime`.
-}
port geometryFlattenFailed : (String -> msg) -> Sub msg


{-| Update browser URL hash without triggering page reload.
-}
port setUrlHash : String -> Cmd msg


{-| Copy full shareable race URL to clipboard.
-}
port copyShareUrl : String -> Cmd msg


{-| Ask JS to inflate a `#b/<base64url>` string pasted into the in-app input.
-}
port decodeBinaryHash : String -> Cmd msg


{-| Receive decompressed `#b/...` payload bytes (or `Nothing` on error).
-}
port binaryHashDecoded : (Maybe (List Int) -> msg) -> Sub msg


{-| Notify Elm when the browser address bar hash changes externally.
-}
port hashChanged : (String -> msg) -> Sub msg


{-| Viewport resize notifications from `initBricksRuntime`.
-}
port viewportResized : ({ width : Int, height : Int } -> msg) -> Sub msg


{-| Stream continuous vehicle telemetry to the procedural Web Audio synthesizer.
-}
port updateAudioState :
    { active : Bool
    , windGain : Float
    , windHz : Float
    , rumbleGain : Float
    , rumbleHz : Float
    , clatterGain : Float
    , scrapeGain : Float
    }
    -> Cmd msg


{-| Trigger one-shot sound effects (`countdown`, `go`, `impact`, `landing`, `pop`, `checkpoint`, `finish`).
-}
port playSfx : { kind : String, intensity : Float } -> Cmd msg


{-| Persist master mute state to `localStorage` and update JS AudioContext gain.
-}
port setAudioMuted : Bool -> Cmd msg


{-| Ask iOS/browser for `DeviceOrientationEvent` tilt permission.
-}
port requestTiltPermission : () -> Cmd msg


{-| Stream device tilt orientation deltas (`gamma`, `beta`) when enabled.
-}
port tiltOrientationReceived : ({ available : Bool, gamma : Float, beta : Float } -> msg) -> Sub msg


{-| Save JSON-encoded local leaderboard records to `localStorage`.
-}
port saveLeaderboard : String -> Cmd msg


{-| Persist chosen language code (`"fi"` or `"en"`) to `localStorage`.
-}
port storeLanguage : String -> Cmd msg
