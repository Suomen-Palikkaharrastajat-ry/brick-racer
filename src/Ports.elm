port module Ports exposing
    ( playSfx
    , updateAudioState
    )

{-| Ports of Brick Racer whose payload is its own: the procedural Web Audio
telemetry and one-shot sounds `audio.js` plays. Every other port the game
uses is shared by the Brick Frame games and lives in `Game.Ports`.
-}


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
