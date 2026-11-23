port module Ports exposing
    ( playSfx
    , requestThumbnails
    , thumbnailLoaded
    , updateAudioState
    )

{-| Ports of Brick Racer whose payload is its own: the procedural Web Audio
telemetry and one-shot sounds `audio.js` plays, and the part pictures of the
car editor's dock. Every other port the game uses is shared by the Brick Frame
games and lives in `Game.Ports`.
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


{-| Ask the shared thumbnail pipeline (`initThumbnails` in `main.js`) for a
picture of each part in a colour, for the car editor's dock.
-}
port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg


{-| A rendered part picture (a data URL) from the thumbnail pipeline.
-}
port thumbnailLoaded : ({ file : String, color : Int, url : String } -> msg) -> Sub msg
