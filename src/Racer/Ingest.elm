module Racer.Ingest exposing
    ( DecodedRaceInput
    , IngestError(..)
    , ParsedHash
    , decodeRaceInput
    , encodeRaceHash
    , findPresetById
    , hashWithoutTrackParams
    , isRawLdrText
    , parseHash
    , presets
    )

{-| Model and race URL ingestion for Brick Racer.

Supports:

  - Binary high-density `#b/<base64url>` hashes exported by Brick Play and Brick Studio
    (including JS predecoded `initialBytes` flags and in-app pasted URLs),
  - Text deep-link `#model=...&parts=...&free=...` hashes via `Studio.UrlPayload`,
  - Combined racer hashes such as `#track=quarry-switchbacks&seed=42&b/...`,
  - Raw LDraw `.ldr` / `.mpd` text pasted directly or loaded via `initBricksRuntime`,
  - Curated built-in starter soapbox presets.

-}

import Base64
import Flate
import LDraw.Document as Document exposing (Document)
import Racer.Types exposing (PresetBuild, TrackId(..), trackIdFromString, trackIdToString)
import Studio.HashPayload as HashPayload
import Studio.UrlPayload as UrlPayload
import Url


{-| Parsed components extracted from a URL or fragment string.
-}
type alias ParsedHash =
    { trackId : Maybe TrackId
    , seed : Maybe Int
    , base64Payload : Maybe String
    , rawFragment : String
    , hasModelParam : Bool
    }


{-| Result of decoding a URL, hash, or pasted LDraw source.
-}
type alias DecodedRaceInput =
    { document : Document
    , suggestedName : String
    , trackId : Maybe TrackId
    , seed : Maybe Int
    }


{-| Structured ingestion error cases.
-}
type IngestError
    = EmptyInput
    | InvalidBinaryPayload
    | InvalidModelText
    | NoPlacementsInModel


{-| Decode input bytes (optional, from `url-hash-codec.js`) and/or a URL/hash/LDraw text.
-}
decodeRaceInput : Maybe (List Int) -> String -> Result IngestError DecodedRaceInput
decodeRaceInput maybeInitialBytes rawInput =
    let
        trimmed =
            String.trim rawInput
    in
    case maybeInitialBytes |> Maybe.andThen HashPayload.decodeBytes of
        Just payload ->
            let
                parsed =
                    parseHash trimmed
            in
            documentResult "Imported Racer" parsed.trackId parsed.seed payload.document

        Nothing ->
            if String.isEmpty trimmed then
                Err EmptyInput

            else if isRawLdrText trimmed then
                let
                    doc =
                        Document.parse trimmed
                in
                documentResult (extractModelTitle doc) Nothing Nothing doc

            else
                let
                    parsed =
                        parseHash trimmed
                in
                case parsed.base64Payload of
                    Just b64 ->
                        case decodeBase64Payload b64 of
                            Just doc ->
                                documentResult (extractModelTitle doc) parsed.trackId parsed.seed doc

                            Nothing ->
                                Err InvalidBinaryPayload

                    Nothing ->
                        if parsed.hasModelParam then
                            let
                                urlPayload =
                                    UrlPayload.decodeHash (hashWithoutTrackParams trimmed)
                            in
                            case urlPayload.modelText of
                                Just ldrText ->
                                    let
                                        doc =
                                            Document.parse ldrText
                                    in
                                    documentResult (extractModelTitle doc) parsed.trackId parsed.seed doc

                                Nothing ->
                                    Err InvalidModelText

                        else
                            Err InvalidModelText


documentResult : String -> Maybe TrackId -> Maybe Int -> Document -> Result IngestError DecodedRaceInput
documentResult defaultName maybeTrack maybeSeed doc =
    if List.isEmpty (Document.mainPlacements doc) then
        Err NoPlacementsInModel

    else
        let
            title =
                let
                    candidate =
                        extractModelTitle doc
                in
                if String.isEmpty candidate then
                    defaultName

                else
                    candidate
        in
        Ok
            { document = doc
            , suggestedName = title
            , trackId = maybeTrack
            , seed = maybeSeed
            }


{-| Encode a track, seed, and LDraw document into a shareable `#track=...&seed=...&b/...` hash.
-}
encodeRaceHash : TrackId -> Int -> Document -> String
encodeRaceHash trackId seed doc =
    let
        rawBytes =
            HashPayload.encode
                { mode = HashPayload.FreeBuilding
                , document = doc
                , kit = Nothing
                }

        compressed =
            Flate.deflate rawBytes

        b64Url =
            Base64.fromBytes compressed
                |> Maybe.map HashPayload.standardToBase64Url
                |> Maybe.withDefault ""
    in
    "#track="
        ++ trackIdToString trackId
        ++ "&seed="
        ++ String.fromInt seed
        ++ "&b/"
        ++ b64Url


{-| Parse a full URL or hash string into its racer track, seed, and payload fields.
-}
parseHash : String -> ParsedHash
parseHash rawInput =
    let
        fragment =
            HashPayload.extractFragment (String.trim rawInput)
                |> String.trim

        segments =
            String.split "&" fragment
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        trackId =
            segments
                |> List.filterMap (extractKeyValue "track=")
                |> List.head
                |> Maybe.andThen trackIdFromString

        seed =
            segments
                |> List.filterMap (extractKeyValue "seed=")
                |> List.head
                |> Maybe.andThen String.toInt

        base64Payload =
            HashPayload.parseDirectBase64 rawInput
                |> Maybe.andThen
                    (\val ->
                        if String.isEmpty (String.trim val) then
                            Nothing

                        else
                            Just (String.trim val)
                    )

        hasModelParam =
            List.any (\seg -> String.startsWith "model=" seg) segments
                || String.contains "model=" fragment
    in
    { trackId = trackId
    , seed = seed
    , base64Payload = base64Payload
    , rawFragment = fragment
    , hasModelParam = hasModelParam
    }


{-| Strip racer-specific `track=` and `seed=` params so `Studio.UrlPayload.decodeHash`
receives clean `model=...&parts=...&free=...` tokens.
-}
hashWithoutTrackParams : String -> String
hashWithoutTrackParams rawInput =
    let
        fragment =
            HashPayload.extractFragment (String.trim rawInput)

        filtered =
            fragment
                |> String.split "&"
                |> List.filter
                    (\seg ->
                        let
                            t =
                                String.trim seg
                        in
                        not (String.startsWith "track=" t) && not (String.startsWith "seed=" t)
                    )
    in
    String.join "&" filtered


{-| Detect whether raw text looks like LDraw `.ldr`/`.mpd` source rather than a URL.
-}
isRawLdrText : String -> Bool
isRawLdrText input =
    let
        lines =
            String.lines (String.trim input)
    in
    List.any
        (\line ->
            let
                t =
                    String.trim line
            in
            String.startsWith "1 " t
                || String.startsWith "0 FILE" t
                || String.startsWith "0 Name:" t
                || String.startsWith "11 " t
        )
        lines


decodeBase64Payload : String -> Maybe Document
decodeBase64Payload b64 =
    Base64.toBytes (HashPayload.base64UrlToStandard b64)
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen HashPayload.decode
        |> Maybe.map .document


extractKeyValue : String -> String -> Maybe String
extractKeyValue prefix seg =
    if String.startsWith prefix seg then
        let
            rawVal =
                String.dropLeft (String.length prefix) seg
        in
        Url.percentDecode rawVal
            |> Maybe.withDefault rawVal
            |> Just

    else
        Nothing


extractModelTitle : Document -> String
extractModelTitle doc =
    let
        mainSub =
            Document.mainSubmodel doc

        findMeta entry =
            case entry of
                Document.Raw raw ->
                    let
                        t =
                            String.trim raw
                    in
                    if String.startsWith "0 Name:" t then
                        Just (String.trim (String.dropLeft 7 t))

                    else if String.startsWith "0 FILE " t then
                        Just (String.trim (String.dropLeft 7 t))

                    else
                        Nothing

                Document.PlacementEntry _ ->
                    Nothing
    in
    case List.filterMap findMeta mainSub.entries |> List.head of
        Just title ->
            if String.endsWith ".ldr" (String.toLower title) then
                String.dropRight 4 title

            else
                title

        Nothing ->
            if String.isEmpty (String.trim mainSub.name) then
                "Brick Soapbox"

            else
                mainSub.name


{-| Look up a built-in starter soapbox preset by ID.
-}
findPresetById : String -> Maybe PresetBuild
findPresetById id =
    presets
        |> List.filter (\p -> p.id == id)
        |> List.head


{-| Built-in starter soapboxes ready to inspect and race offline or online.
-}
presets : List PresetBuild
presets =
    [ { id = "turntable-kart"
      , nameEn = "Turntable GP Kart"
      , nameFi = "Kääntöpöytä-karting"
      , summaryEn = "Real front 2x2 turntable steering, balloon tires, and sloped nosecone."
      , summaryFi = "Aito etuakselin kääntöpöytäohjaus, ilmakumirenkaat ja viisto keula."
      , ldrText = turntableKartLdr
      }
    , { id = "pinewood-classic"
      , nameEn = "Pinewood Plank Racer"
      , nameFi = "Klassinen mäkiauto"
      , summaryEn = "Light brick plank with fixed wheel holders — pure soapbox momentum!"
      , summaryFi = "Kevyt palikkalankku kiinteillä pyöränkiinnikkeillä – aitoa mäkiautotuntumaa!"
      , ldrText = pinewoodClassicLdr
      }
    , { id = "alpine-crawler"
      , nameEn = "Alpine Trail Crawler"
      , nameFi = "Alppimaasturi"
      , summaryEn = "High-clearance 37x18 off-road tires and heavy Technic turntable chassis."
      , summaryFi = "Korkea maavara, 37x18-maastorenkaat ja tukeva Technic-ohjausrunko."
      , ldrText = alpineCrawlerLdr
      }
    , { id = "trike-wedge"
      , nameEn = "Streamlined Brick Trike"
      , nameFi = "Virtaviivainen kolmipyörä"
      , summaryEn = "3-wheel delta stance with wide rear track and ultra-low aero wedge."
      , summaryFi = "Kolmipyöräinen delta-rakenne, leveä taka-akseli ja matala ilmanvastus."
      , ldrText = trikeWedgeLdr
      }
    ]


turntableKartLdr : String
turntableKartLdr =
    String.join "\n"
        [ "0 FILE turntable-kart.ldr"
        , "0 Name: Turntable GP Kart"
        , "0 // Rear axle plate + balloon wheels (+Z LDraw = rear)"
        , "1 72 0 0 60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 71 28 5 60 0 0 1 0 1 0 -1 0 0 6014.dat"
        , "1 0 28 5 60 0 0 1 0 1 0 -1 0 0 56890.dat"
        , "1 71 -28 5 60 0 0 -1 0 1 0 1 0 0 6014.dat"
        , "1 0 -28 5 60 0 0 -1 0 1 0 1 0 0 56890.dat"
        , "1 72 0 -8 60 1 0 0 0 1 0 0 0 1 3022.dat"
        , "0 // Articulated front turntable bogie (-Z LDraw = front)"
        , "1 72 0 -8 -60 1 0 0 0 1 0 0 0 1 3680.dat"
        , "1 14 0 -8 -60 1 0 0 0 1 0 0 0 1 3679.dat"
        , "1 72 0 0 -60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 71 28 5 -60 0 0 1 0 1 0 -1 0 0 6014.dat"
        , "1 0 28 5 -60 0 0 1 0 1 0 -1 0 0 56890.dat"
        , "1 71 -28 5 -60 0 0 -1 0 1 0 1 0 0 6014.dat"
        , "1 0 -28 5 -60 0 0 -1 0 1 0 1 0 0 56890.dat"
        , "0 // Main soapbox chassis & cockpit"
        , "1 4 0 -16 0 0 0 1 0 1 0 -1 0 0 3034.dat"
        , "1 4 0 -24 10 0 0 1 0 1 0 -1 0 0 3020.dat"
        , "1 1 0 -24 30 1 0 0 0 1 0 0 0 1 4079.dat"
        , "1 0 0 -24 -20 1 0 0 0 1 0 0 0 1 3829c01.dat"
        , "0 // Sloped aerodynamic cowl & tail"
        , "1 14 0 -40 -60 1 0 0 0 1 0 0 0 1 3039.dat"
        , "1 4 0 -40 65 -1 0 0 0 1 0 0 0 -1 3039.dat"
        ]


pinewoodClassicLdr : String
pinewoodClassicLdr =
    String.join "\n"
        [ "0 FILE pinewood-classic.ldr"
        , "0 Name: Pinewood Plank Racer"
        , "0 // Fixed wheel holders front (-Z) & rear (+Z)"
        , "1 72 0 0 60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 15 28 5 60 0 0 1 0 1 0 -1 0 0 4624.dat"
        , "1 0 28 5 60 0 0 1 0 1 0 -1 0 0 3641.dat"
        , "1 15 -28 5 60 0 0 -1 0 1 0 1 0 0 4624.dat"
        , "1 0 -28 5 60 0 0 -1 0 1 0 1 0 0 3641.dat"
        , "1 72 0 0 -60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 15 28 5 -60 0 0 1 0 1 0 -1 0 0 4624.dat"
        , "1 0 28 5 -60 0 0 1 0 1 0 -1 0 0 3641.dat"
        , "1 15 -28 5 -60 0 0 -1 0 1 0 1 0 0 4624.dat"
        , "1 0 -28 5 -60 0 0 -1 0 1 0 1 0 0 3641.dat"
        , "0 // Classic red plank & yellow nose"
        , "1 4 0 -8 0 0 0 1 0 1 0 -1 0 0 3034.dat"
        , "1 14 0 -32 -60 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 4 0 -16 30 1 0 0 0 1 0 0 0 1 4079.dat"
        , "1 0 0 -16 -20 1 0 0 0 1 0 0 0 1 3829c01.dat"
        ]


alpineCrawlerLdr : String
alpineCrawlerLdr =
    String.join "\n"
        [ "0 FILE alpine-crawler.ldr"
        , "0 Name: Alpine Trail Crawler"
        , "0 // Rear off-road axle (+Z LDraw = rear)"
        , "1 72 0 0 70 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 14 32 5 70 0 0 1 0 1 0 -1 0 0 55981.dat"
        , "1 0 32 5 70 0 0 1 0 1 0 -1 0 0 56891.dat"
        , "1 14 -32 5 70 0 0 -1 0 1 0 1 0 0 55981.dat"
        , "1 0 -32 5 70 0 0 -1 0 1 0 1 0 0 56891.dat"
        , "1 72 0 -24 70 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 72 0 -32 70 1 0 0 0 1 0 0 0 1 3022.dat"
        , "1 72 0 -40 70 1 0 0 0 1 0 0 0 1 3022.dat"
        , "0 // Heavy Technic turntable front bogie (-Z LDraw = front)"
        , "1 72 0 -16 -70 1 0 0 0 1 0 0 0 1 99009.dat"
        , "1 14 0 -16 -70 1 0 0 0 1 0 0 0 1 99010.dat"
        , "1 72 0 0 -70 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 14 32 5 -70 0 0 1 0 1 0 -1 0 0 55981.dat"
        , "1 0 32 5 -70 0 0 1 0 1 0 -1 0 0 56891.dat"
        , "1 14 -32 5 -70 0 0 -1 0 1 0 1 0 0 55981.dat"
        , "1 0 -32 5 -70 0 0 -1 0 1 0 1 0 0 56891.dat"
        , "0 // Raised chassis & roll structure"
        , "1 1 0 -48 0 0 0 1 0 1 0 -1 0 0 3034.dat"
        , "1 72 0 -72 10 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 15 0 -56 40 1 0 0 0 1 0 0 0 1 4079.dat"
        , "1 0 0 -56 -20 1 0 0 0 1 0 0 0 1 3829c01.dat"
        ]


trikeWedgeLdr : String
trikeWedgeLdr =
    String.join "\n"
        [ "0 FILE trike-wedge.ldr"
        , "0 Name: Streamlined Brick Trike"
        , "0 // Single steerable front wheel (-Z LDraw = front)"
        , "1 72 0 -8 -70 1 0 0 0 1 0 0 0 1 3680.dat"
        , "1 14 0 -8 -70 1 0 0 0 1 0 0 0 1 3679.dat"
        , "1 14 0 5 -70 0 0 1 0 1 0 -1 0 0 6014.dat"
        , "1 0 0 5 -70 0 0 1 0 1 0 -1 0 0 56890.dat"
        , "0 // Wide rear stance (+Z LDraw = rear)"
        , "1 72 0 0 55 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 72 0 -8 55 1 0 0 0 1 0 0 0 1 3022.dat"
        , "1 15 36 5 55 0 0 1 0 1 0 -1 0 0 6014.dat"
        , "1 0 36 5 55 0 0 1 0 1 0 -1 0 0 56890.dat"
        , "1 15 -36 5 55 0 0 -1 0 1 0 1 0 0 6014.dat"
        , "1 0 -36 5 55 0 0 -1 0 1 0 1 0 0 56890.dat"
        , "0 // Aero delta body"
        , "1 14 0 -16 0 0 0 1 0 1 0 -1 0 0 3034.dat"
        , "1 14 0 -40 -50 1 0 0 0 1 0 0 0 1 3039.dat"
        , "1 4 0 -24 25 1 0 0 0 1 0 0 0 1 4079.dat"
        ]
