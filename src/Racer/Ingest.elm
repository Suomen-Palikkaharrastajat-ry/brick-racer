module Racer.Ingest exposing
    ( DecodedRaceInput
    , IngestError(..)
    , ParsedHash
    , decodeRaceInput
    , encodeGarageHash
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
  - Combined racer hashes such as `#map=boreal-winter-snow-42&b/...` (an old
    `#track=` part is ignored),
  - Raw LDraw `.ldr` / `.mpd` text pasted directly or loaded via `initBricksRuntime`,
  - Curated built-in starter soapbox presets.

-}

import Base64
import Flate
import Game.MapParams as Map exposing (MapParams)
import LDraw.Document as Document exposing (Document)
import Racer.Types exposing (PresetBuild)
import Studio.HashPayload as HashPayload
import Studio.UrlPayload as UrlPayload
import Url


{-| Parsed components extracted from a URL or fragment string.
-}
type alias ParsedHash =
    { map : Maybe MapParams
    , base64Payload : Maybe String
    , rawFragment : String
    , hasModelParam : Bool
    , isGarage : Bool
    }


{-| Result of decoding a URL, hash, or pasted LDraw source.
-}
type alias DecodedRaceInput =
    { document : Document
    , suggestedName : String
    , map : Maybe MapParams
    , isGarage : Bool
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
            documentResult "Imported Racer" parsed.map parsed.isGarage payload.document

        Nothing ->
            if String.isEmpty trimmed then
                Err EmptyInput

            else if isRawLdrText trimmed then
                let
                    doc =
                        Document.parse trimmed
                in
                documentResult (extractModelTitle doc) Nothing False doc

            else
                let
                    parsed =
                        parseHash trimmed
                in
                case parsed.base64Payload of
                    Just b64 ->
                        case decodeBase64Payload b64 of
                            Just doc ->
                                documentResult (extractModelTitle doc) parsed.map parsed.isGarage doc

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
                                    documentResult (extractModelTitle doc) parsed.map parsed.isGarage doc

                                Nothing ->
                                    Err InvalidModelText

                        else if parsed.isGarage then
                            let
                                defaultDoc =
                                    case presets of
                                        first :: _ ->
                                            Document.parse first.ldrText

                                        [] ->
                                            Document.empty
                            in
                            documentResult "Default Soapbox" parsed.map True defaultDoc

                        else
                            Err InvalidModelText


documentResult : String -> Maybe MapParams -> Bool -> Document -> Result IngestError DecodedRaceInput
documentResult defaultName maybeMap isGarage doc =
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
            , map = maybeMap
            , isGarage = isGarage
            }


{-| Encode a map and an LDraw document into a shareable `#map=<code>&b/...` hash.
-}
encodeRaceHash : MapParams -> Document -> String
encodeRaceHash map doc =
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
    "#map="
        ++ Map.toCode map
        ++ "&b/"
        ++ b64Url


{-| Encode a map and an LDraw document into a garage builder `#garage&map=<code>&b/...` hash.
-}
encodeGarageHash : MapParams -> Document -> String
encodeGarageHash map doc =
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
    "#garage&map="
        ++ Map.toCode map
        ++ "&b/"
        ++ b64Url


isGarageSegment : String -> Bool
isGarageSegment seg =
    let
        s =
            String.toLower (String.trim seg)
    in
    s
        == "garage"
        || s
        == "build"
        || s
        == "garage=1"
        || s
        == "build=1"
        || String.startsWith "mode=garage" s
        || String.startsWith "mode=build" s
        || String.startsWith "view=garage" s
        || String.startsWith "view=build" s


{-| Parse a full URL or hash string into its racer map and payload fields.
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

        map =
            segments
                |> List.filterMap (extractKeyValue "map=")
                |> List.head
                |> Maybe.andThen Map.fromCode

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

        isGarage =
            List.any isGarageSegment segments
    in
    { map = map
    , base64Payload = base64Payload
    , rawFragment = fragment
    , hasModelParam = hasModelParam
    , isGarage = isGarage
    }


{-| Strip racer-specific `map=` (and the old `track=` and `seed=`), `mode=`, and `garage` params so `Studio.UrlPayload.decodeHash`
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
                                String.toLower (String.trim seg)
                        in
                        not (String.startsWith "map=" t)
                            && not (String.startsWith "track=" t)
                            && not (String.startsWith "seed=" t)
                            && not (String.startsWith "mode=" t)
                            && not (String.startsWith "view=" t)
                            && not (isGarageSegment t)
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
      , summaryEn = "High-clearance 37x18 off-road tires and heavy pin-and-axle turntable chassis."
      , summaryFi = "Korkea maavara, 37x18-maastorenkaat ja tukeva tappi- ja akselirunko."
      , ldrText = alpineCrawlerLdr
      }
    , { id = "two-wheel-dart"
      , nameEn = "Two-Wheel Dart"
      , nameFi = "Kaksipyöräinen nuoli"
      , summaryEn = "Two small wheels in line: a turntable-steered front wheel and a fixed rear wheel under low rails."
      , summaryFi = "Kaksi pientä pyörää peräkkäin: kääntöpöydällä ohjattu etupyörä ja kiinteä takapyörä matalien kiskojen alla."
      , ldrText = twoWheelDartLdr
      }
    ]


turntableKartLdr : String
turntableKartLdr =
    String.join "\n"
        [ "0 FILE turntable-kart.ldr"
        , "0 Name: Turntable GP Kart"
        , "0 // Rear axle plate + balloon wheels (+Z LDraw = rear)"
        , "1 72 0 0 60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 71 34 5 60 0 0 -1 0 1 0 1 0 0 6014b.dat"
        , "1 0 40 5 60 0 0 1 0 1 0 -1 0 0 56890.dat"
        , "1 71 -34 5 60 0 0 1 0 1 0 -1 0 0 6014b.dat"
        , "1 0 -40 5 60 0 0 -1 0 1 0 1 0 0 56890.dat"
        , "1 72 0 -8 60 1 0 0 0 1 0 0 0 1 3022.dat"
        , "0 // Articulated front turntable bogie (-Z LDraw = front)"
        , "1 72 0 -8 -60 1 0 0 0 1 0 0 0 1 3680.dat"
        , "1 14 0 -8 -60 1 0 0 0 1 0 0 0 1 3679.dat"
        , "1 72 0 0 -60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 71 34 5 -60 0 0 -1 0 1 0 1 0 0 6014b.dat"
        , "1 0 40 5 -60 0 0 1 0 1 0 -1 0 0 56890.dat"
        , "1 71 -34 5 -60 0 0 1 0 1 0 -1 0 0 6014b.dat"
        , "1 0 -40 5 -60 0 0 -1 0 1 0 1 0 0 56890.dat"
        , "0 // Main soapbox chassis & cockpit"
        , "1 4 0 -16 0 0 0 1 0 1 0 -1 0 0 3034.dat"
        , "1 4 0 -24 0 0 0 1 0 1 0 -1 0 0 3020.dat"
        , "1 1 0 -32 0 1 0 0 0 1 0 0 0 1 4079.dat"
        , "1 0 0 -32 -30 1 0 0 0 1 0 0 0 1 3829c01.dat"
        , "0 // Sloped aerodynamic cowl & tail"
        , "1 14 0 -40 -50 1 0 0 0 1 0 0 0 1 3039.dat"
        , "1 4 0 -40 50 -1 0 0 0 1 0 0 0 -1 3039.dat"
        ]


pinewoodClassicLdr : String
pinewoodClassicLdr =
    String.join "\n"
        [ "0 FILE pinewood-classic.ldr"
        , "0 Name: Pinewood Plank Racer"
        , "0 // Fixed wheel holders front (-Z) & rear (+Z)"
        , "1 72 0 0 60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 15 33 5 60 0 0 1 0 1 0 -1 0 0 4624.dat"
        , "1 0 33 5 60 0 0 1 0 1 0 -1 0 0 3641.dat"
        , "1 15 -33 5 60 0 0 -1 0 1 0 1 0 0 4624.dat"
        , "1 0 -33 5 60 0 0 -1 0 1 0 1 0 0 3641.dat"
        , "1 72 0 0 -60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 15 33 5 -60 0 0 1 0 1 0 -1 0 0 4624.dat"
        , "1 0 33 5 -60 0 0 1 0 1 0 -1 0 0 3641.dat"
        , "1 15 -33 5 -60 0 0 -1 0 1 0 1 0 0 4624.dat"
        , "1 0 -33 5 -60 0 0 -1 0 1 0 1 0 0 3641.dat"
        , "0 // Classic red plank & yellow nose"
        , "1 4 0 -8 0 0 0 1 0 1 0 -1 0 0 3034.dat"
        , "1 14 0 -32 -40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 4 0 -16 40 1 0 0 0 1 0 0 0 1 4079.dat"
        , "1 0 0 -16 10 1 0 0 0 1 0 0 0 1 3829c01.dat"
        ]


alpineCrawlerLdr : String
alpineCrawlerLdr =
    String.join "\n"
        [ "0 FILE alpine-crawler.ldr"
        , "0 Name: Alpine Trail Crawler"
        , "0 // Rear off-road axle (+Z LDraw = rear)"
        , "1 72 0 0 60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 14 39 5 60 0 0 1 0 1 0 -1 0 0 55981.dat"
        , "1 0 39 5 60 0 0 -1 0 1 0 1 0 0 56891.dat"
        , "1 14 -39 5 60 0 0 -1 0 1 0 1 0 0 55981.dat"
        , "1 0 -39 5 60 0 0 1 0 1 0 -1 0 0 56891.dat"
        , "1 72 0 -8 60 1 0 0 0 1 0 0 0 1 3022.dat"
        , "1 72 0 -16 60 1 0 0 0 1 0 0 0 1 3022.dat"
        , "0 // Articulated high-clearance turntable front bogie (-Z LDraw = front)"
        , "1 72 0 -8 -60 1 0 0 0 1 0 0 0 1 3680.dat"
        , "1 14 0 -8 -60 1 0 0 0 1 0 0 0 1 3679.dat"
        , "1 72 0 0 -60 1 0 0 0 1 0 0 0 1 4600.dat"
        , "1 14 39 5 -60 0 0 1 0 1 0 -1 0 0 55981.dat"
        , "1 0 39 5 -60 0 0 -1 0 1 0 1 0 0 56891.dat"
        , "1 14 -39 5 -60 0 0 -1 0 1 0 1 0 0 55981.dat"
        , "1 0 -39 5 -60 0 0 1 0 1 0 -1 0 0 56891.dat"
        , "1 72 0 -16 -60 1 0 0 0 1 0 0 0 1 3022.dat"
        , "0 // Raised chassis & roll structure"
        , "1 1 0 -24 0 0 0 1 0 1 0 -1 0 0 3034.dat"
        , "1 72 0 -48 40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 15 0 -32 -40 1 0 0 0 1 0 0 0 1 4079.dat"
        , "1 0 0 -32 -70 1 0 0 0 1 0 0 0 1 3829c01.dat"
        ]


twoWheelDartLdr : String
twoWheelDartLdr =
    String.join "\n"
        [ "0 FILE two-wheel-dart.ldr"
        , "0 Name: Two-Wheel Dart"
        , "0 // Steerable front wheel on a turntable, just left of the centreline (-Z LDraw = front)"
        , "1 72 -40 -8 -60 1 0 0 0 1 0 0 0 1 3680.dat"
        , "1 14 -40 -8 -60 1 0 0 0 1 0 0 0 1 3679.dat"
        , "1 72 -40 0 -60 1 0 0 0 1 0 0 0 1 4488.dat"
        , "1 15 -7 5 -60 0 0 1 0 1 0 -1 0 0 4624.dat"
        , "1 0 -7 5 -60 0 0 1 0 1 0 -1 0 0 3641.dat"
        , "0 // Fixed rear wheel, just right of the centreline (+Z LDraw = rear)"
        , "1 72 40 0 60 -1 0 0 0 1 0 0 0 -1 4488.dat"
        , "1 15 7 5 60 0 0 -1 0 1 0 1 0 0 4624.dat"
        , "1 0 7 5 60 0 0 -1 0 1 0 1 0 0 3641.dat"
        , "1 72 40 -8 60 1 0 0 0 1 0 0 0 1 3022.dat"
        , "0 // Low rails, cross plate and nose"
        , "1 4 40 -16 40 0 0 1 0 1 0 -1 0 0 3795.dat"
        , "1 4 -40 -16 -40 0 0 1 0 1 0 -1 0 0 3020.dat"
        , "1 4 0 -24 -10 1 0 0 0 1 0 0 0 1 3666.dat"
        , "1 14 -40 -40 -50 1 0 0 0 1 0 0 0 1 3039.dat"
        ]
