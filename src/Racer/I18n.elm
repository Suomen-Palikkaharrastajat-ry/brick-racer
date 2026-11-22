module Racer.I18n exposing
    ( audioMutedLabel
    , audioUnmutedLabel
    , backToMenuLabel
    , camChaseLabel
    , camHoodLabel
    , camOrbitLabel
    , changeCarLabel
    , checkpointBanner
    , controlsSummary
    , coursesHeading
    , crashReasonText
    , defaultPresetSectionTitle
    , dropHintLabel
    , dropZoneButton
    , elevationRemainingLabel
    , finishedHeadline
    , garageClearanceLabel
    , garageInspectTitle
    , garageMassLabel
    , garageReadyToRoll
    , garageResolveProgress
    , garageSteeringLabel
    , garageTooFewWheelsHint
    , garageWheelsLabel
    , goBanner
    , handbrakeHint
    , inspectCarButton
    , menuSubtitle
    , menuTitle
    , newRecordBadge
    , pasteButtonLabel
    , pasteErrorFallback
    , pastePlaceholder
    , pauseLabel
    , pausedHeadline
    , raceAgainButton
    , raceAnywayButton
    , recoveryHint
    , resetCheckpointLabel
    , resumeButton
    , seedLabel
    , shareRaceUrlButton
    , shareUrlCopiedNotice
    , smashBanner
    , splitLabel
    , startRaceButton
    , steeringMechName
    , surfaceName
    , tiltDisabledLabel
    , tiltEnabledLabel
    , trackDescription
    , trackTitle
    , validationTitle
    , warningOffTrack
    , warningScraping
    , warningTipping
    )

{-| Bilingual (`En` / `Fi`) copy for Brick Racer menus, garage diagnostics, HUD, and overlays.
-}

import I18n exposing (Language(..))
import Racer.Types exposing (CrashReason(..), SteeringMechanism(..), SurfaceType(..), TrackId(..), ValidationCode(..))


{-| Main game title.
-}
menuTitle : Language -> String
menuTitle lang =
    case lang of
        Fi ->
            "Palikka-Mäkiauto"

        En ->
            "Brick Racer"


{-| Main game subtitle explaining the brick-family build-to-race workflow.
-}
menuSubtitle : Language -> String
menuSubtitle lang =
    case lang of
        Fi ->
            "Liitä linkki Brick Studiosta tai Brick Playsta ja ohjaa rakennelmasi alas jyrkkiä maalaisteitä painovoiman avulla."

        En ->
            "Paste a build link from Brick Studio or Brick Play and steer your soapbox down steep countryside lanes under gravity."


{-| Heading above the course selector.
-}
coursesHeading : Language -> String
coursesHeading lang =
    case lang of
        Fi ->
            "Maalaisradat"

        En ->
            "Countryside Courses"


{-| Short banner when the racer smashes a piece of roadside scenery.
-}
smashBanner : Language -> String
smashBanner lang =
    case lang of
        Fi ->
            "Kräsh!"

        En ->
            "Smash!"


{-| Track title per course ID.
-}
trackTitle : Language -> TrackId -> String
trackTitle lang id =
    case ( lang, id ) of
        ( Fi, FarmLane ) ->
            "Tilan Kujanne"

        ( En, FarmLane ) ->
            "Farm Lane Drop"

        ( Fi, ForestTrail ) ->
            "Metsäpolku"

        ( En, ForestTrail ) ->
            "Forest Trail"

        ( Fi, MeadowPath ) ->
            "Niittypolku"

        ( En, MeadowPath ) ->
            "Meadow Footpath"


{-| Short course blurb shown on track selector cards.
-}
trackDescription : Language -> TrackId -> String
trackDescription lang id =
    case ( lang, id ) of
        ( Fi, FarmLane ) ->
            "0,5 km / −150 m • Hiekkatie ja sora, kaksi mutkaa, jyrkkä lasku pihaan"

        ( En, FarmLane ) ->
            "0.5 km / -150 m • Dirt and gravel lane, two bends, steep drop into the farmyard"

        ( Fi, ForestTrail ) ->
            "0,55 km / −175 m • Juurakkoinen metsätie, soraränni ja tukkihyppy"

        ( En, ForestTrail ) ->
            "0.55 km / -175 m • Rooty forest track, gravel chute, log hump"

        ( Fi, MeadowPath ) ->
            "0,45 km / −150 m • Kapea ja jyrkin: ruohopolku, kumpu ja mättäät"

        ( En, MeadowPath ) ->
            "0.45 km / -150 m • Narrowest and steepest: grass footpath, hump, tussocks"


{-| Paste input placeholder text.
-}
pastePlaceholder : Language -> String
pastePlaceholder lang =
    case lang of
        Fi ->
            "Liitä #b/... tai #model=... linkki tai LDraw-teksti..."

        En ->
            "Paste #b/... or #model=... URL, or raw LDraw .ldr text..."


{-| Load pasted build CTA label.
-}
pasteButtonLabel : Language -> String
pasteButtonLabel lang =
    case lang of
        Fi ->
            "Avaa rakennelma"

        En ->
            "Load Build"


{-| Upload `.ldr` / `.mpd` / `.io` button label.
-}
dropZoneButton : Language -> String
dropZoneButton lang =
    case lang of
        Fi ->
            "Avaa .ldr / .mpd / .io"

        En ->
            "Open .ldr / .mpd / .io"


{-| Drag-and-drop caption below button.
-}
dropHintLabel : Language -> String
dropHintLabel lang =
    case lang of
        Fi ->
            "Tai pudota tiedosto tähän ruudulle"

        En ->
            "Or drag & drop any build file onto this window"


{-| Section title for built-in starter soapboxes.
-}
defaultPresetSectionTitle : Language -> String
defaultPresetSectionTitle lang =
    case lang of
        Fi ->
            "Valmiit mäkiautot"

        En ->
            "Starter Soapbox Racers"


{-| Track seed input label.
-}
seedLabel : Language -> String
seedLabel lang =
    case lang of
        Fi ->
            "Ratasiemen"

        En ->
            "Track Seed"


{-| Generic paste decode error fallback.
-}
pasteErrorFallback : Language -> String
pasteErrorFallback lang =
    case lang of
        Fi ->
            "Linkkiä tai mallia ei voitu lukea."

        En ->
            "Could not parse model URL hash or LDraw text."


{-| Garage header title.
-}
garageInspectTitle : Language -> String
garageInspectTitle lang =
    case lang of
        Fi ->
            "Varikkotarkastus & Mekaniikka"

        En ->
            "Garage Inspection & Chassis Spec"


{-| Garage back to menu label.
-}
backToMenuLabel : Language -> String
backToMenuLabel lang =
    case lang of
        Fi ->
            "Takaisin valikkoon"

        En ->
            "Back to Menu"


{-| Garage primary drop button.
-}
startRaceButton : Language -> String
startRaceButton lang =
    case lang of
        Fi ->
            "Laske mäkeä alas!"

        En ->
            "Drop Down the Hill!"


{-| Button to bypass non-blocking warnings cleanly.
-}
raceAnywayButton : Language -> String
raceAnywayButton lang =
    case lang of
        Fi ->
            "Aja varoituksista huolimatta"

        En ->
            "Race Anyway"


{-| Hint shown when fewer than 3 wheels block starting.
-}
garageTooFewWheelsHint : Language -> String
garageTooFewWheelsHint lang =
    case lang of
        Fi ->
            "Lisää malliin vähintään 3 rengasta etu- ja taka-akselille Brick Playssa tai Studiossa."

        En ->
            "Add at least 3 wheels across a front & rear axle in Brick Studio or Brick Play to race."


{-| Telemetry card labels in Garage.
-}
garageWheelsLabel : Language -> String
garageWheelsLabel lang =
    case lang of
        Fi ->
            "Renkaat"

        En ->
            "Wheels"


{-| Telemetry card label for vehicle steering system.
-}
garageSteeringLabel : Language -> String
garageSteeringLabel lang =
    case lang of
        Fi ->
            "Ohjaus"

        En ->
            "Steering"


{-| Telemetry card label for vehicle mass.
-}
garageMassLabel : Language -> String
garageMassLabel lang =
    case lang of
        Fi ->
            "Massa (Mäkiauto / LEGO)"

        En ->
            "Mass (Kart / LEGO)"


{-| Telemetry card label for ground clearance.
-}
garageClearanceLabel : Language -> String
garageClearanceLabel lang =
    case lang of
        Fi ->
            "Maavara"

        En ->
            "Clearance"


{-| Ready status badge in Garage.
-}
garageReadyToRoll : Language -> String
garageReadyToRoll lang =
    case lang of
        Fi ->
            "Ajovalmis alusta!"

        En ->
            "Ready to Roll!"


{-| Part resolution progress string in Garage.
-}
garageResolveProgress : Language -> Int -> Int -> String
garageResolveProgress lang loaded total =
    case lang of
        Fi ->
            "Ladataan palikoita: " ++ String.fromInt loaded ++ " / " ++ String.fromInt total

        En ->
            "Resolving parts: " ++ String.fromInt loaded ++ " / " ++ String.fromInt total


{-| Share race hash button text.
-}
shareRaceUrlButton : Language -> String
shareRaceUrlButton lang =
    case lang of
        Fi ->
            "Kopioi kisalinkki"

        En ->
            "Copy Race Link"


{-| Confirmation flash after copying share link.
-}
shareUrlCopiedNotice : Language -> String
shareUrlCopiedNotice lang =
    case lang of
        Fi ->
            "Kisalinkki kopioitu leikepöydälle!"

        En ->
            "Shareable race link copied!"


{-| Human name for detected steering mechanism.
-}
steeringMechName : Language -> SteeringMechanism -> String
steeringMechName lang mech =
    case ( lang, mech ) of
        ( Fi, TurntableBogie _ ) ->
            "Kääntöpöytäohjaus (+25%)"

        ( En, TurntableBogie _ ) ->
            "Turntable Bogie (+25%)"

        ( Fi, TwinKnuckle _ ) ->
            "Nivelohjaus (+22%)"

        ( En, TwinKnuckle _ ) ->
            "Twin Knuckle (+22%)"

        ( Fi, FixedAxleFallback ) ->
            "Painonsiirto (Kiinteä akseli)"

        ( En, FixedAxleFallback ) ->
            "Weight Shift (Virtual Ackermann)"


{-| Localized headline for a `ValidationCode`.
-}
validationTitle : Language -> ValidationCode -> String
validationTitle lang code =
    case ( lang, code ) of
        ( Fi, EmptyModel ) ->
            "Tyhjä malli"

        ( En, EmptyModel ) ->
            "Empty build"

        ( Fi, TooFewWheels n ) ->
            "Liian vähän pyöriä (" ++ String.fromInt n ++ " / 3+)"

        ( En, TooFewWheels n ) ->
            "Too few wheels (" ++ String.fromInt n ++ " / 3+)"

        ( Fi, CollinearStance ) ->
            "Liian kapea tai yhden linjan akseliväli"

        ( En, CollinearStance ) ->
            "Wheels aligned on a single line"

        ( Fi, BellyScrape _ ) ->
            "Matala maavara: pohja raapii maata"

        ( En, BellyScrape _ ) ->
            "Low clearance: chassis scrapes ground"

        ( Fi, HighCenterOfMass _ ) ->
            "Korkea painopiste: herkkä kaatumaan"

        ( En, HighCenterOfMass _ ) ->
            "High Center of Mass: tippy in turns"

        ( Fi, DisconnectedBricks n ) ->
            String.fromInt n ++ " irrallista palikkaa"

        ( En, DisconnectedBricks n ) ->
            String.fromInt n ++ " disconnected brick(s)"

        ( Fi, FixedAxleTip ) ->
            "Vinkki: lisää kääntöpöytä (3679/3680) terävämpään ohjaukseen"

        ( En, FixedAxleTip ) ->
            "Tip: add turntable (3679/3680) or hinges for sharper steering"

        ( Fi, MissingPartGeometry n ) ->
            String.fromInt n ++ " osaa käyttää varamallinnusta"

        ( En, MissingPartGeometry n ) ->
            String.fromInt n ++ " part(s) using procedural fallback geometry"

        ( Fi, RubberGripBonus ) ->
            "Kumirenkaat: täysi pito!"

        ( En, RubberGripBonus ) ->
            "Rubber Tires: full cornering grip bonus!"

        ( Fi, AeroWedgeBonus ) ->
            "Virtaviivainen kiila: pieni ilmanvastus!"

        ( En, AeroWedgeBonus ) ->
            "Aerodynamic Wedge: low drag bonus!"

        ( Fi, RealSteeringBonus ) ->
            "Mekaaninen ohjaus havaittu!"

        ( En, RealSteeringBonus ) ->
            "Articulated Mechanical Steering Detected!"


{-| Surface name shown in HUD telemetry pill.
-}
surfaceName : Language -> SurfaceType -> String
surfaceName lang surf =
    case ( lang, surf ) of
        ( Fi, DirtRoad ) ->
            "Hiekkatie"

        ( En, DirtRoad ) ->
            "Dirt"

        ( Fi, GravelRoad ) ->
            "Sora"

        ( En, GravelRoad ) ->
            "Gravel"

        ( Fi, ForestTrack ) ->
            "Metsätie"

        ( En, ForestTrack ) ->
            "Forest track"

        ( Fi, GrassPath ) ->
            "Polku"

        ( En, GrassPath ) ->
            "Footpath"

        ( Fi, RoughVerge ) ->
            "Piennar"

        ( En, RoughVerge ) ->
            "Verge"

        ( Fi, MeadowGrass ) ->
            "Niitty"

        ( En, MeadowGrass ) ->
            "Meadow"


{-| Localized camera mode label.
-}
camChaseLabel : Language -> String
camChaseLabel lang =
    case lang of
        Fi ->
            "Takaa (C)"

        En ->
            "Chase (C)"


{-| Hood cam label.
-}
camHoodLabel : Language -> String
camHoodLabel lang =
    case lang of
        Fi ->
            "Ohjaamo (C)"

        En ->
            "Hood (C)"


{-| Orbit cam label.
-}
camOrbitLabel : Language -> String
camOrbitLabel lang =
    case lang of
        Fi ->
            "Vapaa (C)"

        En ->
            "Orbit (C)"


{-| Audio muted label.
-}
audioMutedLabel : Language -> String
audioMutedLabel lang =
    case lang of
        Fi ->
            "Ääni pois"

        En ->
            "Muted"


{-| Audio unmuted label.
-}
audioUnmutedLabel : Language -> String
audioUnmutedLabel lang =
    case lang of
        Fi ->
            "Ääni päällä"

        En ->
            "Sound On"


{-| Pause label.
-}
pauseLabel : Language -> String
pauseLabel lang =
    case lang of
        Fi ->
            "Tauko (Esc)"

        En ->
            "Pause (Esc)"


{-| Reset checkpoint label.
-}
resetCheckpointLabel : Language -> String
resetCheckpointLabel lang =
    case lang of
        Fi ->
            "Palauta (R)"

        En ->
            "Respawn (R)"


{-| Elevation drop remaining label.
-}
elevationRemainingLabel : Language -> Int -> String
elevationRemainingLabel lang metersLeft =
    case lang of
        Fi ->
            "-" ++ String.fromInt metersLeft ++ " m jäljellä"

        En ->
            "-" ++ String.fromInt metersLeft ++ " m drop left"


{-| Countdown GO word.
-}
goBanner : Language -> String
goBanner lang =
    case lang of
        Fi ->
            "MENOKSI!"

        En ->
            "ROLL!"


{-| Checkpoint reached banner.
-}
checkpointBanner : Language -> Int -> String
checkpointBanner lang idx =
    case lang of
        Fi ->
            "Tarkistuspiste " ++ String.fromInt (idx + 1)

        En ->
            "Checkpoint " ++ String.fromInt (idx + 1)


{-| Split label prefix.
-}
splitLabel : Language -> String
splitLabel lang =
    case lang of
        Fi ->
            "Väliaika"

        En ->
            "Split"


{-| Scraping warning badge.
-}
warningScraping : Language -> String
warningScraping lang =
    case lang of
        Fi ->
            "POHJA RAAPII"

        En ->
            "SCRAPING"


{-| Tipping hazard badge.
-}
warningTipping : Language -> String
warningTipping lang =
    case lang of
        Fi ->
            "KAATUMISVAARA"

        En ->
            "TIPPING!"


{-| Off-track warning badge.
-}
warningOffTrack : Language -> String
warningOffTrack lang =
    case lang of
        Fi ->
            "RADAN ULKONA"

        En ->
            "OFF-TRACK"


{-| Pause modal headline.
-}
pausedHeadline : Language -> String
pausedHeadline lang =
    case lang of
        Fi ->
            "Kisa tauolla"

        En ->
            "Race Paused"


{-| Resume button label.
-}
resumeButton : Language -> String
resumeButton lang =
    case lang of
        Fi ->
            "Jatka laskua"

        En ->
            "Resume"


{-| Inspect car button label.
-}
inspectCarButton : Language -> String
inspectCarButton lang =
    case lang of
        Fi ->
            "Takaisin varikolle"

        En ->
            "Garage / Inspect"


{-| Crash reason headline.
-}
crashReasonText : Language -> CrashReason -> String
crashReasonText lang reason =
    case ( lang, reason ) of
        ( Fi, StructuralCollapse ) ->
            "Palikat levisivät törmäyksessä!"

        ( En, StructuralCollapse ) ->
            "Chassis Collapsed!"

        ( Fi, RolloverTumble ) ->
            "Mäkiauto kierähti ympäri!"

        ( En, RolloverTumble ) ->
            "Rollover Wipeout!"

        ( Fi, OffMountainDrop ) ->
            "Suistuit tieltä!"

        ( En, OffMountainDrop ) ->
            "Off the Road!"


{-| Crash recovery countdown hint.
-}
recoveryHint : Language -> String
recoveryHint lang =
    case lang of
        Fi ->
            "Palautetaan viimeisimmälle tarkistuspisteelle..."

        En ->
            "Recovering to last checkpoint (press R to skip)..."


{-| Finish overlay headline.
-}
finishedHeadline : Language -> String
finishedHeadline lang =
    case lang of
        Fi ->
            "Maali saavutettu!"

        En ->
            "Finish Line Crossed!"


{-| New personal best badge.
-}
newRecordBadge : Language -> String
newRecordBadge lang =
    case lang of
        Fi ->
            "UUSI RATAENNÄTYS!"

        En ->
            "NEW PERSONAL BEST!"


{-| Race again CTA.
-}
raceAgainButton : Language -> String
raceAgainButton lang =
    case lang of
        Fi ->
            "Laske uudelleen"

        En ->
            "Race Again"


{-| Swap car or track button.
-}
changeCarButtonFallback : String
changeCarButtonFallback =
    "Change Car"


{-| Swap car or track CTA.
-}
changeCarLabel : Language -> String
changeCarLabel lang =
    case lang of
        Fi ->
            "Vaihda auto tai rata"

        En ->
            changeCarButtonFallback


{-| Keyboard shortcut summary string.
-}
controlsSummary : Language -> String
controlsSummary lang =
    case lang of
        Fi ->
            "A/D tai Nuolet: Ohjaa • W: Kyyristy • S: Jarru • Välilyönti: Käsijarru • C: Kamera • R: Palauta"

        En ->
            "A/D or Arrows: Steer • W: Aero Tuck • S: Brake • Space: Drift • C: Camera • R: Respawn"


{-| Handbrake pill button label.
-}
handbrakeHint : Language -> String
handbrakeHint lang =
    case lang of
        Fi ->
            "Liuku"

        En ->
            "DRIFT"


{-| Tilt sensor enabled label.
-}
tiltEnabledLabel : Language -> String
tiltEnabledLabel lang =
    case lang of
        Fi ->
            "Kallistusohjaus päällä"

        En ->
            "Tilt Steer On"


{-| Tilt sensor disabled / enable button label.
-}
tiltDisabledLabel : Language -> String
tiltDisabledLabel lang =
    case lang of
        Fi ->
            "Ota kallistusohjaus käyttöön"

        En ->
            "Enable Tilt Steer"
