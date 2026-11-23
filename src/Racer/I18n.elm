module Racer.I18n exposing
    ( audioMutedLabel
    , audioUnmutedLabel
    , backToMenuLabel
    , basesTab
    , bothPlayersReady
    , buildCarHeading
    , buildCarHint
    , buildTab
    , camChaseLabel
    , camHoodLabel
    , camOrbitLabel
    , carsAndBasesTab
    , changeCarLabel
    , checkpointBanner
    , controlsSummary
    , copyRoomLinkButton
    , coursesHeading
    , crashReasonText
    , createGridRoomButton
    , createTwoPlayerRoomButton
    , defaultPresetSectionTitle
    , dropHintLabel
    , dropZoneButton
    , editCarButton
    , elevationRemainingLabel
    , finishedHeadline
    , garageClearanceLabel
    , garageInspectTitle
    , garageMassLabel
    , garageReadyToRoll
    , garageResolveProgress
    , garageSteeringLabel
    , garageTab
    , garageTooFewWheelsHint
    , garageWheelsLabel
    , goBanner
    , handbrakeHint
    , healthLabel
    , hotSeatSingleDesc
    , hotSeatSplitDesc
    , hotSeatTabLabel
    , inspectCarButton
    , joinRoomButton
    , leaveRoomButton
    , menuSubtitle
    , menuTitle
    , needsFixBanner
    , needsPolishBanner
    , networkHostGridDesc
    , networkHostGridTitle
    , networkJoinTitle
    , networkTabLabel
    , networkTwoPlayersDesc
    , networkTwoPlayersTitle
    , newRecordBadge
    , openBuildTab
    , openBuilderButton
    , openBuilderSubtext
    , pasteButtonLabel
    , pasteErrorFallback
    , pastePlaceholder
    , pauseLabel
    , pausedHeadline
    , playerOptionsHeading
    , raceAgainButton
    , raceAnywayButton
    , readyToRaceBanner
    , recoveryHint
    , resetCheckpointLabel
    , resumeButton
    , reviewTab
    , scanQrButton
    , seedLabel
    , shareRaceUrlButton
    , shareUrlCopiedNotice
    , singlePlayerModeLabel
    , smashBanner
    , splitLabel
    , splitScreenModeLabel
    , startMultiplayerRaceButton
    , startRaceButton
    , steeringMechName
    , surfaceName
    , tiltDisabledLabel
    , tiltEnabledLabel
    , trackDescription
    , trackTitle
    , validationDetail
    , validationTitle
    , waitingForHostToStart
    , waitingForPlayerTwo
    , warningOffTrack
    , warningScraping
    , warningTipping
    , wreckedHint
    )

{-| Bilingual (`En` / `Fi`) copy for Brick Racer menus, garage diagnostics, HUD, and overlays.
-}

import I18n exposing (Language(..))
import Racer.Types exposing (CrashReason(..), SteeringMechanism(..), SurfaceType(..), TrackId(..), ValidationCode(..))


{-| Main game title.
-}
menuTitle : Language -> String
menuTitle _ =
    "Mäkiautoleikki"


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
            "1,0 km / −300 m • Hiekkatie ja sora, kaksi mutkaa, jyrkkä lasku pihaan"

        ( En, FarmLane ) ->
            "1.0 km / -300 m • Dirt and gravel lane, two bends, steep drop into the farmyard"

        ( Fi, ForestTrail ) ->
            "1,1 km / −350 m • Juurakkoinen metsätie, soraränni ja tukkihyppy"

        ( En, ForestTrail ) ->
            "1.1 km / -350 m • Rooty forest track, gravel chute, log hump"

        ( Fi, MeadowPath ) ->
            "0,85 km / −300 m • Kapea ja jyrkin: ruohopolku, kumpu ja mättäät"

        ( En, MeadowPath ) ->
            "0.85 km / -300 m • Narrowest and steepest: grass footpath, hump, tussocks"


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
            "Laske mäki alas!"

        En ->
            "Roll downhill!"


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
            "Massa (Mäkiauto / palikat)"

        En ->
            "Mass (Kart / bricks)"


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

        ( Fi, WheelOffGround n ) ->
            String.fromInt n ++ " pyörää ei kosketa maata"

        ( En, WheelOffGround n ) ->
            String.fromInt n ++ " wheel(s) off the ground"

        ( Fi, NoseOverRisk _ ) ->
            "Painopiste lähellä akselia: voi keikahtaa"

        ( En, NoseOverRisk _ ) ->
            "Weight close to an axle: may nose over"

        ( Fi, CoMOutsideSupport ) ->
            "Painopiste pyörien ulkopuolella"

        ( En, CoMOutsideSupport ) ->
            "Centre of mass outside the wheels"

        ( Fi, SteeringNotLinked ) ->
            "Kääntöpöydässä ei ole pyöriä"

        ( En, SteeringNotLinked ) ->
            "Turntable has no wheels on it"


{-| Localized explanation for the validation codes that need one; `Nothing`
for codes whose title says enough.
-}
validationDetail : Language -> ValidationCode -> Maybe String
validationDetail lang code =
    case ( lang, code ) of
        ( Fi, WheelOffGround _ ) ->
            Just "Ilmassa roikkuvat pyörät eivät kanna autoa eikä niitä lasketa pyörien määrään."

        ( En, WheelOffGround _ ) ->
            Just "Wheels that hang in the air carry no weight and do not count toward the wheel total."

        ( Fi, NoseOverRisk _ ) ->
            Just "Siirrä painoa kauemmas akselista tai pidennä akseliväliä."

        ( En, NoseOverRisk _ ) ->
            Just "Move weight away from that axle or lengthen the wheelbase."

        ( Fi, CoMOutsideSupport ) ->
            Just "Auto kaatuu ennen kuin se lähtee rullaamaan. Lisää pyöriä painopisteen toiselle puolelle."

        ( En, CoMOutsideSupport ) ->
            Just "The car tips over before it rolls. Add wheels on the other side of the centre of mass."

        ( Fi, SteeringNotLinked ) ->
            Just "Kiinnitä etuakseli ja pyörät kääntöpöydän alle, jotta se ohjaa."

        ( En, SteeringNotLinked ) ->
            Just "Mount the front axle and wheels under the turntable so it steers."

        _ ->
            Nothing


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
            "Romuttui! Auto hajosi palikoiksi."

        ( En, StructuralCollapse ) ->
            "Wrecked! Your car broke into bricks."

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


{-| Shown under the wreck headline while the broken car is rebuilt.
-}
wreckedHint : Language -> String
wreckedHint lang =
    case lang of
        Fi ->
            "Liikaa kolhuja. Auto kootaan uudelleen viimeisimmällä tarkistuspisteellä..."

        En ->
            "Too many crashes. Rebuilding your car at the last checkpoint..."


{-| HUD label of the car's health bar.
-}
healthLabel : Language -> String
healthLabel lang =
    case lang of
        Fi ->
            "KUNTO"

        En ->
            "HEALTH"


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
            "Kallistusohjaus päällä (napauta: napit)"

        En ->
            "Tilt steering on (tap for buttons)"


{-| Tilt sensor disabled / enable button label.
-}
tiltDisabledLabel : Language -> String
tiltDisabledLabel lang =
    case lang of
        Fi ->
            "Kallistusohjaus pois (napauta: kallista)"

        En ->
            "Tilt steering off (tap to tilt)"


{-| Heading of the menu card that opens the car editor.
-}
buildCarHeading : Language -> String
buildCarHeading lang =
    case lang of
        Fi ->
            "Rakenna oma auto"

        En ->
            "Build your own car"


{-| Hint under the car editor heading: pick a base to build on.
-}
buildCarHint : Language -> String
buildCarHint lang =
    case lang of
        Fi ->
            "Valitse alusta. Pyörät ovat valmiina, rakenna korin palikoista ja väritä se."

        En ->
            "Pick a base. The wheels are on already: build the body and colour it."


{-| Garage button that opens the car in the car editor again.
-}
editCarButton : Language -> String
editCarButton lang =
    case lang of
        Fi ->
            "Muokkaa autoa"

        En ->
            "Edit car"


{-| Open builder button on setup screen.
-}
openBuilderButton : Language -> String
openBuilderButton lang =
    case lang of
        Fi ->
            "Rakenna oma kärry!"

        En ->
            "Build your own cart!"


{-| Subtext under open builder button.
-}
openBuilderSubtext : Language -> String
openBuilderSubtext lang =
    case lang of
        Fi ->
            "Vaihda alustaa, lisää palikoita tai tee varikkotarkastus"

        En ->
            "Change chassis, add bricks or run garage inspection"


{-| 1 Player mode label.
-}
singlePlayerModeLabel : Language -> String
singlePlayerModeLabel lang =
    case lang of
        Fi ->
            "Yksinpeli (1P)"

        En ->
            "Single Player (1P)"


{-| 2 Player split-screen mode label.
-}
splitScreenModeLabel : Language -> String
splitScreenModeLabel lang =
    case lang of
        Fi ->
            "Jaettu ruutu (2P)"

        En ->
            "Split Screen (2P)"


{-| Heading for bottom player options section.
-}
playerOptionsHeading : Language -> String
playerOptionsHeading lang =
    case lang of
        Fi ->
            "Pelaajat & Moninpeli"

        En ->
            "Players & Multiplayer"


{-| Tab label for car presets and starter bases in builder.
-}
carsAndBasesTab : Language -> String
carsAndBasesTab lang =
    case lang of
        Fi ->
            "Autot & alustat"

        En ->
            "Cars & chassis"


{-| Tab label for opening builds in builder.
-}
openBuildTab : Language -> String
openBuildTab lang =
    case lang of
        Fi ->
            "Avaa malli"

        En ->
            "Open build"


{-| Tab label for garage inspection in builder.
-}
garageTab : Language -> String
garageTab lang =
    reviewTab lang


{-| Tab label for evaluation/review in builder.
-}
reviewTab : Language -> String
reviewTab lang =
    case lang of
        Fi ->
            "Arvostele"

        En ->
            "Review"


{-| Tab label for starter bases in builder.
-}
basesTab : Language -> String
basesTab lang =
    case lang of
        Fi ->
            "Alustat"

        En ->
            "Bases"


readyToRaceBanner : Language -> String
readyToRaceBanner lang =
    case lang of
        Fi ->
            "⭐⭐⭐ VALMIS MÄKEEN"

        En ->
            "⭐⭐⭐ READY TO RACE"


needsFixBanner : Language -> String
needsFixBanner lang =
    case lang of
        Fi ->
            "⚠️ VAATII KORJAUSTA"

        En ->
            "⚠️ NEEDS FIX"


needsPolishBanner : Language -> String
needsPolishBanner lang =
    case lang of
        Fi ->
            "ℹ️ VIIMEISTELTÄVISSÄ"

        En ->
            "ℹ️ CAN BE POLISHED"


{-| Tab label for brick palette / parts in builder.
-}
buildTab : Language -> String
buildTab lang =
    case lang of
        Fi ->
            "Palikat"

        En ->
            "Bricks"


{-| Hot seat tab label.
-}
hotSeatTabLabel : Language -> String
hotSeatTabLabel lang =
    case lang of
        Fi ->
            "Paikallispeli (Hot seat)"

        En ->
            "Local Play (Hot seat)"


{-| Network play tab label.
-}
networkTabLabel : Language -> String
networkTabLabel lang =
    case lang of
        Fi ->
            "Verkkopeli (Multiplayer)"

        En ->
            "Network Play (Multiplayer)"


{-| Single player description.
-}
hotSeatSingleDesc : Language -> String
hotSeatSingleDesc lang =
    case lang of
        Fi ->
            "Yksinpeli: aja kelloa vastaan ja paranna radan ennätysaikaa."

        En ->
            "Solo time trial: race against the clock and beat track records."


{-| Split screen description.
-}
hotSeatSplitDesc : Language -> String
hotSeatSplitDesc lang =
    case lang of
        Fi ->
            "Jaettu ruutu tällä laitteella kahdelle pelaajalle (P1: Nuolinäppäimet / P2: WASD)."

        En ->
            "Split-screen on this device for 2 players (P1: Arrows / P2: WASD)."


{-| Network 2 players section title.
-}
networkTwoPlayersTitle : Language -> String
networkTwoPlayersTitle lang =
    case lang of
        Fi ->
            "2 Pelaajan verkkokisa"

        En ->
            "2-Player Network Race"


{-| Network 2 players description.
-}
networkTwoPlayersDesc : Language -> String
networkTwoPlayersDesc lang =
    case lang of
        Fi ->
            "Luo kahden pelaajan huone. Toinen pelaaja skannaa QR-koodin puhelimellaan ja liittyy kisaan."

        En ->
            "Create a 2-player room. The second player scans the QR code with their phone to join."


{-| Network host grid section title.
-}
networkHostGridTitle : Language -> String
networkHostGridTitle lang =
    case lang of
        Fi ->
            "Isännöi moninpeliä (Ruudukko / max 8)"

        En ->
            "Host Multiplayer (Grid / max 8)"


{-| Network host grid description.
-}
networkHostGridDesc : Language -> String
networkHostGridDesc lang =
    case lang of
        Fi ->
            "Isännöi kisaa isolla ruudulla (äly-TV / PC). Pelaajat liittyvät puhelimillaan ja isäntä näyttää live-ruudukkonäkymän."

        En ->
            "Host on a big screen (Smart TV / PC). Players join on phones; host displays the live multi-camera grid."


{-| Network join section title.
-}
networkJoinTitle : Language -> String
networkJoinTitle lang =
    case lang of
        Fi ->
            "Liity peliin"

        En ->
            "Join Room"


{-| Create 2-player room button label.
-}
createTwoPlayerRoomButton : Language -> String
createTwoPlayerRoomButton lang =
    case lang of
        Fi ->
            "Luo 2P-peli"

        En ->
            "Create 2P Race"


{-| Create grid session button label.
-}
createGridRoomButton : Language -> String
createGridRoomButton lang =
    case lang of
        Fi ->
            "Luo ruudukkohuone"

        En ->
            "Create Grid Room"


{-| Join room button label.
-}
joinRoomButton : Language -> String
joinRoomButton lang =
    case lang of
        Fi ->
            "Liity"

        En ->
            "Join"


{-| Scan QR button label.
-}
scanQrButton : Language -> String
scanQrButton lang =
    case lang of
        Fi ->
            "Lue QR-koodi"

        En ->
            "Scan QR"


{-| Copy room link button label.
-}
copyRoomLinkButton : Language -> String
copyRoomLinkButton lang =
    case lang of
        Fi ->
            "Kopioi linkki"

        En ->
            "Copy link"


{-| Waiting for second player status text.
-}
waitingForPlayerTwo : Language -> String
waitingForPlayerTwo lang =
    case lang of
        Fi ->
            "Odotetaan toista pelaajaa (1/2)..."

        En ->
            "Waiting for second player to join (1/2)..."


{-| Both players ready status text.
-}
bothPlayersReady : Language -> String
bothPlayersReady lang =
    case lang of
        Fi ->
            "Molemmat pelaajat liittyneet (2/2)!"

        En ->
            "Both players joined (2/2)!"


{-| Start multiplayer race button label.
-}
startMultiplayerRaceButton : Language -> String
startMultiplayerRaceButton lang =
    case lang of
        Fi ->
            "Aloita kisa!"

        En ->
            "Start Race!"


{-| Leave / cancel room button label.
-}
leaveRoomButton : Language -> String
leaveRoomButton lang =
    case lang of
        Fi ->
            "Poistu huoneesta"

        En ->
            "Leave Room"


{-| Waiting for host to start race status text.
-}
waitingForHostToStart : Language -> String
waitingForHostToStart lang =
    case lang of
        Fi ->
            "Odotetaan isännän käynnistävän kisan..."

        En ->
            "Waiting for host to start the race..."
