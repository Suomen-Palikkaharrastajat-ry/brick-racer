module Pages.Build exposing
    ( BuilderTab(..)
    , Model
    , Msg(..)
    , Outcome(..)
    , document
    , init
    , resize
    , setLanguage
    , setPartCacheAndIndex
    , subscriptions
    , update
    , view
    )

{-| The car editor (`BuildPage`): a Brick Play builder for one car.

Includes the 3D builder `Studio.Core`, the parts dock, starter bases & preset cars,
URL/file/QR model import, and live mechanical inspection & chassis specs.

-}

import DesignTokens.Colors as Brand
import Dict exposing (Dict)
import FeatherIcons
import Game.Ports as GamePorts
import Html exposing (Html, button, div, h3, input, span, text, textarea)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import I18n exposing (Language(..))
import LDraw.Catalog as Catalog exposing (PartIndex)
import LDraw.ColorData as ColorData
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache)
import Ports
import Racer.Analyze as Analyze
import Racer.Editor as Editor exposing (Base, PartEntry)
import Racer.GarageFloor as GarageFloor
import Racer.I18n as RI18n
import Racer.Ingest as Ingest
import Racer.Types exposing (PresetBuild, SteeringMechanism(..), ValidationSeverity(..))
import Set
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.Shortcuts exposing (TurnDirection(..))
import Svg
import Svg.Attributes as SvgAttr
import UI.PartCard as PartCard
import Viewport


{-| Tabs available within the builder experience.
-}
type BuilderTab
    = BuildBricksTab
    | AllBricksTab
    | BasesTab
    | ImportBuildTab
    | ReviewTab


{-| Editor state: studio, base, active tab, thumbnails, import inputs, and part cache/index.
-}
type alias Model =
    { studio : Core.Model
    , base : Maybe Base
    , baseLocked : Bool
    , colour : Int
    , language : Language
    , width : Int
    , height : Int
    , thumbnails : Dict String String
    , notice : Maybe String
    , activeTab : BuilderTab
    , pasteText : String
    , pasteError : Maybe String
    , partCache : PartCache
    , partIndex : PartIndex
    }


{-| Editor messages.
-}
type Msg
    = StudioMsg Core.Msg
    | PickColour Int
    | AddPart String
    | Turn TurnDirection
    | Raise
    | Lower
    | DeleteSelected
    | Undo
    | Redo
    | FitView
    | Finish
    | Leave
    | ThumbnailLoaded { file : String, color : Int, url : String }
    | SelectTab BuilderTab
    | SelectPreset PresetBuild
    | SelectBase Base
    | ToggleBaseLock
    | PasteTextChanged String
    | SubmitPaste
    | RequestFileUpload
    | OpenQrScanner
    | LoadExternalDocument Document (Maybe Base)


{-| What the editor asks `Main` to do after an update.
-}
type Outcome
    = Stay
    | Quit
    | Finished Document
    | TriggerFileUpload
    | TriggerQrScanner


{-| Open the editor on a build that optionally stands on `base`.
-}
init :
    { ldrawBase : String, ldrawFallbackBase : String, supportsPointerEvents : Bool, language : Language, width : Int, height : Int }
    -> Maybe Base
    -> Document
    -> ( Model, Cmd Msg )
init config maybeBase doc =
    let
        ( studio, studioCmd ) =
            Core.init
                { ldrawBase = config.ldrawBase
                , ldrawFallbackBase = config.ldrawFallbackBase
                , defaultModelUrl = ""
                , initialHash = ""
                , supportsPointerEvents = config.supportsPointerEvents
                , document = doc
                , gridStep = Just 20
                , rotateStep = Just 90
                , snapDistance = Just 24.0
                , collisionMode = Nothing
                }
                |> Tuple.mapFirst
                    (Core.setGizmoStyle Core.NoGizmo
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                        >> Core.setTwoFingerOrbits True
                        >> Core.setGlueEditable False
                        >> Core.setPasteEnabled False
                        >> Core.setNudgeSteps { horizontal = 20, vertical = 8 }
                    )

        ( fittedStudio, fitCmd ) =
            Core.fitView studio

        model =
            { studio = fittedStudio
            , base = maybeBase
            , baseLocked = maybeBase /= Nothing
            , colour = Editor.defaultColour
            , language = config.language
            , width = config.width
            , height = config.height
            , thumbnails = Dict.empty
            , notice = Nothing
            , activeTab = BuildBricksTab
            , pasteText = ""
            , pasteError = Nothing
            , partCache = Dict.empty
            , partIndex = Catalog.emptyPartIndex
            }

        ( sized, sizeCmd ) =
            resize config.width config.height model
    in
    ( sized
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, Cmd.map StudioMsg fitCmd, sizeCmd, requestThumbnails sized ]
    )


{-| The build as it stands.
-}
document : Model -> Document
document model =
    Core.getDocument model.studio


{-| Inject part cache and index for live chassis validation.
-}
setPartCacheAndIndex : PartCache -> PartIndex -> Model -> Model
setPartCacheAndIndex cache index model =
    { model | partCache = cache, partIndex = index }


{-| Follow a change of the window size.
-}
resize : Int -> Int -> Model -> ( Model, Cmd Msg )
resize width height model =
    let
        sized =
            { model | width = width, height = height }

        ( next, cmd ) =
            runCore (Core.update (Core.ViewportMsg (Viewport.WindowResize width (canvasHeight sized)))) sized
    in
    ( next, cmd )


{-| Follow a change of the language.
-}
setLanguage : Language -> Model -> Model
setLanguage language model =
    { model | language = language }


{-| Handle an editor message.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Outcome )
update msg model =
    let
        stay ( m, c ) =
            ( m, c, Stay )
    in
    case msg of
        StudioMsg studioMsg ->
            stay (runCore (Core.update studioMsg) model)

        PickColour code ->
            let
                picked =
                    { model | colour = code, notice = Nothing }
            in
            if Core.hasSelection model.studio && not (baseSelected model) then
                let
                    ( next, cmd ) =
                        runCore (Core.recolorSelection code) picked
                in
                ( next, Cmd.batch [ cmd, requestThumbnails next ], Stay )

            else if Core.hasSelection model.studio && baseSelected model then
                ( { picked | notice = Just (keepBaseColours model.language) }, Cmd.none, Stay )

            else
                ( picked, requestThumbnails picked, Stay )

        AddPart file ->
            let
                matrix =
                    Editor.dropMatrix (Core.getDocument model.studio) model.studio.selection file

                partName =
                    Editor.parts
                        |> List.filter (\p -> p.file == file)
                        |> List.head
                        |> Maybe.map
                            (\p ->
                                case model.language of
                                    Fi ->
                                        p.nameFi

                                    En ->
                                        p.nameEn
                            )
                        |> Maybe.withDefault file

                noticeText =
                    case model.language of
                        Fi ->
                            "➕ Lisätty: " ++ partName

                        En ->
                            "➕ Added: " ++ partName
            in
            stay (runCore (Core.addPartAt file model.colour matrix) { model | notice = Just noticeText })

        Turn direction ->
            stay (runCore (Core.turnSelected direction) model)

        Raise ->
            stay (runCore Core.raiseSelected model)

        Lower ->
            stay (runCore Core.lowerSelected model)

        DeleteSelected ->
            if baseSelected model then
                ( { model | notice = Just (keepBaseText model.language) }, Cmd.none, Stay )

            else
                stay (runCore Core.deleteSelection model)

        Undo ->
            stay (runCore Core.undo model)

        Redo ->
            stay (runCore Core.redo model)

        FitView ->
            let
                ( studio, cmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = studio }, Cmd.map StudioMsg cmd, Stay )

        Finish ->
            ( model, Cmd.none, Finished (Core.getDocument model.studio) )

        Leave ->
            ( model, Cmd.none, Quit )

        ThumbnailLoaded thumb ->
            ( { model | thumbnails = Dict.insert (PartCard.thumbnailKey thumb.file thumb.color) thumb.url model.thumbnails }
            , Cmd.none
            , Stay
            )

        SelectTab tab ->
            let
                updated =
                    { model | activeTab = tab, notice = Nothing }

                ( next, cmd ) =
                    runCore (Core.update (Core.ViewportMsg (Viewport.WindowResize model.width (canvasHeight updated)))) updated
            in
            ( next, cmd, Stay )

        SelectPreset preset ->
            let
                doc =
                    Document.parse preset.ldrText

                newBase =
                    Editor.baseOf doc

                ( nextStudio, cmd, events ) =
                    Core.resetDocument doc model.studio

                ( fittedStudio, fitCmd ) =
                    Core.fitView nextStudio

                updated =
                    { model
                        | studio = fittedStudio
                        , base = newBase
                        , baseLocked = newBase /= Nothing
                        , activeTab = BuildBricksTab
                        , notice = Nothing
                    }
            in
            ( updated
            , Cmd.batch
                [ Cmd.map StudioMsg cmd
                , Cmd.map StudioMsg fitCmd
                , Cmd.batch (List.map eventCmd events)
                , requestThumbnails updated
                ]
            , Stay
            )

        SelectBase base ->
            let
                doc =
                    base.document

                ( nextStudio, cmd, events ) =
                    Core.resetDocument doc model.studio

                ( fittedStudio, fitCmd ) =
                    Core.fitView nextStudio

                updated =
                    { model
                        | studio = fittedStudio
                        , base = Just base
                        , baseLocked = True
                        , activeTab = BuildBricksTab
                        , notice = Nothing
                    }
            in
            ( updated
            , Cmd.batch
                [ Cmd.map StudioMsg cmd
                , Cmd.map StudioMsg fitCmd
                , Cmd.batch (List.map eventCmd events)
                , requestThumbnails updated
                ]
            , Stay
            )

        ToggleBaseLock ->
            ( { model | baseLocked = not model.baseLocked, notice = Nothing }, Cmd.none, Stay )

        PasteTextChanged textVal ->
            ( { model | pasteText = textVal, pasteError = Nothing }, Cmd.none, Stay )

        SubmitPaste ->
            case Ingest.decodeRaceInput Nothing model.pasteText of
                Ok decoded ->
                    let
                        newBase =
                            Editor.baseOf decoded.document

                        ( nextStudio, cmd, events ) =
                            Core.resetDocument decoded.document model.studio

                        ( fittedStudio, fitCmd ) =
                            Core.fitView nextStudio

                        updated =
                            { model
                                | studio = fittedStudio
                                , base = newBase
                                , baseLocked = newBase /= Nothing
                                , pasteText = ""
                                , pasteError = Nothing
                                , activeTab = BuildBricksTab
                            }
                    in
                    ( updated
                    , Cmd.batch
                        [ Cmd.map StudioMsg cmd
                        , Cmd.map StudioMsg fitCmd
                        , Cmd.batch (List.map eventCmd events)
                        , requestThumbnails updated
                        ]
                    , Stay
                    )

                Err _ ->
                    ( { model | pasteError = Just (RI18n.pasteErrorFallback model.language) }, Cmd.none, Stay )

        RequestFileUpload ->
            ( model, Cmd.none, TriggerFileUpload )

        OpenQrScanner ->
            ( model, Cmd.none, TriggerQrScanner )

        LoadExternalDocument doc maybeBase ->
            let
                newBase =
                    case maybeBase of
                        Just b ->
                            Just b

                        Nothing ->
                            Editor.baseOf doc

                ( nextStudio, cmd, events ) =
                    Core.resetDocument doc model.studio

                ( fittedStudio, fitCmd ) =
                    Core.fitView nextStudio

                updated =
                    { model
                        | studio = fittedStudio
                        , base = newBase
                        , baseLocked = newBase /= Nothing
                        , pasteText = ""
                        , pasteError = Nothing
                        , activeTab = BuildBricksTab
                    }
            in
            ( updated
            , Cmd.batch
                [ Cmd.map StudioMsg cmd
                , Cmd.map StudioMsg fitCmd
                , Cmd.batch (List.map eventCmd events)
                , requestThumbnails updated
                ]
            , Stay
            )


{-| Run a studio step, unless it would split the base.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore step model =
    let
        ( studio, cmd, events ) =
            step model.studio

        isAllowed =
            if not model.baseLocked then
                True

            else
                case model.base of
                    Just base ->
                        Editor.baseIntact base (Core.getDocument studio)

                    Nothing ->
                        True
    in
    if isAllowed then
        ( { model | studio = studio }
        , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
        )

    else
        ( { model | notice = Just (keepBaseText model.language) }, Cmd.none )


eventCmd : Core.Event -> Cmd Msg
eventCmd event =
    case event of
        Core.FlattenRequested payload ->
            GamePorts.requestGeometryFlatten payload

        _ ->
            Cmd.none


baseSelected : Model -> Bool
baseSelected model =
    if not model.baseLocked then
        False

    else
        case model.base of
            Just base ->
                Core.getPlacements model.studio
                    |> List.any (\p -> Set.member p.id model.studio.selection && Editor.isBasePlacement base p)

            Nothing ->
                False


requestThumbnails : Model -> Cmd Msg
requestThumbnails model =
    let
        missing =
            Editor.parts
                |> List.filter (\p -> not (Dict.member (PartCard.thumbnailKey p.file model.colour) model.thumbnails))
    in
    if List.isEmpty missing then
        Cmd.none

    else
        Ports.requestThumbnails
            (List.map
                (\p ->
                    { file = p.file
                    , color = model.colour
                    , rgb = Colors.resolveColor model.colour 16 |> (\c -> [ c.r, c.g, c.b ])
                    }
                )
                missing
            )


{-| The studio's subscriptions, geometry worker, and thumbnails.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , GamePorts.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , GamePorts.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , Ports.thumbnailLoaded ThumbnailLoaded
        ]



-- ── View ─────────────────────────────────────────────────────────────────────


{-| Height of bottom panel depending on active tab.
-}
tabDrawerHeight : BuilderTab -> Int
tabDrawerHeight tab =
    case tab of
        BuildBricksTab ->
            138

        AllBricksTab ->
            320

        _ ->
            280


canvasHeight : Model -> Int
canvasHeight model =
    max 160 (model.height - tabDrawerHeight model.activeTab)


{-| The builder view.
-}
view : Model -> Html Msg
view model =
    let
        analyzed =
            Analyze.analyzeDocument "Car" (Core.getDocument model.studio) model.partCache model.partIndex

        floorY =
            analyzed.spec.wheelContactYLdu
    in
    div
        [ Attr.class "brick-editor"
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "radial-gradient(ellipse 85% 65% at 50% 18%, #1f2e3d 0%, #0d1722 55%, #050b10 100%)"
        , Attr.style "color" "#f8fafc"
        , Attr.style "font-family" "var(--font-sans, system-ui, sans-serif)"
        , Attr.style "user-select" "none"
        , Attr.style "-webkit-user-select" "none"
        ]
        [ div
            [ Attr.style "position" "absolute"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" (px model.width)
            , Attr.style "height" (px (canvasHeight model))
            , Attr.style "touch-action" "none"
            ]
            [ Html.map StudioMsg
                (Core.viewWithExtra
                    { width = model.width
                    , height = canvasHeight model
                    , extraEntities =
                        \camera aspect ->
                            GarageFloor.entities floorY camera aspect
                    }
                    model.studio
                )
            ]
        , topBar model
        , noticeView model
        , bottomPanel model
        ]


topBar : Model -> Html Msg
topBar model =
    let
        lang =
            model.language

        tabBtn tab label icon =
            let
                isActive =
                    model.activeTab == tab
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectTab tab)
                , Attr.attribute "aria-label" label
                , Attr.title label
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "width" "40px"
                , Attr.style "height" "40px"
                , Attr.style "flex-shrink" "0"
                , Attr.style "border-radius" "12px"
                , Attr.style "border"
                    (if isActive then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.18)"
                    )
                , Attr.style "background"
                    (if isActive then
                        Brand.legoYellow

                     else
                        "rgba(15, 23, 42, 0.82)"
                    )
                , Attr.style "color"
                    (if isActive then
                        Brand.legoBlack

                     else
                        "#e2e8f0"
                    )
                , Attr.style "cursor" "pointer"
                , Attr.style "pointer-events" "auto"
                , Attr.style "transition" "all 0.15s ease"
                ]
                [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "max(8px, env(safe-area-inset-top)) max(8px, env(safe-area-inset-right)) 8px max(8px, env(safe-area-inset-left))"
        , Attr.style "pointer-events" "none"
        ]
        [ iconButton (tr lang "Back to the menu" "Takaisin valikkoon") FeatherIcons.arrowLeft (Just Leave)
        , div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "4px"
            , Attr.style "overflow-x" "auto"
            , Attr.style "max-width" "calc(100vw - 220px)"
            ]
            [ tabBtn BuildBricksTab (RI18n.buildTab lang) FeatherIcons.box
            , tabBtn AllBricksTab (tr lang "All bricks" "Kaikki palikat") FeatherIcons.grid
            , tabBtn BasesTab (RI18n.basesTab lang) FeatherIcons.truck
            , tabBtn ImportBuildTab (RI18n.openBuildTab lang) FeatherIcons.download
            , tabBtn ReviewTab (RI18n.reviewTab lang) FeatherIcons.award
            , button
                [ Attr.type_ "button"
                , onClick ToggleBaseLock
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "width" "40px"
                , Attr.style "height" "40px"
                , Attr.style "flex-shrink" "0"
                , Attr.style "border-radius" "12px"
                , Attr.style "border"
                    (if model.baseLocked then
                        "1px solid rgba(255, 255, 255, 0.2)"

                     else
                        "1px solid #f59e0b"
                    )
                , Attr.style "background"
                    (if model.baseLocked then
                        "rgba(15, 23, 42, 0.82)"

                     else
                        "rgba(245, 158, 11, 0.25)"
                    )
                , Attr.style "color"
                    (if model.baseLocked then
                        "#cbd5e1"

                     else
                        "#fbbf24"
                    )
                , Attr.style "cursor" "pointer"
                , Attr.style "pointer-events" "auto"
                , Attr.attribute "title"
                    (if model.baseLocked then
                        tr lang "Base locked (click to unlock and edit chassis)" "Alusta lukittu (klikkaa avataksesi alustan muokkaus)"

                     else
                        tr lang "Base unlocked (click to lock chassis)" "Alusta avattu (klikkaa lukitaksesi alusta)"
                    )
                , Attr.attribute "aria-label"
                    (if model.baseLocked then
                        tr lang "Base locked (click to unlock and edit chassis)" "Alusta lukittu (klikkaa avataksesi alustan muokkaus)"

                     else
                        tr lang "Base unlocked (click to lock chassis)" "Alusta avattu (klikkaa lukitaksesi alusta)"
                    )
                ]
                [ (if model.baseLocked then
                    FeatherIcons.lock

                   else
                    FeatherIcons.unlock
                  )
                    |> FeatherIcons.withSize 18
                    |> FeatherIcons.toHtml []
                ]
            ]
        , div [ Attr.style "margin-left" "auto", Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "4px" ]
            [ iconButton (tr lang "Undo" "Kumoa") FeatherIcons.cornerUpLeft (enabledIf (Core.canUndo model.studio) Undo)
            , iconButton (tr lang "Redo" "Tee uudelleen") FeatherIcons.cornerUpRight (enabledIf (Core.canRedo model.studio) Redo)
            , iconButton (tr lang "Show the whole car" "Näytä koko auto") FeatherIcons.maximize (Just FitView)
            , button
                (buttonStyle True
                    ++ [ onClick Finish
                       , Attr.style "background" Brand.legoYellow
                       , Attr.style "color" Brand.legoBlack
                       , Attr.style "border" "none"
                       , Attr.attribute "aria-label" (tr lang "To the garage" "Talliin")
                       , Attr.title (tr lang "To the garage" "Talliin")
                       ]
                )
                [ FeatherIcons.check |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]
            ]
        ]


noticeView : Model -> Html Msg
noticeView model =
    case model.notice of
        Just message ->
            div
                [ Attr.attribute "role" "status"
                , Attr.style "position" "absolute"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translateX(-50%)"
                , Attr.style "top" "64px"
                , Attr.style "max-width" "calc(100% - 32px)"
                , Attr.style "padding" "6px 12px"
                , Attr.style "border-radius" "10px"
                , Attr.style "background" "rgba(15, 23, 42, 0.86)"
                , Attr.style "font-size" "13px"
                , Attr.style "text-align" "center"
                ]
                [ text message ]

        Nothing ->
            text ""


{-| Render bottom panel according to active tab.
-}
bottomPanel : Model -> Html Msg
bottomPanel model =
    case model.activeTab of
        BuildBricksTab ->
            dock model

        AllBricksTab ->
            viewAllBricksDrawer model

        BasesTab ->
            viewBasesDrawer model

        ImportBuildTab ->
            viewImportDrawer model

        ReviewTab ->
            viewReviewDrawer model


dockHeight : Int
dockHeight =
    136


dock : Model -> Html Msg
dock model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "height" (px dockHeight)
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "8px max(8px, env(safe-area-inset-right)) max(8px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        , Attr.style "background" "rgba(15, 23, 42, 0.94)"
        , Attr.style "border-top" "1px solid rgba(148, 163, 184, 0.25)"
        ]
        [ dockControlsRow model, partsStrip model ]


dockControlsRow : Model -> Html Msg
dockControlsRow model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "10px"
        , Attr.style "overflow-x" "auto"
        , Attr.style "padding" "0 4px"
        ]
        [ modelingControls model
        , div
            [ Attr.style "width" "1px"
            , Attr.style "height" "24px"
            , Attr.style "background" "rgba(255, 255, 255, 0.18)"
            , Attr.style "flex-shrink" "0"
            ]
            []
        , colourRow model
        ]


modelingControls : Model -> Html Msg
modelingControls model =
    let
        lang =
            model.language

        hasSel =
            Core.hasSelection model.studio

        onBase =
            baseSelected model

        modelToolBtn label icon maybeMsg =
            let
                enabled =
                    maybeMsg /= Nothing

                clickAttrs =
                    case maybeMsg of
                        Just msg ->
                            [ onClick msg ]

                        Nothing ->
                            []
            in
            button
                ([ Attr.type_ "button"
                 , Attr.attribute "aria-label" label
                 , Attr.title label
                 , Attr.disabled (not enabled)
                 , Attr.style "display" "inline-flex"
                 , Attr.style "align-items" "center"
                 , Attr.style "justify-content" "center"
                 , Attr.style "width" "34px"
                 , Attr.style "height" "34px"
                 , Attr.style "flex-shrink" "0"
                 , Attr.style "border-radius" "10px"
                 , Attr.style "border"
                    (if enabled then
                        "1px solid rgba(255, 255, 255, 0.25)"

                     else
                        "1px solid rgba(255, 255, 255, 0.08)"
                    )
                 , Attr.style "background"
                    (if enabled then
                        "rgba(30, 41, 59, 0.85)"

                     else
                        "rgba(15, 23, 42, 0.4)"
                    )
                 , Attr.style "color"
                    (if enabled then
                        "#f8fafc"

                     else
                        "#475569"
                    )
                 , Attr.style "cursor"
                    (if enabled then
                        "pointer"

                     else
                        "default"
                    )
                 , Attr.style "opacity"
                    (if enabled then
                        "1"

                     else
                        "0.35"
                    )
                 , Attr.style "touch-action" "manipulation"
                 , Attr.style "transition" "all 0.15s ease"
                 ]
                    ++ clickAttrs
                )
                [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
    in
    div
        [ Attr.attribute "role" "toolbar"
        , Attr.attribute "aria-label" (tr lang "Part modeling tools" "Osan muokkaustyökalut")
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "4px"
        , Attr.style "flex-shrink" "0"
        ]
        [ modelToolBtn (tr lang "Turn left" "Käännä vasemmalle") FeatherIcons.rotateCcw (enabledIf hasSel (Turn TurnLeft))
        , modelToolBtn (tr lang "Turn right" "Käännä oikealle") FeatherIcons.rotateCw (enabledIf hasSel (Turn TurnRight))
        , modelToolBtn (tr lang "Raise" "Nosta") FeatherIcons.arrowUp (enabledIf hasSel Raise)
        , modelToolBtn (tr lang "Lower" "Laske") FeatherIcons.arrowDown (enabledIf hasSel Lower)
        , modelToolBtn
            (if onBase then
                tr lang "The base cannot be deleted" "Alustaa ei voi poistaa"

             else
                tr lang "Delete" "Poista"
            )
            FeatherIcons.trash2
            (enabledIf (hasSel && not onBase) DeleteSelected)
        ]


colourRow : Model -> Html Msg
colourRow model =
    div
        [ Attr.attribute "role" "listbox"
        , Attr.attribute "aria-label" (tr model.language "Colour" "Väri")
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "6px"
        ]
        (List.map (swatchView model) Editor.palette)


swatchView : Model -> Editor.Swatch -> Html Msg
swatchView model swatch =
    let
        selected =
            swatch.code == model.colour

        name =
            tr model.language swatch.nameEn swatch.nameFi

        cssColour =
            Dict.get swatch.code ColorData.colorTable
                |> Maybe.map Colours.css
                |> Maybe.withDefault "#888"
    in
    button
        [ Attr.type_ "button"
        , onClick (PickColour swatch.code)
        , Attr.attribute "role" "option"
        , Attr.attribute "aria-selected"
            (if selected then
                "true"

             else
                "false"
            )
        , Attr.attribute "aria-label" name
        , Attr.title name
        , Attr.style "width" "34px"
        , Attr.style "height" "34px"
        , Attr.style "flex" "0 0 34px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background" cssColour
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        , Attr.style "border"
            (if selected then
                "3px solid " ++ Brand.legoYellow

             else
                "2px solid rgba(255, 255, 255, 0.35)"
            )
        , Attr.style "box-shadow"
            (if selected then
                "0 0 0 2px " ++ Brand.legoBlack ++ " inset"

             else
                "none"
            )
        ]
        []


partsStrip : Model -> Html Msg
partsStrip model =
    div
        [ Attr.attribute "aria-label" (tr model.language "Parts" "Osat")
        , Attr.class "palette-scroll-x"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.style "scrollbar-width" "thin"
        , Attr.style "-webkit-overflow-scrolling" "touch"
        , Attr.style "padding-bottom" "2px"
        ]
        (button
            [ Attr.type_ "button"
            , onClick (SelectTab AllBricksTab)
            , Attr.style "flex-shrink" "0"
            , Attr.style "height" "56px"
            , Attr.style "padding" "0 12px"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "gap" "3px"
            , Attr.style "border-radius" "10px"
            , Attr.style "border" "1px solid rgba(250, 200, 10, 0.5)"
            , Attr.style "background" "rgba(250, 200, 10, 0.15)"
            , Attr.style "color" Brand.legoYellow
            , Attr.style "cursor" "pointer"
            , Attr.style "white-space" "nowrap"
            , Attr.title (tr model.language "Browse all categorized bricks" "Selaa kaikkia osia kategorioittain")
            ]
            [ span [ Attr.style "font-size" "16px" ] [ text "📦" ]
            , span [ Attr.style "font-size" "10px", Attr.style "font-weight" "800" ] [ text (tr model.language "All Bricks" "Kaikki palikat") ]
            ]
            :: List.map (partCard model) Editor.parts
        )


partCard : Model -> PartEntry -> Html Msg
partCard model entry =
    let
        name =
            tr model.language entry.nameEn entry.nameFi

        number =
            String.replace ".dat" "" entry.file
    in
    PartCard.compactView
        { imageUrl = Dict.get (PartCard.thumbnailKey entry.file model.colour) model.thumbnails
        , tooltip = name ++ " (" ++ number ++ ")"
        , ariaLabel = tr model.language "Add " "Lisää " ++ name
        , badge = Nothing
        , onClick = Just (AddPart entry.file)
        , attributes = []
        }


{-| Drawer showing the 4 starter bases.
-}
viewBasesDrawer : Model -> Html Msg
viewBasesDrawer model =
    let
        lang =
            model.language

        renderBaseCard base =
            let
                isCurrent =
                    case model.base of
                        Just b ->
                            base.id == b.id

                        Nothing ->
                            False

                name =
                    tr lang base.nameEn base.nameFi
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectBase base)
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "8px"
                , Attr.style "padding" "10px 12px"
                , Attr.style "border-radius" "12px"
                , Attr.style "border"
                    (if isCurrent then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.15)"
                    )
                , Attr.style "background"
                    (if isCurrent then
                        "rgba(250, 200, 10, 0.16)"

                     else
                        "rgba(15, 23, 42, 0.7)"
                    )
                , Attr.style "color" Brand.legoWhite
                , Attr.style "cursor" "pointer"
                , Attr.style "text-align" "left"
                ]
                [ div
                    [ Attr.style "width" "100%"
                    , Attr.style "height" "75px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "background" "#070c14"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.08)"
                    ]
                    [ basePreviewSvg base.id ]
                , div []
                    [ div [ Attr.style "font-weight" "800", Attr.style "font-size" "13px" ] [ text ("🔨 " ++ name) ]
                    , div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "margin-top" "2px" ]
                        [ text (String.fromInt base.partCount ++ tr lang " parts • chassis with wheels" " osaa • alusta pyörineen") ]
                    ]
                ]
    in
    div (drawerStyle 280)
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px", Attr.style "height" "100%", Attr.style "overflow-y" "auto" ]
            [ h3 [ Attr.style "font-size" "13px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase" ]
                [ text (tr lang "Choose Starter Chassis" "Valitse aloituspohja") ]
            , div [ Attr.style "display" "grid", Attr.style "grid-template-columns" "repeat(auto-fill, minmax(200px, 1fr))", Attr.style "gap" "10px" ]
                (List.map renderBaseCard Editor.bases)
            ]
        ]


{-| Category definitions for the categorized All-Bricks drawer.
-}
partCategories : List { titleEn : String, titleFi : String, partFiles : List String }
partCategories =
    [ { titleEn = "Bricks"
      , titleFi = "Peruspalikat"
      , partFiles = [ "3005.dat", "3004.dat", "3622.dat", "3010.dat", "3009.dat", "3003.dat", "3002.dat", "3001.dat" ]
      }
    , { titleEn = "Plates"
      , titleFi = "Levyt"
      , partFiles = [ "3024.dat", "3023b.dat", "3623.dat", "3710.dat", "3666.dat", "3022.dat", "3021.dat", "3020.dat", "3795.dat", "3034.dat", "3031.dat" ]
      }
    , { titleEn = "Slopes & Curves"
      , titleFi = "Viisteet ja kaaret"
      , partFiles = [ "3040b.dat", "3039.dat", "3298.dat", "54200.dat", "85984.dat", "11477.dat", "15068.dat", "50950.dat", "3665b.dat" ]
      }
    , { titleEn = "Tiles"
      , titleFi = "Laatat"
      , partFiles = [ "3070b.dat", "3069b.dat", "2431.dat", "3068b.dat" ]
      }
    , { titleEn = "Car Parts"
      , titleFi = "Auton erikoisosat"
      , partFiles = [ "3829c01.dat", "4079.dat", "3823.dat", "50745.dat", "4070.dat", "2412b.dat", "3700.dat" ]
      }
    , { titleEn = "Wheels & Axles"
      , titleFi = "Pyörät ja akselit"
      , partFiles = [ "3679.dat", "3680.dat", "4600.dat", "4488.dat", "4624.dat", "3641.dat", "6014.dat", "56890.dat", "55981.dat", "56891.dat" ]
      }
    ]


{-| Drawer displaying all available bricks grouped into categories.
-}
viewAllBricksDrawer : Model -> Html Msg
viewAllBricksDrawer model =
    let
        lang =
            model.language

        categorySection cat =
            let
                title =
                    tr lang cat.titleEn cat.titleFi

                catParts =
                    Editor.parts
                        |> List.filter (\p -> List.member p.file cat.partFiles)

                renderCard entry =
                    let
                        name =
                            tr lang entry.nameEn entry.nameFi

                        number =
                            String.replace ".dat" "" entry.file

                        thumb =
                            Dict.get (PartCard.thumbnailKey entry.file model.colour) model.thumbnails
                    in
                    button
                        [ Attr.type_ "button"
                        , onClick (AddPart entry.file)
                        , Attr.style "display" "flex"
                        , Attr.style "flex-direction" "column"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        , Attr.style "gap" "4px"
                        , Attr.style "padding" "8px 6px"
                        , Attr.style "border-radius" "10px"
                        , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.12)"
                        , Attr.style "color" "#f8fafc"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "transition" "all 0.15s ease"
                        , Attr.title (name ++ " (" ++ number ++ ")")
                        ]
                        [ div
                            [ Attr.style "width" "44px"
                            , Attr.style "height" "44px"
                            , Attr.style "display" "flex"
                            , Attr.style "align-items" "center"
                            , Attr.style "justify-content" "center"
                            ]
                            [ case thumb of
                                Just url ->
                                    Html.img
                                        [ Attr.src url
                                        , Attr.alt name
                                        , Attr.style "max-width" "100%"
                                        , Attr.style "max-height" "100%"
                                        , Attr.style "object-fit" "contain"
                                        ]
                                        []

                                Nothing ->
                                    span [ Attr.style "font-size" "20px" ] [ text "🧊" ]
                            ]
                        , div [ Attr.style "font-size" "11px", Attr.style "font-weight" "700", Attr.style "text-align" "center", Attr.style "line-height" "1.2" ]
                            [ text name ]
                        , div [ Attr.style "font-size" "9px", Attr.style "color" "#94a3b8" ]
                            [ text ("#" ++ number) ]
                        ]
            in
            div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "8px" ]
                [ div [ Attr.style "font-size" "12px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase", Attr.style "letter-spacing" "0.5px" ]
                    [ text title ]
                , div [ Attr.style "display" "grid", Attr.style "grid-template-columns" "repeat(auto-fill, minmax(110px, 1fr))", Attr.style "gap" "8px" ]
                    (List.map renderCard catParts)
                ]
    in
    div (drawerStyle 320)
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "12px", Attr.style "height" "100%", Attr.style "overflow-y" "auto" ]
            [ div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "justify-content" "space-between", Attr.style "gap" "8px", Attr.style "flex-wrap" "wrap" ]
                [ h3 [ Attr.style "font-size" "14px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "margin" "0" ]
                    [ text (tr lang "📦 All Available Bricks" "📦 Kaikki saatavilla olevat palikat") ]
                , colourRow model
                ]
            , div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "16px" ]
                (List.map categorySection partCategories)
            ]
        ]


{-| Dedicated schematic preview SVG for each starter chassis.
-}
basePreviewSvg : String -> Html msg
basePreviewSvg baseId =
    case baseId of
        "turntable-kart" ->
            Svg.svg
                [ SvgAttr.viewBox "0 0 160 90"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                ]
                [ Svg.rect [ SvgAttr.width "160", SvgAttr.height "90", SvgAttr.fill "#090d16" ] []
                , Svg.rect [ SvgAttr.x "32", SvgAttr.y "38", SvgAttr.width "86", SvgAttr.height "14", SvgAttr.rx "3", SvgAttr.fill "#334155", SvgAttr.stroke "#64748b", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "28", SvgAttr.y "18", SvgAttr.width "12", SvgAttr.height "54", SvgAttr.rx "2", SvgAttr.fill "#1e293b", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "20", SvgAttr.y "8", SvgAttr.width "28", SvgAttr.height "14", SvgAttr.rx "4", SvgAttr.fill "#0f172a", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "25", SvgAttr.y "11", SvgAttr.width "18", SvgAttr.height "8", SvgAttr.rx "2", SvgAttr.fill "#e2e8f0" ] []
                , Svg.rect [ SvgAttr.x "20", SvgAttr.y "68", SvgAttr.width "28", SvgAttr.height "14", SvgAttr.rx "4", SvgAttr.fill "#0f172a", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "25", SvgAttr.y "71", SvgAttr.width "18", SvgAttr.height "8", SvgAttr.rx "2", SvgAttr.fill "#e2e8f0" ] []
                , Svg.rect [ SvgAttr.x "114", SvgAttr.y "18", SvgAttr.width "12", SvgAttr.height "54", SvgAttr.rx "2", SvgAttr.fill "#1e293b", SvgAttr.stroke "#fac80a", SvgAttr.strokeWidth "1" ] []
                , Svg.circle [ SvgAttr.cx "120", SvgAttr.cy "45", SvgAttr.r "10", SvgAttr.fill "#fac80a", SvgAttr.stroke "#fde047", SvgAttr.strokeWidth "2" ] []
                , Svg.circle [ SvgAttr.cx "120", SvgAttr.cy "45", SvgAttr.r "3.5", SvgAttr.fill "#0f172a" ] []
                , Svg.rect [ SvgAttr.x "106", SvgAttr.y "8", SvgAttr.width "28", SvgAttr.height "14", SvgAttr.rx "4", SvgAttr.fill "#0f172a", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "111", SvgAttr.y "11", SvgAttr.width "18", SvgAttr.height "8", SvgAttr.rx "2", SvgAttr.fill "#fac80a" ] []
                , Svg.rect [ SvgAttr.x "106", SvgAttr.y "68", SvgAttr.width "28", SvgAttr.height "14", SvgAttr.rx "4", SvgAttr.fill "#0f172a", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "111", SvgAttr.y "71", SvgAttr.width "18", SvgAttr.height "8", SvgAttr.rx "2", SvgAttr.fill "#fac80a" ] []
                ]

        "pinewood-classic" ->
            Svg.svg
                [ SvgAttr.viewBox "0 0 160 90"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                ]
                [ Svg.rect [ SvgAttr.width "160", SvgAttr.height "90", SvgAttr.fill "#090d16" ] []
                , Svg.rect [ SvgAttr.x "24", SvgAttr.y "36", SvgAttr.width "112", SvgAttr.height "18", SvgAttr.rx "3", SvgAttr.fill "#dc2626", SvgAttr.stroke "#ef4444", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "36", SvgAttr.y "20", SvgAttr.width "10", SvgAttr.height "50", SvgAttr.rx "2", SvgAttr.fill "#1e293b", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "114", SvgAttr.y "20", SvgAttr.width "10", SvgAttr.height "50", SvgAttr.rx "2", SvgAttr.fill "#1e293b", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "32", SvgAttr.y "12", SvgAttr.width "18", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#0f172a", SvgAttr.stroke "#334155", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "36", SvgAttr.y "14", SvgAttr.width "10", SvgAttr.height "6", SvgAttr.rx "1", SvgAttr.fill "#ef4444" ] []
                , Svg.rect [ SvgAttr.x "32", SvgAttr.y "68", SvgAttr.width "18", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#0f172a", SvgAttr.stroke "#334155", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "36", SvgAttr.y "70", SvgAttr.width "10", SvgAttr.height "6", SvgAttr.rx "1", SvgAttr.fill "#ef4444" ] []
                , Svg.rect [ SvgAttr.x "110", SvgAttr.y "12", SvgAttr.width "18", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#0f172a", SvgAttr.stroke "#334155", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "114", SvgAttr.y "14", SvgAttr.width "10", SvgAttr.height "6", SvgAttr.rx "1", SvgAttr.fill "#ef4444" ] []
                , Svg.rect [ SvgAttr.x "110", SvgAttr.y "68", SvgAttr.width "18", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#0f172a", SvgAttr.stroke "#334155", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "114", SvgAttr.y "70", SvgAttr.width "10", SvgAttr.height "6", SvgAttr.rx "1", SvgAttr.fill "#ef4444" ] []
                ]

        "alpine-crawler" ->
            Svg.svg
                [ SvgAttr.viewBox "0 0 160 90"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                ]
                [ Svg.rect [ SvgAttr.width "160", SvgAttr.height "90", SvgAttr.fill "#090d16" ] []
                , Svg.rect [ SvgAttr.x "28", SvgAttr.y "34", SvgAttr.width "104", SvgAttr.height "22", SvgAttr.rx "3", SvgAttr.fill "#1e293b", SvgAttr.stroke "#fac80a", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "50", SvgAttr.y "26", SvgAttr.width "60", SvgAttr.height "38", SvgAttr.rx "2", SvgAttr.fill "#334155", SvgAttr.stroke "#64748b", SvgAttr.strokeWidth "1" ] []
                , Svg.circle [ SvgAttr.cx "120", SvgAttr.cy "45", SvgAttr.r "10", SvgAttr.fill "#fac80a", SvgAttr.stroke "#fde047", SvgAttr.strokeWidth "2" ] []
                , Svg.circle [ SvgAttr.cx "120", SvgAttr.cy "45", SvgAttr.r "3.5", SvgAttr.fill "#0f172a" ] []
                , Svg.rect [ SvgAttr.x "16", SvgAttr.y "5", SvgAttr.width "34", SvgAttr.height "16", SvgAttr.rx "4", SvgAttr.fill "#020617", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "2" ] []
                , Svg.rect [ SvgAttr.x "22", SvgAttr.y "8", SvgAttr.width "22", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#94a3b8" ] []
                , Svg.rect [ SvgAttr.x "16", SvgAttr.y "69", SvgAttr.width "34", SvgAttr.height "16", SvgAttr.rx "4", SvgAttr.fill "#020617", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "2" ] []
                , Svg.rect [ SvgAttr.x "22", SvgAttr.y "72", SvgAttr.width "22", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#94a3b8" ] []
                , Svg.rect [ SvgAttr.x "104", SvgAttr.y "5", SvgAttr.width "34", SvgAttr.height "16", SvgAttr.rx "4", SvgAttr.fill "#020617", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "2" ] []
                , Svg.rect [ SvgAttr.x "110", SvgAttr.y "8", SvgAttr.width "22", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#fac80a" ] []
                , Svg.rect [ SvgAttr.x "104", SvgAttr.y "69", SvgAttr.width "34", SvgAttr.height "16", SvgAttr.rx "4", SvgAttr.fill "#020617", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "2" ] []
                , Svg.rect [ SvgAttr.x "110", SvgAttr.y "72", SvgAttr.width "22", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#fac80a" ] []
                ]

        "trike-wedge" ->
            Svg.svg
                [ SvgAttr.viewBox "0 0 160 90"
                , SvgAttr.width "100%"
                , SvgAttr.height "100%"
                ]
                [ Svg.rect [ SvgAttr.width "160", SvgAttr.height "90", SvgAttr.fill "#090d16" ] []
                , Svg.polygon [ SvgAttr.points "28,24 28,66 126,45", SvgAttr.fill "#1e293b", SvgAttr.stroke "#38bdf8", SvgAttr.strokeWidth "2" ] []
                , Svg.rect [ SvgAttr.x "24", SvgAttr.y "12", SvgAttr.width "10", SvgAttr.height "66", SvgAttr.rx "2", SvgAttr.fill "#334155", SvgAttr.stroke "#475569", SvgAttr.strokeWidth "1" ] []
                , Svg.rect [ SvgAttr.x "16", SvgAttr.y "5", SvgAttr.width "26", SvgAttr.height "15", SvgAttr.rx "3", SvgAttr.fill "#0f172a", SvgAttr.stroke "#38bdf8", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "21", SvgAttr.y "8", SvgAttr.width "16", SvgAttr.height "9", SvgAttr.rx "1.5", SvgAttr.fill "#e2e8f0" ] []
                , Svg.rect [ SvgAttr.x "16", SvgAttr.y "70", SvgAttr.width "26", SvgAttr.height "15", SvgAttr.rx "3", SvgAttr.fill "#0f172a", SvgAttr.stroke "#38bdf8", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "21", SvgAttr.y "73", SvgAttr.width "16", SvgAttr.height "9", SvgAttr.rx "1.5", SvgAttr.fill "#e2e8f0" ] []
                , Svg.circle [ SvgAttr.cx "120", SvgAttr.cy "45", SvgAttr.r "9", SvgAttr.fill "#fac80a", SvgAttr.stroke "#fde047", SvgAttr.strokeWidth "1.5" ] []
                , Svg.circle [ SvgAttr.cx "120", SvgAttr.cy "45", SvgAttr.r "3", SvgAttr.fill "#0f172a" ] []
                , Svg.rect [ SvgAttr.x "114", SvgAttr.y "36", SvgAttr.width "30", SvgAttr.height "18", SvgAttr.rx "4", SvgAttr.fill "#0f172a", SvgAttr.stroke "#fac80a", SvgAttr.strokeWidth "1.5" ] []
                , Svg.rect [ SvgAttr.x "120", SvgAttr.y "40", SvgAttr.width "18", SvgAttr.height "10", SvgAttr.rx "2", SvgAttr.fill "#e2e8f0" ] []
                ]

        _ ->
            Svg.svg [ SvgAttr.viewBox "0 0 160 90" ] []


{-| Drawer for pasting or loading build files / QR scans.
-}
viewImportDrawer : Model -> Html Msg
viewImportDrawer model =
    let
        lang =
            model.language
    in
    div (drawerStyle 280)
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px", Attr.style "max-width" "680px", Attr.style "margin" "0 auto" ]
            [ h3 [ Attr.style "font-size" "13px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase" ]
                [ text (RI18n.openBuildTab lang) ]
            , div [ Attr.style "display" "flex", Attr.style "gap" "8px" ]
                [ textarea
                    [ Attr.placeholder (RI18n.pastePlaceholder lang)
                    , Attr.value model.pasteText
                    , onInput PasteTextChanged
                    , Attr.style "flex" "1"
                    , Attr.style "height" "70px"
                    , Attr.style "padding" "8px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "font-family" "monospace"
                    , Attr.style "font-size" "12px"
                    , Attr.style "resize" "none"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick SubmitPaste
                    , Attr.style "padding" "0 16px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    , Attr.style "font-weight" "800"
                    , Attr.style "font-size" "13px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "border" "none"
                    ]
                    [ text (RI18n.pasteButtonLabel lang) ]
                ]
            , case model.pasteError of
                Just err ->
                    div [ Attr.style "color" "#ef4444", Attr.style "font-size" "12px" ] [ text err ]

                Nothing ->
                    text ""
            , div [ Attr.style "display" "flex", Attr.style "gap" "10px", Attr.style "align-items" "center", Attr.style "padding-top" "6px" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick RequestFileUpload
                    , Attr.style "padding" "8px 14px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" "rgba(255, 255, 255, 0.1)"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "font-size" "12px"
                    , Attr.style "font-weight" "600"
                    , Attr.style "cursor" "pointer"
                    ]
                    [ text ("📂 " ++ RI18n.dropZoneButton lang) ]
                , button
                    [ Attr.type_ "button"
                    , onClick OpenQrScanner
                    , Attr.style "padding" "8px 14px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" "rgba(255, 255, 255, 0.1)"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "font-size" "12px"
                    , Attr.style "font-weight" "600"
                    , Attr.style "cursor" "pointer"
                    ]
                    [ text "📷 Skannaa QR-koodi" ]
                ]
            ]
        ]


{-| Drawer showing car evaluation, telemetry specs, helper actions, and race CTA.
-}
viewReviewDrawer : Model -> Html Msg
viewReviewDrawer model =
    let
        lang =
            model.language

        title =
            case model.base of
                Just b ->
                    baseName lang b

                Nothing ->
                    tr lang "Custom Soapbox" "Oma mäkiauto"

        doc =
            Core.getDocument model.studio

        allParts =
            Document.flattenedPlacements doc |> List.map .part

        hasSteeringWheel =
            List.member "3829c01.dat" allParts

        hasSeat =
            List.member "4079.dat" allParts

        analyzed =
            Analyze.analyzeDocument title doc model.partCache model.partIndex

        spec =
            analyzed.spec

        tooFewWheels =
            List.length spec.wheels < 3

        hasBlocking =
            List.any (\i -> i.severity == Blocker) spec.issues

        statTile lbl val =
            div
                [ Attr.style "flex" "1"
                , Attr.style "padding" "8px 12px"
                , Attr.style "border-radius" "8px"
                , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.08)"
                ]
                [ div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8" ] [ text lbl ]
                , div [ Attr.style "font-size" "14px", Attr.style "font-weight" "700", Attr.style "color" Brand.legoWhite, Attr.style "margin-top" "2px" ] [ text val ]
                ]

        helperBtn label partFile =
            button
                [ Attr.type_ "button"
                , onClick (AddPart partFile)
                , Attr.style "padding" "5px 10px"
                , Attr.style "border-radius" "8px"
                , Attr.style "background" "rgba(250, 200, 10, 0.15)"
                , Attr.style "border" "1px solid rgba(250, 200, 10, 0.4)"
                , Attr.style "color" Brand.legoYellow
                , Attr.style "font-size" "11px"
                , Attr.style "font-weight" "700"
                , Attr.style "cursor" "pointer"
                ]
                [ text label ]
    in
    div (drawerStyle 280)
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px", Attr.style "height" "100%", Attr.style "overflow-y" "auto" ]
            [ div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "justify-content" "space-between" ]
                [ h3 [ Attr.style "font-size" "13px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase" ]
                    [ text (RI18n.reviewTab lang) ]
                , div
                    [ Attr.style "padding" "4px 10px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "font-size" "11px"
                    , Attr.style "font-weight" "800"
                    , Attr.style "letter-spacing" "0.03em"
                    , Attr.style "background"
                        (if spec.isDrivable && not hasBlocking then
                            "rgba(34, 197, 94, 0.22)"

                         else if hasBlocking then
                            "rgba(239, 68, 68, 0.22)"

                         else
                            "rgba(245, 158, 11, 0.22)"
                        )
                    , Attr.style "color"
                        (if spec.isDrivable && not hasBlocking then
                            "#22c55e"

                         else if hasBlocking then
                            "#ef4444"

                         else
                            "#f59e0b"
                        )
                    ]
                    [ text
                        (if spec.isDrivable && not hasBlocking then
                            RI18n.readyToRaceBanner lang

                         else if hasBlocking then
                            RI18n.needsFixBanner lang

                         else
                            RI18n.needsPolishBanner lang
                        )
                    ]
                ]
            , div [ Attr.style "display" "grid", Attr.style "grid-template-columns" "repeat(4, 1fr)", Attr.style "gap" "8px" ]
                [ statTile (RI18n.garageWheelsLabel lang) (String.fromInt (List.length spec.wheels) ++ " kpl")
                , statTile (RI18n.garageSteeringLabel lang) (RI18n.steeringMechName lang spec.steering)
                , statTile (RI18n.garageMassLabel lang) (String.fromInt (round (spec.totalMassKg * 1000.0)) ++ " g")
                , statTile (RI18n.garageClearanceLabel lang) (String.fromFloat (roundFloat 1 (spec.groundClearanceM * 100.0)) ++ " cm")
                ]
            , -- Actionable Helper Buttons for Missing Elements
              div [ Attr.style "display" "flex", Attr.style "flex-wrap" "wrap", Attr.style "gap" "6px" ]
                [ if tooFewWheels then
                    helperBtn (tr lang "➕ Add wheels" "➕ Lisää pyörät") "4600.dat"

                  else
                    text ""
                , if spec.steering == FixedAxleFallback then
                    helperBtn (tr lang "➕ Add turntable" "➕ Lisää kääntöpöytä") "3680.dat"

                  else
                    text ""
                , if not hasSteeringWheel then
                    helperBtn (tr lang "➕ Add steering wheel" "➕ Lisää ratti") "3829c01.dat"

                  else
                    text ""
                , if not hasSeat then
                    helperBtn (tr lang "➕ Add seat" "➕ Lisää istuin") "4079.dat"

                  else
                    text ""
                ]
            , if List.isEmpty spec.issues then
                div [ Attr.style "font-size" "12px", Attr.style "color" "#22c55e", Attr.style "padding-top" "2px" ]
                    [ text (tr lang "✓ No issues found. Your car is ready to roll downhill!" "✓ Ei huomautuksia. Auto on valmis laskemaan mäkeä alas!") ]

              else
                div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "4px" ]
                    (List.map
                        (\issue ->
                            div
                                [ Attr.style "display" "flex"
                                , Attr.style "align-items" "center"
                                , Attr.style "gap" "8px"
                                , Attr.style "font-size" "12px"
                                , Attr.style "color"
                                    (if issue.severity == Blocker then
                                        "#ef4444"

                                     else if issue.severity == Bonus then
                                        "#22c55e"

                                     else
                                        "#f59e0b"
                                    )
                                ]
                                [ span [ Attr.style "font-weight" "700" ]
                                    [ text
                                        (case issue.severity of
                                            Blocker ->
                                                "[ESTE]"

                                            Warning ->
                                                "[VAROITUS]"

                                            Info ->
                                                "[INFO]"

                                            Bonus ->
                                                "[BONUS]"
                                        )
                                    ]
                                , text (RI18n.validationTitle lang issue.code)
                                ]
                        )
                        spec.issues
                    )
            , if spec.isDrivable && not hasBlocking then
                div [ Attr.style "padding-top" "4px" ]
                    [ button
                        [ Attr.type_ "button"
                        , onClick Finish
                        , Attr.style "width" "100%"
                        , Attr.style "padding" "10px 16px"
                        , Attr.style "border-radius" "12px"
                        , Attr.style "background" Brand.legoYellow
                        , Attr.style "color" Brand.legoBlack
                        , Attr.style "font-size" "14px"
                        , Attr.style "font-weight" "800"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "border" "none"
                        , Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        , Attr.style "gap" "8px"
                        , Attr.style "box-shadow" "0 4px 14px rgba(0, 0, 0, 0.3)"
                        ]
                        [ FeatherIcons.play |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                        , span [] [ text (RI18n.startRaceButton lang) ]
                        ]
                    ]

              else
                text ""
            ]
        ]


drawerStyle : Int -> List (Html.Attribute Msg)
drawerStyle heightPx =
    [ Attr.style "position" "absolute"
    , Attr.style "left" "0"
    , Attr.style "right" "0"
    , Attr.style "bottom" "0"
    , Attr.style "height" (px heightPx)
    , Attr.style "box-sizing" "border-box"
    , Attr.style "padding" "12px max(16px, env(safe-area-inset-right)) max(12px, env(safe-area-inset-bottom)) max(16px, env(safe-area-inset-left))"
    , Attr.style "background" "rgba(5, 19, 29, 0.96)"
    , Attr.style "border-top" "1px solid rgba(255, 255, 255, 0.15)"
    , Attr.style "box-shadow" "0 -4px 16px rgba(0, 0, 0, 0.4)"
    ]


iconButton : String -> FeatherIcons.Icon -> Maybe Msg -> Html Msg
iconButton label icon maybeMsg =
    button
        (buttonStyle (maybeMsg /= Nothing)
            ++ [ Attr.attribute "aria-label" label
               , Attr.title label
               , Attr.disabled (maybeMsg == Nothing)
               ]
            ++ (case maybeMsg of
                    Just msg ->
                        [ onClick msg ]

                    Nothing ->
                        []
               )
        )
        [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]


buttonStyle : Bool -> List (Html.Attribute Msg)
buttonStyle enabled =
    [ Attr.type_ "button"
    , Attr.style "pointer-events" "auto"
    , Attr.style "display" "inline-flex"
    , Attr.style "align-items" "center"
    , Attr.style "justify-content" "center"
    , Attr.style "min-width" "40px"
    , Attr.style "height" "40px"
    , Attr.style "flex-shrink" "0"
    , Attr.style "border-radius" "12px"
    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.35)"
    , Attr.style "background" "rgba(15, 23, 42, 0.82)"
    , Attr.style "color" "#f8fafc"
    , Attr.style "cursor"
        (if enabled then
            "pointer"

         else
            "default"
        )
    , Attr.style "opacity"
        (if enabled then
            "1"

         else
            "0.4"
        )
    , Attr.style "touch-action" "manipulation"
    ]


enabledIf : Bool -> Msg -> Maybe Msg
enabledIf condition msg =
    if condition then
        Just msg

    else
        Nothing


px : Int -> String
px n =
    String.fromInt n ++ "px"


roundFloat : Int -> Float -> Float
roundFloat decimals val =
    let
        factor =
            10.0 ^ toFloat decimals
    in
    toFloat (round (val * factor)) / factor


baseName : Language -> Base -> String
baseName lang base =
    tr lang base.nameEn base.nameFi


keepBaseText : Language -> String
keepBaseText lang =
    tr lang "The base with the wheels stays in one piece." "Alusta pyörineen pysyy yhtenä kappaleena."


keepBaseColours : Language -> String
keepBaseColours lang =
    tr lang "The base keeps its colours. Pick a part you added to colour it." "Alusta pitää värinsä. Valitse lisäämäsi osa värittääksesi sen."


tr : Language -> String -> String -> String
tr lang en fi =
    case lang of
        Fi ->
            fi

        En ->
            en
