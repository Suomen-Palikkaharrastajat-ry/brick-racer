module BuildHistoryPageTest exposing (suite)

{-| The car editor keeps the player's build: it saves it into the build
history a moment after an edit (only the newest edit), when the page is
hidden and when the bases drawer opens, never saves a bare starter base or
the same build twice, keeps the build replaced by another base in the
history, and opens a saved build again from the bases drawer. The bases
drawer asks the shared thumbnail pipeline for a picture of every starter base
and every saved build, and shows them once they arrive.
-}

import Browser.Events
import Expect
import Game.BuildHistory as BuildHistory
import Html.Attributes
import I18n
import LDraw.Document as Document
import Pages.Build as Build exposing (BuilderTab(..), Msg(..))
import Racer.Editor as Editor exposing (Base)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time


open : Base -> Build.Model
open base =
    Build.init
        { ldrawBase = ""
        , ldrawFallbackBase = ""
        , supportsPointerEvents = True
        , language = I18n.En
        , width = 390
        , height = 844
        }
        (Just base)
        base.document
        |> Tuple.first
        |> step (ClockTick (Time.millisToPosix 1000000))


step : Msg -> Build.Model -> Build.Model
step msg model =
    let
        ( next, _, _ ) =
            Build.update msg model
    in
    next


{-| The autosave that the last edit scheduled, as it arrives.
-}
autoSave : Int -> Build.Model -> Build.Model
autoSave at model =
    step (AutoSave model.revision (Time.millisToPosix at)) model


partCount : Build.Model -> Int
partCount model =
    List.length (Document.flattenedPlacements (Build.document model))


savedCount : Build.Model -> Int
savedCount model =
    List.length (BuildHistory.entries (Build.history model))


suite : Test
suite =
    case Editor.bases of
        first :: second :: _ ->
            let
                opened =
                    open first

                edited =
                    opened |> step (AddPart "3001.dat")

                saved =
                    autoSave 1002000 edited
            in
            describe "Car editor build history"
                [ test "an opened starter base is not saved" <|
                    \_ ->
                        Expect.equal 0 (savedCount (opened |> step (SelectTab BasesTab) |> step (PageVisibility Browser.Events.Hidden)))
                , test "an edit is saved when its autosave arrives" <|
                    \_ ->
                        Expect.all
                            [ \m -> Expect.equal 1 (savedCount m)
                            , \m -> Expect.equal (Just first.id) (Maybe.map .base (BuildHistory.latest (Build.history m)))
                            , \m -> Expect.equal (Just (partCount m)) (Maybe.map .parts (BuildHistory.latest (Build.history m)))
                            , \m -> Expect.equal (Just 1002000) (Maybe.map .savedAt (BuildHistory.latest (Build.history m)))
                            ]
                            saved
                , test "an autosave overtaken by a later edit does nothing" <|
                    \_ ->
                        let
                            stale =
                                edited.revision
                        in
                        edited
                            |> step (AddPart "3001.dat")
                            |> step (AutoSave stale (Time.millisToPosix 1001000))
                            |> savedCount
                            |> Expect.equal 0
                , test "edits of one session replace their entry" <|
                    \_ ->
                        saved
                            |> step (AddPart "3003.dat")
                            |> autoSave 1003000
                            |> step (AddPart "3003.dat")
                            |> autoSave 1004000
                            |> savedCount
                            |> Expect.equal 1
                , test "hiding the page saves the build at once" <|
                    \_ ->
                        Expect.equal 1 (savedCount (step (PageVisibility Browser.Events.Hidden) edited))
                , test "opening the bases drawer saves the build, and later edits become a new entry" <|
                    \_ ->
                        edited
                            |> step (SelectTab BasesTab)
                            |> step (SelectTab BasesTab)
                            |> step (AddPart "3003.dat")
                            |> autoSave 1005000
                            |> savedCount
                            |> Expect.equal 2
                , test "opening the bases drawer twice without edits saves the build once" <|
                    \_ ->
                        saved
                            |> step (SelectTab BasesTab)
                            |> step (SelectTab BasesTab)
                            |> step (SelectTab BasesTab)
                            |> savedCount
                            |> Expect.equal 1
                , test "another base replaces the build, which stays in the history" <|
                    \_ ->
                        let
                            switched =
                                edited |> step (SelectTab BasesTab) |> step (SelectBase second)
                        in
                        Expect.all
                            [ \m -> Expect.equal (Just second.id) (Maybe.map .id m.base)
                            , \m -> Expect.equal (List.length (Document.flattenedPlacements second.document)) (partCount m)
                            , \m -> Expect.equal 1 (savedCount m)
                            ]
                            switched
                , test "tapping the base the build stands on keeps the build" <|
                    \_ ->
                        Expect.equal (partCount edited) (partCount (edited |> step (SelectTab BasesTab) |> step (SelectBase first)))
                , test "a saved build opens again from the history" <|
                    \_ ->
                        let
                            switched =
                                edited |> step (SelectTab BasesTab) |> step (SelectBase second)

                            restored =
                                case BuildHistory.latest (Build.history switched) of
                                    Just entry ->
                                        switched |> step (SelectTab BasesTab) |> step (RestoreBuild entry)

                                    Nothing ->
                                        switched
                        in
                        Expect.all
                            [ \m -> Expect.equal (Just first.id) (Maybe.map .id m.base)
                            , \m -> Expect.equal (partCount edited) (partCount m)
                            , \m -> Expect.equal True m.baseLocked
                            , \m -> Expect.equal BuildBricksTab m.activeTab
                            ]
                            restored
                , test "the editor starts with the history it is handed" <|
                    \_ ->
                        Build.setHistory (Build.history saved) opened
                            |> savedCount
                            |> Expect.equal 1
                , describe "base and build pictures"
                    [ test "the bases drawer asks for a picture of every starter base" <|
                        \_ ->
                            Build.previewRequests (step (SelectTab BasesTab) opened)
                                |> List.map .key
                                |> Expect.equal (List.map (\b -> "base:" ++ b.id) Editor.bases)
                    , test "every base picture request carries the base's parts and colours" <|
                        \_ ->
                            Build.previewRequests opened
                                |> List.filter (\r -> String.isEmpty r.text || List.isEmpty r.colours)
                                |> List.map .key
                                |> Expect.equal []
                    , test "a saved build is asked for too, under its build link" <|
                        \_ ->
                            let
                                withSaved =
                                    step (SelectTab BasesTab) saved

                                codes =
                                    List.map .code (BuildHistory.entries (Build.history withSaved))
                            in
                            Build.previewRequests withSaved
                                |> List.map .key
                                |> List.filter (\k -> not (String.startsWith "base:" k))
                                |> Expect.equal codes
                    , test "a picture that has arrived is not asked for again" <|
                        \_ ->
                            opened
                                |> step (ModelPreviewLoaded { key = "base:" ++ first.id, url = "data:image/png;base64,AAAA" })
                                |> Build.previewRequests
                                |> List.map .key
                                |> List.member ("base:" ++ first.id)
                                |> Expect.equal False
                    , test "a base card shows its rendered picture once it arrives" <|
                        \_ ->
                            opened
                                |> step (SelectTab BasesTab)
                                |> step (ModelPreviewLoaded { key = "base:" ++ first.id, url = "data:image/png;base64,AAAA" })
                                |> Build.view
                                |> Query.fromHtml
                                |> Query.find [ Selector.attribute (Html.Attributes.attribute "data-base" first.id) ]
                                |> Query.has [ Selector.attribute (Html.Attributes.attribute "data-preview" "rendered") ]
                    , test "the saved builds are listed in the bases drawer" <|
                        \_ ->
                            saved
                                |> step (SelectTab BasesTab)
                                |> Build.view
                                |> Query.fromHtml
                                |> Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "build-history") ]
                                |> Query.findAll [ Selector.tag "button" ]
                                |> Query.count (Expect.equal 1)
                    ]
                ]

        _ ->
            test "has two starter bases" (\_ -> Expect.fail "fewer than two starter bases")
