module BuildPageTest exposing (suite)

{-| The car editor screen (`Pages.Build`) keeps the base in one piece: with
the base selected, the keys and buttons that would ungroup, delete, cut,
duplicate or regroup it leave the build as it was, while parts added from the
dock can still be turned and deleted.
-}

import Expect
import I18n exposing (Language(..))
import LDraw.Document as Document exposing (Document)
import Pages.Build as Build exposing (Msg(..))
import Racer.Editor as Editor exposing (Base)
import Studio.Core as Core
import Studio.Shortcuts exposing (TurnDirection(..))
import Test exposing (Test, describe, test)
import Viewport


suite : Test
suite =
    describe "Car editor screen"
        (List.map baseTest Editor.bases
            ++ [ test "custom document without starter base opens and can be edited" <|
                    \_ ->
                        let
                            doc =
                                Document.parse "0 Name: Custom\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"

                            opened =
                                Build.init
                                    { ldrawBase = ""
                                    , ldrawFallbackBase = ""
                                    , supportsPointerEvents = True
                                    , language = En
                                    , width = 390
                                    , height = 844
                                    }
                                    Nothing
                                    doc
                                    |> Tuple.first

                            added =
                                step (AddPart "3001.dat") opened
                        in
                        Expect.equal (partCount doc + 1) (partCount (Build.document added))
               ]
        )


baseTest : Base -> Test
baseTest base =
    let
        opened =
            Build.init
                { ldrawBase = ""
                , ldrawFallbackBase = ""
                , supportsPointerEvents = True
                , language = En
                , width = 390
                , height = 844
                }
                (Just base)
                base.document
                |> Tuple.first

        baseId =
            Document.mainPlacements base.document |> List.map .id |> List.head |> Maybe.withDefault -1

        withBaseSelected =
            step (StudioMsg (Core.SelectOnly baseId)) opened

        attempts =
            [ ( "Ctrl+Shift+G ungroups", key "G" True True )
            , ( "Delete", key "Delete" False False )
            , ( "Backspace", key "Backspace" False False )
            , ( "Ctrl+X cuts", key "x" False True )
            , ( "Ctrl+D duplicates", key "d" False True )
            , ( "Ctrl+G regroups", key "g" False True )
            , ( "the delete button", DeleteSelected )
            ]
    in
    describe base.id
        (List.map
            (\( label, msg ) ->
                test (label ++ " leaves the base in one piece") <|
                    \_ ->
                        let
                            after =
                                Build.document (step msg withBaseSelected)
                        in
                        Expect.all
                            [ \d -> Expect.equal True (Editor.baseIntact base d)
                            , \d -> Expect.equal (partCount base.document) (partCount d)
                            ]
                            after
            )
            attempts
            ++ [ test "the studio alone would ungroup it (so the checks above mean something)" <|
                    \_ ->
                        case key "G" True True of
                            StudioMsg studioMsg ->
                                let
                                    ( studio, _, _ ) =
                                        Core.update studioMsg withBaseSelected.studio
                                in
                                Expect.equal False (Editor.baseIntact base (Core.getDocument studio))

                            _ ->
                                Expect.fail "not a studio message"
               , test "added parts can be turned and deleted" <|
                    \_ ->
                        let
                            added =
                                opened |> step (AddPart "3001.dat") |> step (AddPart "3039.dat")

                            turned =
                                step (Turn TurnLeft) added

                            deleted =
                                step DeleteSelected turned
                        in
                        Expect.all
                            [ \_ -> Expect.equal (partCount base.document + 2) (partCount (Build.document added))
                            , \_ -> Expect.notEqual (Build.document added |> placementMatrices) (Build.document turned |> placementMatrices)
                            , \_ -> Expect.equal (partCount base.document + 1) (partCount (Build.document deleted))
                            , \_ -> Expect.equal True (Editor.baseIntact base (Build.document deleted))
                            ]
                            ()
               , test "picking a colour recolours the selected part" <|
                    \_ ->
                        let
                            added =
                                opened |> step (AddPart "3001.dat") |> step (PickColour 14)

                            colours =
                                Document.mainPlacements (Build.document added) |> List.map .color
                        in
                        Expect.equal True (List.member 14 colours)
               , test "toggling base lock unlocks and explodes base grouping into individual placements" <|
                    \_ ->
                        let
                            unlocked =
                                step ToggleBaseLock opened

                            doc =
                                Build.document unlocked
                        in
                        Expect.all
                            [ \_ -> Expect.equal False unlocked.baseLocked
                            , \_ -> Expect.equal False (List.any (Editor.isBasePlacement base) (Document.mainPlacements doc))
                            , \_ -> Expect.equal base.partCount (List.length (Document.mainPlacements doc))
                            ]
                            ()
               ]
        )


step : Msg -> Build.Model -> Build.Model
step msg model =
    let
        ( next, _, _ ) =
            Build.update msg model
    in
    next


key : String -> Bool -> Bool -> Msg
key k shift ctrl =
    StudioMsg
        (Core.ViewportMsg
            (Viewport.KeyPressed
                { key = k
                , shift = shift
                , ctrl = ctrl
                , meta = False
                , alt = False
                , repeat = False
                , targetTag = "BODY"
                , targetEditable = False
                , targetRole = ""
                }
            )
        )


partCount : Document -> Int
partCount doc =
    List.length (Document.flattenedPlacements doc)


placementMatrices : Document -> List String
placementMatrices doc =
    Document.mainPlacements doc |> List.map (\p -> Debug.toString p.matrix)
