module Dashboard exposing (Model(..), view)

import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Branch
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Model
    = Empty
    | Branches Branches


type alias Branches =
    Dict Branch.Identifier Branch


type alias Branch =
    Branch.Branch


type alias DeviceShortcut =
    DeviceShortcut.Shortcut



-- UPDATE


type Msg
    = OpenBranch Branch.Identifier
    | OpenDevice DeviceShortcut.Identifier
    | NewBranch
    | LoadBranches



-- update : Msg -> Model -> (Model, Cmd Msg)
-- update msg model =
--   case msg of
--     OpenBranch Branch ->
--       -
--
-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Empty ->
            text "you have no branches yet"

        Branches branches ->
            div []
                [ viewBranches branches
                , Button.button
                    [ Button.dark
                    , Button.attrs
                        [ Spacing.ml1, onClick NewBranch ]
                    ]
                    [ text "Create Branch" ]
                ]


viewBranches : Branches -> Html Msg
viewBranches branches =
    ul [ class "tree" ] <|
        List.map
            viewBranchWithCmd
            (Dict.values branches)


viewBranchWithCmd : Branch -> Html Msg
viewBranchWithCmd branch =
    Branch.viewInDashboard
        (OpenBranch branch.id)
        branch
        (ul [] <|
            List.map viewDeviceShortcutWithCmd <|
                Dict.values branch.shortcuts
        )


viewDeviceShortcutWithCmd : DeviceShortcut -> Html Msg
viewDeviceShortcutWithCmd shortcut =
    DeviceShortcut.view (OpenDevice shortcut.id) shortcut
