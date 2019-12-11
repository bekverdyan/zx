module Dashboard exposing (Model(..), Msg(..), update, view)

import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Branch
import Debug
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Model
    = Empty
    | Branches Branches
    | Error


type alias Branches =
    Dict Branch.Identifier Branch


type alias Branch =
    Branch.Branch


type alias DeviceShortcut =
    DeviceShortcut.Shortcut



-- UPDATE


type Msg
    = NewBranch
    | SelectBranch Branch.Identifier
    | SelectDevice DeviceShortcut.Identifier


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
            case model of
                Empty ->
                    text "You have no branches yet!"

                Branches branches ->
                    viewBranches branches

                Error ->
                    text "Failed to load branches!"
    in
    div []
        [ content
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
        (SelectBranch branch.id)
        branch
        (ul [] <|
            List.map viewDeviceShortcutWithCmd <|
                Dict.values branch.shortcuts
        )


viewDeviceShortcutWithCmd : DeviceShortcut -> Html Msg
viewDeviceShortcutWithCmd shortcut =
    DeviceShortcut.view (SelectDevice shortcut.id) shortcut
