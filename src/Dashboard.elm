module Dashboard exposing (Model(..), Msg(..), view)

import Branch
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



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "menu" ]
        [ div [ class "pure-menu" ]
            [ a
                [ class "pure-menu-heading"
                , href "#"
                ]
                [ text "ZX" ]
            , case model of
                Empty ->
                    viewBranches Dict.empty

                Branches branches ->
                    viewBranches branches

                Error ->
                    viewBranches Dict.empty
            ]
        ]


viewNewBranchButton : Html Msg
viewNewBranchButton =
    li [ class "pure-menu-item" ]
        [ a
            [ class "pure-menu-link button-primary"
            , href "#"
            , onClick NewBranch
            ]
            [ text "New Branch" ]
        ]


viewBranches : Branches -> Html Msg
viewBranches branches =
    ul [ class "pure-menu-list" ] <|
        List.append
            (List.map
                viewBranchWithCmd
                (Dict.values branches)
            )
            [ viewNewBranchButton ]


viewBranchWithCmd : Branch -> Html Msg
viewBranchWithCmd branch =
    Branch.viewInDashboard
        (SelectBranch branch.id)
        branch
        (div [] <|
            List.map viewDeviceShortcutWithCmd <|
                Dict.values branch.shortcuts
        )


viewDeviceShortcutWithCmd : DeviceShortcut -> Html Msg
viewDeviceShortcutWithCmd shortcut =
    DeviceShortcut.view (SelectDevice shortcut.id) shortcut
