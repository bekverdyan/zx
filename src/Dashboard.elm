module Dashboard exposing
    ( BranchView
    , Model(..)
    , Msg(..)
    , compact
    , initBranchView
    , toggleById
    , view
    )

import Branch
import Branch.Shortcut as BranchShortcut
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
    Dict Branch.Identifier BranchView


type BranchView
    = Compact Branch
    | Opened ( Branch, List DeviceShortcut )


type alias Branch =
    Branch.Branch


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias DeviceShortcut =
    DeviceShortcut.Shortcut



-- UPDATE


type Msg
    = SelectBranch Branch.Identifier
    | SelectDevice DeviceShortcut.Identifier
    | NewBranch


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        SelectBranch id ->
            case model of
                Branches branches ->
                    ( Branches <| toggleById id branches
                    , False
                    )

                _ ->
                    -- let
                    --     log = Debug.log "This should not happen"
                    --     "!"
                    -- in
                    ( model, False )

        SelectDevice id ->
            ( model, False )

        NewBranch ->
            ( model, False )



-- MAP


initBranchView : Branch.Identifier -> Branch -> BranchView
initBranchView id branch =
    Compact branch


toggleById : Branch.Identifier -> Branches -> Branches
toggleById id branches =
    let
        checkThenToggle :
            Branch.Identifier
            -> BranchView
            -> BranchView
        checkThenToggle idAtHand branchView =
            if id == idAtHand then
                toggle branchView

            else
                branchView
    in
    Dict.map checkThenToggle branches


toggle : BranchView -> BranchView
toggle model =
    case model of
        Compact branch ->
            Opened ( branch, Dict.values branch.shortcuts )

        Opened ( branch, shortcuts ) ->
            Compact branch


compact : Branch.Identifier -> BranchView -> BranchView
compact id model =
    case model of
        Compact branch ->
            Compact branch

        Opened _ ->
            toggle model



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
            [ class "pure-menu-link label-error"
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


viewBranchWithCmd : BranchView -> Html Msg
viewBranchWithCmd model =
    case model of
        Compact branch ->
            BranchShortcut.viewCompact
                (SelectBranch branch.id)
            <|
                Branch.createShortcut branch

        Opened ( branch, shortcuts ) ->
            BranchShortcut.viewOpened
                (SelectBranch branch.id)
                (Branch.createShortcut branch)
                (div [] <|
                    List.map
                        viewDeviceShortcutWithCmd
                        shortcuts
                )


viewDeviceShortcutWithCmd : DeviceShortcut -> Html Msg
viewDeviceShortcutWithCmd shortcut =
    DeviceShortcut.view (SelectDevice shortcut.id) shortcut
