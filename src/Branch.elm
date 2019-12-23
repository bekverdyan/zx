module Branch exposing (Branch, Identifier, Mode(..), Model, Msg(..), createShortcut, decoder, encode, idToString, newBranch, update, view, viewInDashboard)

import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { branch : Branch
    , mode : Mode
    }


type Mode
    = Normal
    | NameEdit String


type alias Branch =
    { id : Identifier
    , name : Name
    , shortcuts : Shortcuts
    }


type alias Identifier =
    String


type alias Name =
    String


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias Shortcuts =
    Dict.Dict DeviceShortcut.Identifier DeviceShortcut.Shortcut



-- CREATE


createShortcut : Branch -> BranchShortcut
createShortcut branch =
    { id = branch.id, name = branch.name }



-- CREATE


newIdentifier : String -> Identifier
newIdentifier salt =
    Hash.sha512_224 salt


newBranch : String -> Branch
newBranch salt =
    let
        id =
            newIdentifier salt

        name =
            String.slice 0 7 <| idToString id
    in
    { id = id
    , name = name
    , shortcuts = Dict.empty
    }



-- ENCODE


encode : Branch -> E.Value
encode branch =
    E.object
        [ ( "id", E.string branch.id )
        , ( "name", E.string branch.name )
        , ( "shortcuts", encodeShortcuts branch.shortcuts )
        ]


encodeShortcuts : Shortcuts -> E.Value
encodeShortcuts shortcuts =
    E.dict idToString DeviceShortcut.encode shortcuts


idToString : Identifier -> String
idToString id =
    id



-- DECODER


decoder : D.Decoder Branch
decoder =
    D.map3 Branch
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "shortcuts" decodeShortcuts)


decodeShortcuts : D.Decoder Shortcuts
decodeShortcuts =
    D.dict DeviceShortcut.decoder



-- MAP
-- TODO deal with expose necessary functions


addShortcut : DeviceShortcut.Shortcut -> Shortcuts -> Shortcuts
addShortcut shortcut shortcuts =
    Dict.insert
        shortcut.id
        shortcut
        shortcuts


removeShortcut : Identifier -> Shortcuts -> Shortcuts
removeShortcut id shortcuts =
    Dict.remove id shortcuts



-- UPDATE


type Msg
    = NameEditMode
    | SetName String
    | NormalMode
    | NewDevice Branch
    | NameInput String


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        NameEditMode ->
            ( { model
                | mode = NameEdit model.branch.name
              }
            , False
            )

        SetName name ->
            let
                branch =
                    model.branch
            in
            ( { model
                | branch =
                    { branch
                        | name = name
                    }
                , mode = Normal
              }
            , True
            )

        NormalMode ->
            ( { model | mode = Normal }, False )

        NewDevice branch ->
            ( model, False )

        NameInput value ->
            ( { model | mode = NameEdit value }, False )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ div [ class "header" ]
            [ case model.mode of
                NameEdit editable ->
                    viewNameEditMode editable

                _ ->
                    viewNameNormalMode model.branch.name
            ]
        , div [ class "content" ] [ viewControlPanel model ]
        ]


viewInDashboard : msg -> Branch -> Html msg -> Html msg
viewInDashboard openBranchCmd branch shortcuts =
    li [ class "pure-menu-item" ]
        [ a
            [ class "pure-menu-link"
            , href "#"
            , onClick openBranchCmd
            ]
            [ text branch.name ]
        ]


viewNameNormalMode : String -> Html Msg
viewNameNormalMode name =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ h2 []
                [ label
                    [ for "branchName"
                    , onClick NameEditMode
                    ]
                    [ text name ]
                ]
            , span
                [ class "pure-form-message-inline" ]
                [ text "Click to edit" ]
            ]
        ]


viewNameEditMode : String -> Html Msg
viewNameEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "branchName"
                , placeholder "Branch name"
                , onInput NameInput
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SetName editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewControlPanel : Model -> Html Msg
viewControlPanel model =
    Html.form [ class "pure-form pure-form-stacked" ]
        [ fieldset []
            [ legend [] [ text "Control panel" ]
            , label [] [ text "Այստեղ կարող է լինել ձեր գովազդը" ]
            , button
                [ class "pure-button button-warning"
                , onClick <| NewDevice model.branch
                ]
                [ text "New Device" ]
            ]
        ]
