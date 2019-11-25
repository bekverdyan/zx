module Branch exposing (Branch, Identifier, createShortcut, decoder, encode, idToString, newBranch, view)

import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


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


newBranch : String -> String -> Branch
newBranch name salt =
    { id = newIdentifier salt
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



-- VIEW


view : Branch -> Html msg
view branch =
    li []
        [ span [ class "caret" ] [ text branch.name ]
        , viewBranchShortcuts branch.shortcuts
        ]


viewBranchShortcuts : Shortcuts -> Html msg
viewBranchShortcuts shortcuts =
    let
        shortcutsList =
            Dict.values shortcuts
    in
    ul [ class "nested" ]
        (List.map
            viewBranchShortcut
            shortcutsList
        )


viewBranchShortcut : DeviceShortcut.Shortcut -> Html msg
viewBranchShortcut shortcut =
    li [] [ span [] [ text shortcut.name ] ]
