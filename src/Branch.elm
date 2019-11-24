module Branch exposing (Branch, Identifier, Msg, createShortcut, decoder, encode, idToString, newBranch, update)

import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
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



-- UPDATE


type Msg
    = GenerateBranch String


update : Msg -> Maybe Branch -> ( Branch, Maybe Msg )
update msg branch =
    case msg of
        GenerateBranch salt ->
            ( newBranch "exo" salt, Nothing )



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
        , encodeShortcuts branch.shortcuts
        ]


encodeShortcuts : Shortcuts -> ( String, E.Value )
encodeShortcuts shortcuts =
    ( "shortcuts", E.dict idToString DeviceShortcut.encode shortcuts )


idToString : Identifier -> String
idToString id =
    id



-- DECODER


decoder : D.Decoder Branch
decoder =
    D.map3 Branch
        (D.field "id" D.string)
        (D.field "name" D.string)
        decodeShortcut


decodeShortcut : D.Decoder Shortcuts
decodeShortcut =
    D.field "shortcuts" <| D.dict DeviceShortcut.decoder



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
