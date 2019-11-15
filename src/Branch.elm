module Branch exposing (Branches, decoder, encode)

import Device
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


type alias Branches =
    Dict Identifier Branch


type alias Branch =
    { name : Name
    , shortcuts : Dict Identifier DeviceShortcut
    }


type alias Identifier =
    String


type alias DeviceShortcut =
    { name : Name
    }


type alias Name =
    String



-- ENCODE


encode : Branches -> E.Value
encode branches =
    E.dict idToString encodeBranch branches


encodeBranch : Branch -> E.Value
encodeBranch branch =
    E.object
        [ ( "name", E.string branch.name )
        , ( "shortuts", encodeShortcuts branch.shortcuts )
        ]


encodeShortcuts : Dict Identifier DeviceShortcut -> E.Value
encodeShortcuts shortcuts =
    E.dict idToString encodeShortcut shortcuts


idToString : Identifier -> String
idToString id =
    id


encodeShortcut : DeviceShortcut -> E.Value
encodeShortcut shortcut =
    E.object
        [ ( "name", E.string shortcut.name )
        ]



-- DECODER


decoder : D.Decoder Branch
decoder =
    D.map2 Branch
        (D.field "name" D.string)
        (D.field "shortcuts" decodeShortcuts)


decodeShortcuts : D.Decoder (Dict Identifier DeviceShortcut)
decodeShortcuts =
    D.dict decodeShortcut


decodeShortcut : D.Decoder DeviceShortcut
decodeShortcut =
    D.map DeviceShortcut
        (D.field "name" D.string)



-- CREATE


generateId : Identifier
generateId =
    -- FIXME should be sha1
    "gag"


createBranch : String -> ( Identifier, Branch )
createBranch name =
    ( generateId
    , { name = name
      , shortcuts = Dict.empty
      }
    )


createShortcut : Device.Device -> ( Identifier, DeviceShortcut )
createShortcut device =
    ( device.id
    , { name = device.name }
    )



-- MAP
-- TODO deal with expose necessary functions


addBranch : ( Identifier, Branch ) -> Branches -> Branches
addBranch ( id, branch ) branches =
    Dict.insert id branch branches


removeBranch : Identifier -> Branches -> Branches
removeBranch id branches =
    Dict.remove id branches


addDevice : Device.Device -> Branch -> Branch
addDevice device branch =
    let
        shortcut =
            createShortcut device
    in
    let
        updatedShortcuts =
            Dict.insert
                (Tuple.first shortcut)
                (Tuple.second shortcut)
                branch.shortcuts
    in
    { branch | shortcuts = updatedShortcuts }


removeDevice : Identifier -> Branch -> Branch
removeDevice id branch =
    let
        updatedShortcuts =
            Dict.remove id branch.shortcuts
    in
    { branch | shortcuts = updatedShortcuts }
