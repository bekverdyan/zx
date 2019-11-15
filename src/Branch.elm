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



-- MAP
-- TODO implement me
-- addBranch : String -> Branches -> Branches
-- TODO implement me
-- removeBranch : Identifier -> Branches -> Branches
-- TODO implement me
-- addDevice : Device.Device -> Branch -> Branch
-- TODO implement me
-- removeDevice : Identifier -> Branch -> Branch
