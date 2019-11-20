module Branch exposing (Branch, Branches, Identifier, decoder, encode)

import Branch.Shortcut as BranchShortcut
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


type alias Branches =
    Dict Identifier Branch


type alias Branch =
    { name : Name
    , shortcuts : DeviceShortcut.Shortcuts
    }


type alias Identifier =
    String


type alias Name =
    String



-- CREATE


create : ( Identifier, Branch ) -> BranchShortcut.Shortcut
create ( id, branch ) =
    { id = id, name = branch.name }



-- ENCODE


encode : Branches -> E.Value
encode branches =
    E.dict idToString encodeBranch branches


encodeBranch : Branch -> E.Value
encodeBranch branch =
    E.object
        [ ( "name", E.string branch.name )
        , DeviceShortcut.encode branch.shortcuts
        ]


idToString : Identifier -> String
idToString id =
    id



-- DECODER


decoder : D.Decoder Branch
decoder =
    D.map2 Branch
        (D.field "name" D.string)
        DeviceShortcut.decoder



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



-- MAP
-- TODO deal with expose necessary functions


addBranch : ( Identifier, Branch ) -> Branches -> Branches
addBranch ( id, branch ) branches =
    Dict.insert id branch branches


removeBranch : Identifier -> Branches -> Branches
removeBranch id branches =
    Dict.remove id branches
