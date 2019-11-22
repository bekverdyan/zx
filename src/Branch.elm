module Branch exposing (Branch, Branches, Identifier, Msg, createShortcut, decoder, encode, newBranch, pullBranches, update)

import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
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


type alias BranchShortcut =
    BranchShortcut.Shortcut



-- UPDATE


type Msg
    = GenerateBranch String


update : Msg -> Maybe Branch -> ( ( Identifier, Branch ), Maybe Msg )
update msg branch =
    case msg of
        GenerateBranch salt ->
            ( newBranch "exo" salt, Nothing )



-- CREATE


createShortcut : ( Identifier, Branch ) -> BranchShortcut
createShortcut ( id, branch ) =
    { id = id, name = branch.name }



-- CREATE


newIdentifier : String -> Identifier
newIdentifier salt =
    Hash.sha512 salt


newBranch : String -> String -> ( Identifier, Branch )
newBranch name salt =
    ( newIdentifier salt
    , { name = name
      , shortcuts = Dict.empty
      }
    )



-- FLAG


pullBranches : D.Value -> Maybe Branches
pullBranches value =
    handleResult <|
        D.decodeValue decoder value


handleResult : Result D.Error Branches -> Maybe Branches
handleResult result =
    case result of
        Ok branches ->
            Just branches

        Err _ ->
            Nothing



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


decoder : D.Decoder Branches
decoder =
    D.field "branches" <| D.dict decodeBranch


decodeBranch : D.Decoder Branch
decodeBranch =
    D.map2 Branch
        (D.field "name" D.string)
        DeviceShortcut.decoder



-- MAP
-- TODO deal with expose necessary functions


addBranch : ( Identifier, Branch ) -> Branches -> Branches
addBranch ( id, branch ) branches =
    Dict.insert id branch branches


removeBranch : Identifier -> Branches -> Branches
removeBranch id branches =
    Dict.remove id branches
