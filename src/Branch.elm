module Branch exposing (Branches, decoder, encode)

import Json.Decode as D
import Json.Encode as E


type alias Branches =
    List Branch


type alias Branch =
    { id : Identifier
    , name : String
    , devices : Labels
    }


type alias Identifier =
    String


type alias Labels =
    List Label


type alias Label =
    { deviceId : String
    , deviceName : String
    }


newIdentifier : Identifier
newIdentifier =
    -- FIXME should be sha1
    "bar"



-- ENCODE


encode : Branches -> E.Value
encode branches =
    E.object
        [ ( "branches", E.list encodeBranch branches ) ]


encodeBranch : Branch -> E.Value
encodeBranch branch =
    E.object
        [ ( "id", E.string branch.id )
        , ( "name", E.string branch.name )
        , ( "devices", E.list encodeLabel branch.devices )
        ]


encodeLabel : Label -> E.Value
encodeLabel label =
    E.object
        [ ( "deviceId", E.string label.deviceId )
        , ( "deviceName", E.string label.deviceName )
        ]



-- DECODER


decoder : D.Decoder Branches
decoder =
    D.field "branches" <| D.list decodeBranch


decodeBranch : D.Decoder Branch
decodeBranch =
    D.map3 Branch
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "devices" <| D.list decodeLabel)


decodeLabel : D.Decoder Label
decodeLabel =
    D.map2 Label
        (D.field "deviceId" D.string)
        (D.field "deviceName" D.string)
