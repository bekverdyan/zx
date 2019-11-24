module Device.Shortcut exposing (Identifier, Shortcut, decoder, encode)

import Dict
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Shortcut =
    { id : Identifier
    , name : Name
    }


type alias Identifier =
    String


type alias Name =
    String



-- ENCODE


encode : Shortcut -> E.Value
encode shortcut =
    E.object
        [ ( "id", E.string shortcut.id )
        , ( "name", E.string shortcut.name )
        ]



-- DECODE


decoder : D.Decoder Shortcut
decoder =
    D.map2 Shortcut
        (D.field "id" D.string)
        (D.field "name" D.string)



-- MAP


idToString : Identifier -> String
idToString id =
    id
