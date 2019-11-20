module Device.Shortcut exposing (Shortcut, Shortcuts, addShortcut, decoder, encode, removeShortcut)

import Dict
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Shortcuts =
    Dict.Dict Identifier Shortcut


type alias Shortcut =
    { name : Name
    }


type alias Identifier =
    String


type alias Name =
    String



-- ENCODE


encode : Shortcuts -> ( String, E.Value )
encode shortcuts =
    ( "shortcuts", E.dict idToString encodeShortcut shortcuts )


encodeShortcut : Shortcut -> E.Value
encodeShortcut shortcut =
    E.object
        [ ( "name", E.string shortcut.name )
        ]



-- DECODE


decoder : D.Decoder Shortcuts
decoder =
    D.field "shortcuts" <| D.dict decodeShortcut


decodeShortcut : D.Decoder Shortcut
decodeShortcut =
    D.map Shortcut
        (D.field "name" D.string)



-- MAP


idToString : Identifier -> String
idToString id =
    id


addShortcut : ( Identifier, Shortcut ) -> Shortcuts -> Shortcuts
addShortcut ( id, shortcut ) shortcuts =
    Dict.insert
        id
        shortcut
        shortcuts


removeShortcut : Identifier -> Shortcuts -> Shortcuts
removeShortcut id shortcuts =
    Dict.remove id shortcuts
