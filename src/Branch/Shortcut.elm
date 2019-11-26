module Branch.Shortcut exposing (Shortcut, decoder, encode, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Shortcut =
    { id : Identifier, name : Name }


type alias Identifier =
    String


type alias Name =
    String



-- ENCODE


encode : Shortcut -> ( String, E.Value )
encode shortcut =
    ( "shortcut", encodeShortcut shortcut )


encodeShortcut : Shortcut -> E.Value
encodeShortcut shortcut =
    E.object
        [ ( "id", E.string shortcut.id )
        , ( "name", E.string shortcut.name )
        ]



-- DECODER


decoder : D.Decoder Shortcut
decoder =
    D.field "shortcut" decodeShortcut


decodeShortcut : D.Decoder Shortcut
decodeShortcut =
    D.map2 Shortcut
        (D.field "id" D.string)
        (D.field "name" D.string)



-- VIEW


view : Shortcut -> Html msg
view shortcut =
    li [] [ span [] [ text shortcut.name ] ]
