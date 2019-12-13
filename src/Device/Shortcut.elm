module Device.Shortcut exposing (Identifier, Shortcut, decoder, encode, view)

-- import Bootstrap.Button as Button
-- import Bootstrap.ListGroup as ListGroup
-- import Bootstrap.Utilities.Spacing as Spacing

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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



-- VIEW


view : msg -> Shortcut -> Html msg
view openDeviceCmd shortcut =
    li [ id shortcut.id ]
        [ a [ onClick openDeviceCmd ]
            [ label [ attribute "for" shortcut.id ] [ text shortcut.name ]
            , input
                [ attribute "checked" ""
                , attribute "id" shortcut.id
                , attribute "value" ""
                , attribute "type" "checkbox"
                ]
                []
            ]
        ]
