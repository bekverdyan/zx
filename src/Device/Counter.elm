module Device.Counter exposing (Counter, encodeCounter)

import Json.Decode as D
import Json.Encode as E


type alias Counter =
    ( Name, Pointer )


type alias Name =
    String


type alias Pointer =
    ( Daily, Total )


type alias Daily =
    Int


type alias Total =
    Int



--ENCODE


encodeCounter : Counter -> E.Value
encodeCounter counter =
    E.object
        [ ( "name", E.string <| Tuple.first counter )
        , ( "pointer", encodePointer <| Tuple.second counter )
        ]


encodeName : Name -> E.Value
encodeName name =
    E.object [ ( "name", E.string name ) ]


encodePointer : Pointer -> E.Value
encodePointer pointer =
    E.object
        [ ( "daily", E.int (Tuple.first pointer) )
        , ( "total", E.int (Tuple.second pointer) )
        ]
