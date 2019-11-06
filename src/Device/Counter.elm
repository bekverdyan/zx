module Device.Counter exposing (Counter, decodeCounter, encodeCounter)

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



--CREATOR


newPointer : Int -> Int -> Pointer
newPointer daily total =
    ( daily, total )


newCounter : String -> Pointer -> Counter
newCounter name pointer =
    ( name, pointer )



--ENCODE


encodeCounter : Counter -> E.Value
encodeCounter counter =
    E.object
        [ ( "name", E.string <| Tuple.first counter )
        , ( "pointer", encodePointer <| Tuple.second counter )
        ]


encodePointer : Pointer -> E.Value
encodePointer pointer =
    E.object
        [ ( "daily", E.int (Tuple.first pointer) )
        , ( "total", E.int (Tuple.second pointer) )
        ]



--DECODE


decodeCounter : D.Decoder Counter
decodeCounter =
    D.map2 newCounter
        (D.field "name" D.string)
        (D.field "pointer" decodePointer)


decodePointer : D.Decoder Pointer
decodePointer =
    D.map2 newPointer
        (D.field "daily" D.int)
        (D.field "total" D.int)
