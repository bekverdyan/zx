module Device.Counter exposing (Counters, decoder, encode, newCounters)

import Json.Decode as D
import Json.Encode as E


type alias Counters =
    List Counter


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


createPointer : Daily -> Total -> Pointer
createPointer daily total =
    ( daily, total )


createCounter : Name -> Pointer -> Counter
createCounter name pointer =
    ( name, pointer )


newPointer : Pointer
newPointer =
    ( 0, 0 )


newCounter : Name -> Counter
newCounter name =
    ( name, newPointer )


newCounters : List Name -> Counters
newCounters names =
    List.map newCounter names



--ENCODE


encode : Counters -> E.Value
encode counters =
    E.list encodeCounter counters


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


decoder : D.Decoder Counters
decoder =
    D.list decodeCounter


decodeCounter : D.Decoder Counter
decodeCounter =
    D.map2 createCounter
        (D.field "name" D.string)
        (D.field "pointer" decodePointer)


decodePointer : D.Decoder Pointer
decodePointer =
    D.map2 createPointer
        (D.field "daily" D.int)
        (D.field "total" D.int)



-- TODO mappers (should be used only with some setting changests)
