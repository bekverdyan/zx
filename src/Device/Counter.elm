module Device.Counter exposing (Model, Msg, decoder, encode, newCounters, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Model =
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


newCounters : List Name -> Model
newCounters names =
    List.map newCounter names



--ENCODE


encode : Model -> E.Value
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


decoder : D.Decoder Model
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



-- UPDATE


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Bool )
update msg model =
    ( model, False )



-- VIEW


view : Model -> Html Msg
view counters =
    text "counters"



-- TODO mappers (should be used only with some setting changests)
