module Device.Channel exposing (Model, Msg, decoder, encode, newChannels, update, view)

import Html exposing (..)
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Model =
    ( Actual, Defined )


type alias Actual =
    Int


type alias Defined =
    List Channel


type alias Channel =
    ( Index, List Ingredient )


type alias Index =
    Int


type alias Ingredient =
    ( Resource, Portion )


type alias Portion =
    Int


type alias Resource =
    ( Name, Unit )


type alias Unit =
    String


type alias Name =
    String



-- CREATE


newChannels : Actual -> Model
newChannels actual =
    ( actual, [] )



-- UPDATE


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Bool )
update msg model =
    ( model, False )



-- DECODE


decoder : D.Decoder Model
decoder =
    D.map2 channelsOf
        (D.field "actual" D.int)
        (D.field "defined" <| D.list decodeComponent)


channelsOf : Actual -> Defined -> Model
channelsOf actual defined =
    ( actual, defined )


componentOf : Int -> List Ingredient -> Channel
componentOf index ingredients =
    ( index, ingredients )


decodeComponent : D.Decoder Channel
decodeComponent =
    D.map2 componentOf
        (D.field "index" D.int)
        (D.field "ingredients" <| D.list decodeIngredient)


newIngredient : Resource -> Portion -> Ingredient
newIngredient resource portion =
    ( resource, portion )


newResource : Name -> Unit -> Resource
newResource name unit =
    ( name, unit )


decodeIngredient : D.Decoder Ingredient
decodeIngredient =
    D.map2 newIngredient
        (D.field "resource" decodeResource)
        (D.field "portion" D.int)


decodeResource : D.Decoder Resource
decodeResource =
    D.map2 newResource
        (D.field "name" D.string)
        (D.field "unit" D.string)



-- ENCODE


encodeResource : Resource -> E.Value
encodeResource resource =
    E.object
        [ ( "name", E.string <| Tuple.first resource ) ]


encodeIngredient : Ingredient -> E.Value
encodeIngredient ingredient =
    E.object
        [ ( "resource", encodeResource <| Tuple.first ingredient )
        , ( "portion", E.int <| Tuple.second ingredient )
        ]


encodeChannel : Channel -> E.Value
encodeChannel channel =
    E.object
        [ ( String.fromInt <| Tuple.first channel
          , E.list encodeIngredient <| Tuple.second channel
          )
        ]


encodeDefined : Defined -> E.Value
encodeDefined defined =
    E.list encodeChannel defined


encode : Model -> E.Value
encode ( actual, defined ) =
    E.object
        [ ( "actual", E.int actual )
        , ( "defined", encodeDefined defined )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    text "actual and defined channels"
