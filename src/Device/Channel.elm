module Device.Channel exposing
    ( Model
    , Msg
    , decoder
    ,  encode
       -- , newChannels

    , update
    , view
    )

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
    ( Index, List Component )


type alias Index =
    Int


type alias Component =
    ( Name, Unit )


type alias Name =
    String


type Unit
    = Liter Int
    | Gram Int
    | Kilowatt Int
    | Meter Int



-- type alias Ingredient =
--     ( Resource, Portion )
-- type alias Portion =
--     Int
--
-- type alias Resource =
--     ( Name, Unit )
--
-- type alias Unit =
--     String
-- CREATE


init : Actual -> Model
init actual =
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
        (D.field "defined" <| D.list decodeChannel)


channelsOf : Actual -> Defined -> Model
channelsOf actual defined =
    ( actual, defined )


decodeChannel : D.Decoder Channel
decodeChannel =
    D.map2 ( Index, unitFrom )
        (D.field "index" D.int)
        (D.field "component" decodeComponent)


decodeComponent : D.Decoder Compoent
decodeComponent =
    D.map2 ( Name, unitFrom )
        (D.field "name" D.string)
        (D.field "unit" decodeUnit)


decodeUnit : D.Decoder Unit
decodeUnit =
    D.map2 unitFrom
        (D.field "name" D.string)
        (D.field "value" D.int)


unitFrom : String -> Int -> Maybe Unit
unitFrom name value =
    case name of
        "liter" ->
            Just <| Liter value

        "gram" ->
            Just <| Gram value

        "kilowatt" ->
            Just <| Kilowatt value

        "meter" ->
            Just <| Meter value

        _ ->
            Nothing



-- ENCODE


encode : Model -> E.Value
encode ( actual, defined ) =
    E.object
        [ ( "actual", E.int actual )

        -- , ( "defined", E.list <| encodeComponent defined )
        ]



--
-- encodeResource : Resource -> E.Value
-- encodeResource resource =
--     E.object
--         [ ( "name", E.string <| Tuple.first resource ) ]
--
--
-- encodeIngredient : Ingredient -> E.Value
-- encodeIngredient ingredient =
--     E.object
--         [ ( "resource", encodeResource <| Tuple.first ingredient )
--         , ( "portion", E.int <| Tuple.second ingredient )
--         ]
-- encodeComponent : Component -> E.Value
-- encodeComponent (name, unit) =
-- case
-- E.object
--     [ ( String.fromInt <| Tuple.first channel
--       , E.list encodeIngredient <| Tuple.second channel
--       )
--     ]
-- VIEW


view : Model -> Html Msg
view model =
    text "actual and defined channels"
