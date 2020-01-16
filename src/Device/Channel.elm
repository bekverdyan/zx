module Device.Channel exposing
    ( Model
    , Msg
    , decoder
    , encode
    , init
    , update
    , view
    )

import Assets.Component as Component
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
    Component.Component



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


channelOf : Index -> List (Maybe Component) -> Channel
channelOf index components =
    ( index, filterValidComponents components )


decodeChannel : D.Decoder Channel
decodeChannel =
    D.map2 channelOf
        (D.field "index" D.int)
        (D.field "components" <|
            D.list Component.decoder
        )


filterValidComponents :
    List (Maybe Component)
    -> List Component
filterValidComponents components =
    List.filterMap
        (\value ->
            case value of
                Just component ->
                    Just component

                Nothing ->
                    Nothing
        )
        components



-- ENCODE


encode : Model -> E.Value
encode ( actual, defined ) =
    E.object
        [ ( "actual", E.int actual )
        , ( "defined", E.list encodeChannel defined )
        ]


encodeChannel : Channel -> E.Value
encodeChannel ( index, components ) =
    E.object
        [ ( "index", E.int index )
        , ( "components"
          , E.list
                Component.encode
                components
          )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    text "actual and defined channels"
