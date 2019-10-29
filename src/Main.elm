module Main exposing (Model, init, main)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type alias Model =
    { carwashes : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0, Cmd.none )


type Msg
    = SamuilArshak



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SamuilArshak ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Դատարկ մարդ"
    , body = [ text "" ]
    }
