module Main exposing (Model, init, main)

import Browser
import Debug
import Device as Device
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
    { carwashes : Int
    , device : Device.Device
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        0
      <|
        Device.newDevice Device.Washbox
    , Cmd.none
    )


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
    , body = [ text "դիվայս" ]
    }
