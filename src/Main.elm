port module Main exposing (Model, init, main)

import Branch
import Branch.Shortcut as BranchShortcut
import Browser
import Debug
import Device
import Device.Shortcut as DeviceShortcut
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Encode as E


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- PORT


port saveBranches : E.Value -> Cmd msg


port saveDevices : E.Value -> Cmd msg


port loadBranches : (E.Value -> msg) -> Sub msg


port loadDevices : (E.Value -> msg) -> Sub msg



-- MODEL


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type alias Model =
    { branches : Maybe Branch.Branches
    , devices : Maybe Device.Devices
    }


type alias Flags =
    { branches : D.Value
    , devices : D.Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        (Branch.pullBranches flags.branches)
      <|
        Device.pullDevices flags.devices
    , Cmd.none
    )


type Msg
    = NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Դատարկ մարդ"
    , body = [ text "դիվայս" ]
    }
