module Editor exposing (Model(..), Msg, update, view)

import Branch as Branch
import Debug
import Device as Device
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Model
    = NotSelected
    | Branch Branch
    | Device Device.ViewModel
    | NotFound


type alias Content =
    { device : Device.ViewModel
    , branch : Branch
    }


type alias Branch =
    Branch.Branch


type alias Device =
    Device.Device


type Msg
    = DeviceMsg Device.Msg
    | OpenBranch Branch
    | OpenDevice Device.ViewModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeviceMsg deviceMsg ->
            case model of
                Device device ->
                    let
                        updated =
                            Device.update deviceMsg device
                    in
                    ( Device <| Tuple.first updated
                    , Cmd.map DeviceMsg <| Tuple.second updated
                    )

                _ ->
                    let
                        gag =
                            Debug.log "Operation not permited"
                    in
                    ( model, Cmd.none )

        OpenBranch branch ->
            ( Branch branch, Cmd.none )

        OpenDevice device ->
            ( Device device, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        NotSelected ->
            h1 [] [ text "Initial" ]

        NotFound ->
            h1 [] [ text "Not found" ]

        Branch branch ->
            text branch.name

        Device viewModel ->
            Html.map DeviceMsg <| Device.view viewModel
