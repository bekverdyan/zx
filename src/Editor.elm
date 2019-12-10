module Editor exposing (Model(..), Msg(..), update, view)

import Branch as Branch
import Debug
import Device as Device
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Model
    = NotSelected
    | Branch Branch.Model
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
    | BranchMsg Branch.Msg
    | OpenDevice Device.ViewModel
    | OpenBranch Branch
    | NewDevice Branch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeviceMsg cmd ->
            case model of
                Device device ->
                    let
                        updated =
                            Device.update cmd device
                    in
                    ( Device <| Tuple.first updated
                    , Cmd.none
                    )

                _ ->
                    let
                        gag =
                            Debug.log "Operation not permited" "!"
                    in
                    ( model, Cmd.none )

        OpenBranch branch ->
            ( Branch { branch = branch, mode = Branch.Normal }, Cmd.none )

        OpenDevice device ->
            ( Device device, Cmd.none )

        NewDevice branch ->
            ( model, Cmd.none )

        BranchMsg cmd ->
            case model of
                Branch branch ->
                    let
                        updated =
                            Branch.update cmd branch
                    in
                    ( Branch <| Tuple.first updated, Cmd.none )

                _ ->
                    let
                        gag =
                            Debug.log "Operation not permited" "!"
                    in
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        NotSelected ->
            h1 [] [ text "Initial" ]

        NotFound ->
            h1 [] [ text "Not found" ]

        Branch branch ->
            Html.map BranchMsg <|
                Branch.view branch

        Device viewModel ->
            Html.map DeviceMsg <| Device.view viewModel
