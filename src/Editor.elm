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


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        BranchMsg branchMsg ->
            case model of
                Branch branch ->
                    let
                        ( updated, saveMe ) =
                            Branch.update branchMsg branch
                    in
                    ( Branch updated, saveMe )

                _ ->
                    let
                        gag =
                            Debug.log "This case should not happen basically" "!"
                    in
                    ( NotSelected, False )

        DeviceMsg deviceMsg ->
            case model of
                Device device ->
                    let
                        ( updated, saveMe ) =
                            Device.update deviceMsg device
                    in
                    ( Device updated, saveMe )

                _ ->
                    let
                        gag =
                            Debug.log "This case should not happen basically" "!"
                    in
                    ( NotSelected, False )


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
            Html.map DeviceMsg <|
                Device.view viewModel
