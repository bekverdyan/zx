module Editor exposing (Model(..))

import Branch as Branch
import Device as Device
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Model
    = NotSelected
    | Branch Branch
    | Device Device
    | NotFound


type alias Branch =
    Branch.Branch


type alias Device =
    Device.Device


view : Model -> msg -> Html msg
view model cmdMsg =
    case model of
        NotSelected ->
            h1 [] [ text "Initial" ]

        NotFound ->
            h1 [] [ text "Not found" ]

        Branch branch ->
            text branch.name

        Device device ->
            text device.name
