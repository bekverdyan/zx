port module Storage exposing
    ( Model
    , loadBranches
    , loadDevices
    , pushBranches
    , pushDevices
    )

import Branch
import Device
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E



-- PORT


port saveBranches : E.Value -> Cmd msg


port saveDevices : E.Value -> Cmd msg


port loadBranches : (E.Value -> msg) -> Sub msg


port loadDevices : (E.Value -> msg) -> Sub msg



-- MODEL


type alias Model =
    { devices : Devices
    , branches : Branches
    }



-- ALIAS


type alias Branches =
    Dict BranchId Branch


type alias Devices =
    Dict DeviceId Device


type alias DeviceId =
    Device.Identifier


type alias BranchId =
    Branch.Identifier


type alias Device =
    Device.Device


type alias Branch =
    Branch.Branch



-- UPDATE


type Msg
    = PullDevices E.Value
    | PullBranches E.Value
    | PushDevices (Dict DeviceId Device)
    | PushBranches (Dict BranchId Branch)
    | AddDevice ( Device, Branch )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PullDevices encoded ->
            ( { model
                | devices = decodeDevices encoded
              }
            , Cmd.none
            )

        PullBranches encoded ->
            ( { model
                | branches = decodeBranches encoded
              }
            , Cmd.none
            )

        PushDevices devices ->
            ( model
            , saveDevices <|
                encodeDevices devices
            )

        PushBranches branches ->
            ( model
            , saveBranches <|
                encodeBranches branches
            )

        AddDevice ( device, container ) ->
            let
                devices =
                    model.devices

                branches =
                    model.branches
            in
            ( { model
                | devices =
                    Dict.insert
                        device.id
                        device
                        devices
                , branches =
                    Dict.insert
                        container.id
                        container
                        branches
              }
            , Cmd.batch []
            )



-- WRITE


pushBranches : Maybe Branches -> Cmd msg
pushBranches dashboard =
    case dashboard of
        Just branches ->
            saveBranches <| encodeBranches branches

        Nothing ->
            Cmd.none


pushDevices : Maybe Devices -> Cmd msg
pushDevices devices =
    case devices of
        Just value ->
            saveDevices <| encodeDevices value

        Nothing ->
            Cmd.none



-- ENCODE


encodeBranches : Branches -> E.Value
encodeBranches branches =
    E.dict Branch.idToString Branch.encode branches


encodeDevices : Devices -> E.Value
encodeDevices devices =
    E.dict Device.idToString Device.encode devices



-- DECODER


decodeDevices : D.Value -> Devices
decodeDevices encoded =
    let
        deviceDecoder : D.Decoder Devices
        deviceDecoder =
            D.dict Device.decoder
    in
    case D.decodeValue deviceDecoder encoded of
        Ok value ->
            value

        Err message ->
            Dict.empty


decodeBranches : D.Value -> Branches
decodeBranches encoded =
    let
        decoder : D.Decoder Branches
        decoder =
            D.dict Branch.decoder
    in
    case D.decodeValue decoder encoded of
        Ok branches ->
            branches

        Err message ->
            Dict.empty
