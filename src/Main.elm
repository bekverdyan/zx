port module Main exposing (Model, init, main)

import Branch
import Branch.Shortcut as BranchShortcut
import Browser
import Crypto.Hash as Hash
import Debug
import Device
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Encode as E
import Random
import Random.Char as RandomChar
import Random.String as RandomString


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



-- port loadBranches : (E.Value -> msg) -> Sub msg
-- port loadDevices : (E.Value -> msg) -> Sub msg
-- MODEL


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type alias Model =
    { branches : Maybe Branches
    , devices : Maybe Devices
    }


type alias Branches =
    Dict Branch.Identifier Branch


type alias Branch =
    Branch.Branch


type alias Devices =
    Dict Device.Identifier Device


type alias Device =
    Device.Device


type alias Flags =
    { branches : D.Value
    , devices : D.Value
    }



-- FLAG


pullBranches : D.Value -> Maybe Branches
pullBranches value =
    handleBranchResult <|
        D.decodeValue decodeBranches value


handleBranchResult : Result D.Error Branches -> Maybe Branches
handleBranchResult result =
    case result of
        Ok branches ->
            Just branches

        Err _ ->
            Nothing


pullDevices : D.Value -> Maybe Devices
pullDevices value =
    handleDeviceResult <|
        D.decodeValue decodeDevices value


handleDeviceResult : Result D.Error Devices -> Maybe Devices
handleDeviceResult result =
    case result of
        Ok value ->
            Just value

        Err _ ->
            Nothing


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        (pullBranches flags.branches)
      <|
        pullDevices flags.devices
    , Cmd.none
    )



-- UPDATE


requestDeviceGeneration : Branch -> Cmd Msg
requestDeviceGeneration branch =
    let
        constant =
            Random.constant branch
    in
    let
        salt =
            RandomString.string 6 RandomChar.armenian
    in
    let
        random =
            Random.pair salt constant
    in
    Random.generate GenerateDevice <| random


requestBranchGeneration : Cmd Msg
requestBranchGeneration =
    Random.generate GenerateBranch <|
        RandomString.string 6 RandomChar.armenian


generateSha : String -> String
generateSha salt =
    Hash.sha512 salt


type alias BranchID =
    Branch.Identifier


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias DeviceShortcut =
    DeviceShortcut.Shortcut


type Msg
    = GenerateDevice ( String, Branch.Branch )
    | GenerateBranch String
    | NewDevice Branch.Branch
    | NewBranch
    | PushDeviceToBranch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDevice branch ->
            ( model, requestDeviceGeneration branch )

        NewBranch ->
            ( model, requestBranchGeneration )

        GenerateDevice ( salt, branch ) ->
            let
                branchShortcut =
                    Branch.createShortcut branch

                device =
                    Device.newDevice
                        Device.Exchange
                        salt
                        branchShortcut

                deviceShortcut =
                    Device.createShortcut device

                updatedShortcuts =
                    Dict.insert
                        deviceShortcut.id
                        deviceShortcut
                        branch.shortcuts

                updatedBranch =
                    { branch | shortcuts = updatedShortcuts }

                updatedBranches =
                    case model.branches of
                        Just value ->
                            Dict.insert branch.id branch value

                        Nothing ->
                            Dict.singleton branch.id branch

                updatedDevices =
                    case model.devices of
                        Just value ->
                            Dict.insert device.id device value

                        Nothing ->
                            Dict.singleton device.id device
            in
            ( { model
                | devices = Just updatedDevices
                , branches = Just updatedBranches
              }
            , Cmd.batch
                [ saveDevices <|
                    encodeDevices updatedDevices
                ]
            )

        GenerateBranch salt ->
            let
                branch =
                    Branch.newBranch "եղո" salt
            in
            let
                branches =
                    case model.branches of
                        Just value ->
                            Dict.insert
                                branch.id
                                branch
                                value

                        Nothing ->
                            Dict.singleton
                                branch.id
                                branch
            in
            ( { model | branches = Just branches }
            , saveBranches <| encodeBranches branches
            )

        PushDeviceToBranch ->
            ( model, Cmd.none )



-- ENCODE


encodeBranches : Branches -> E.Value
encodeBranches branches =
    E.dict Branch.idToString Branch.encode branches


encodeDevices : Devices -> E.Value
encodeDevices devices =
    E.dict Device.idToString Device.encode devices



-- DECODER


decodeBranches : D.Decoder Branches
decodeBranches =
    D.field "branches" <| D.dict Branch.decoder


decodeDevices : D.Decoder Devices
decodeDevices =
    D.field "devices" <| D.dict Device.decoder



-- MAP


addBranch : Branch -> Branches -> Branches
addBranch branch branches =
    Dict.insert branch.id branch branches


removeBranch : Branch.Identifier -> Branches -> Branches
removeBranch id branches =
    Dict.remove id branches



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Դատարկ մարդ"
    , body = [ text "դիվայս" ]
    }
