port module Main exposing (Model, init, main)

import Branch
import Branch.Shortcut as BranchShortcut
import Browser
import Device
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
        , subscriptions = subscriptions
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


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias DeviceShortcut =
    DeviceShortcut.Shortcut


type alias Flags =
    { branches : D.Value
    , devices : D.Value
    }



-- SUBSCRIBE


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadDevices PullDevices
        , loadBranches PullBranches
        ]



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


type Msg
    = GenerateDevice ( String, Branch )
    | GenerateBranch String
    | NewDevice Branch
    | NewBranch
    | PullDevices E.Value
    | PullBranches E.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDevice branch ->
            ( model, requestDeviceGeneration branch )

        NewBranch ->
            ( model, requestBranchGeneration )

        GenerateDevice ( salt, branch ) ->
            let
                ( devices, branches ) =
                    handleDeviceGeneration salt branch model
            in
            ( { model
                | devices = Just devices
                , branches = Just branches
              }
            , Cmd.batch
                [ saveDevices <|
                    encodeDevices devices
                , saveBranches <|
                    encodeBranches branches
                ]
            )

        GenerateBranch salt ->
            let
                updatedBranches =
                    handleBranchGeneration salt model.branches
            in
            ( { model
                | branches = Just updatedBranches
              }
            , saveBranches <| encodeBranches updatedBranches
            )

        PullDevices devices ->
            ( { model
                | devices =
                    handleDeviceResult <|
                        D.decodeValue decodeDevices devices
              }
            , Cmd.none
            )

        PullBranches branches ->
            ( { model
                | branches =
                    handleBranchResult <|
                        D.decodeValue decodeBranches branches
              }
            , Cmd.none
            )



-- HANDLER


handleDeviceGeneration : String -> Branch -> Model -> ( Devices, Branches )
handleDeviceGeneration salt branch model =
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
    ( updatedDevices, updatedBranches )


handleBranchGeneration : String -> Maybe Branches -> Branches
handleBranchGeneration salt branches =
    let
        branch =
            Branch.newBranch "եղո" salt
    in
    case branches of
        Just value ->
            Dict.insert
                branch.id
                branch
                value

        Nothing ->
            Dict.singleton
                branch.id
                branch



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
    D.dict Branch.decoder


decodeDevices : D.Decoder Devices
decodeDevices =
    D.dict Device.decoder



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
    , body = [ viewDashboard model.branches ]
    }


viewDashboard : Maybe Branches -> Html Msg
viewDashboard branches =
    div []
        [ viewBranches branches
        , button [ onClick NewBranch ] [ text "Create Branch" ]
        ]


viewBranches : Maybe Branches -> Html Msg
viewBranches branches =
    case branches of
        Just value ->
            let
                branchesList =
                    Dict.values value
            in
            ul [ id "myUL" ] <|
                List.map Branch.view branchesList

        Nothing ->
            text "Ops !!! You have no branches yet"
