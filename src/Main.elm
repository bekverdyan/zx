port module Main exposing (Model, init, main)

import Bootstrap.Button as Button
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Branch
import Branch.Shortcut as BranchShortcut
import Browser
import Dashboard
import Debug
import Device
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Editor as Editor
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


pullBranches : D.Value -> Maybe Branches
pullBranches decoded =
    handleBranchResult <|
        D.decodeValue decodeBranches decoded


handleBranchResult : Result D.Error Branches -> Maybe Branches
handleBranchResult result =
    case result of
        Ok branches ->
            Just branches

        Err _ ->
            Nothing


pullDevices : D.Value -> Maybe Devices
pullDevices decoded =
    handleDeviceResult <|
        D.decodeValue decodeDevices decoded


handleDeviceResult : Result D.Error Devices -> Maybe Devices
handleDeviceResult result =
    case result of
        Ok value ->
            Just value

        Err message ->
            let
                log =
                    Debug.log "ERRORS: " message
            in
            Nothing


pushBranches : Maybe Branches -> Cmd Msg
pushBranches dashboard =
    case dashboard of
        Just branches ->
            saveBranches <| encodeBranches branches

        Nothing ->
            Cmd.none


pushDevices : Maybe Devices -> Cmd Msg
pushDevices devices =
    case devices of
        Just value ->
            saveDevices <| encodeDevices value

        Nothing ->
            Cmd.none



-- MODEL


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type alias Model =
    { branches : Maybe Branches
    , devices : Maybe Devices
    , dashboard : Dashboard.Model
    , editor : Editor.Model
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



-- SUBSCRIBE


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadDevices PullDevices
        , loadBranches PullBranches
        ]



-- FLAG


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        (pullBranches flags.branches)
        (pullDevices flags.devices)
        Dashboard.Empty
        Editor.NotSelected
    , Cmd.none
    )



-- REQUEST


requestDeviceGeneration : Branch -> Cmd Msg
requestDeviceGeneration branch =
    let
        constant =
            Random.constant branch

        salt =
            RandomString.string 6 RandomChar.armenian

        random =
            Random.pair salt constant
    in
    Random.generate GenerateDevice <| random


requestBranchGeneration : Cmd Msg
requestBranchGeneration =
    Random.generate GenerateBranch <|
        RandomString.string 6 RandomChar.armenian



-- UPDATE


type Msg
    = GenerateDevice ( String, Branch )
    | GenerateBranch String
    | PullDevices E.Value
    | PullBranches E.Value
    | EditorMsg Editor.Msg
    | DashboardMsg Dashboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateDevice ( salt, branch ) ->
            let
                ( device, container ) =
                    generateDevice salt branch
            in
            ( addDevice device container model
            , Cmd.batch
                [ pushDevices model.devices
                , pushBranches model.branches
                ]
            )

        GenerateBranch salt ->
            let
                branches =
                    handleBranchGeneration salt model.branches
            in
            ( { model
                | branches = Just branches
                , dashboard = Dashboard.Branches branches
              }
            , pushBranches <| Just branches
            )

        PullDevices encoded ->
            ( { model
                | devices = pullDevices encoded
              }
            , Cmd.none
            )

        PullBranches encoded ->
            let
                branches =
                    pullBranches encoded

                dashboard =
                    case branches of
                        Just value ->
                            Dashboard.Branches value

                        Nothing ->
                            Dashboard.Empty
            in
            ( { model
                | branches = branches
                , dashboard = dashboard
              }
            , Cmd.none
            )

        EditorMsg editorMsg ->
            case editorMsg of
                -- FIXME Newly generated device is not saved because incorrect
                -- message handling
                Editor.NewDevice container ->
                    ( model, requestDeviceGeneration container )

                _ ->
                    let
                        ( editor, command ) =
                            Editor.update editorMsg model.editor
                    in
                    ( { model | editor = editor }
                    , Cmd.map EditorMsg command
                    )

        DashboardMsg dashboardMsg ->
            case dashboardMsg of
                Dashboard.NewBranch ->
                    ( model, requestBranchGeneration )

                Dashboard.SelectBranch id ->
                    ( { model
                        | editor = openBranch id model.branches
                      }
                    , Cmd.none
                    )

                Dashboard.SelectDevice id ->
                    ( { model
                        | editor = openDevice id model.devices
                      }
                    , Cmd.none
                    )

                _ ->
                    let
                        ( dashboard, command ) =
                            Dashboard.update
                                dashboardMsg
                                model.dashboard
                    in
                    ( { model | dashboard = dashboard }
                    , Cmd.map DashboardMsg command
                    )


openBranch : Branch.Identifier -> Maybe Branches -> Editor.Model
openBranch id branches =
    let
        branch =
            case branches of
                Just value ->
                    Dict.get id value

                Nothing ->
                    Nothing
    in
    case branch of
        Just value ->
            Editor.Branch
                { branch = value
                , mode = Branch.Normal
                }

        Nothing ->
            Editor.NotFound


openDevice : Device.Identifier -> Maybe Devices -> Editor.Model
openDevice id devices =
    let
        device =
            case devices of
                Just value ->
                    Dict.get id value

                Nothing ->
                    Nothing
    in
    case device of
        Just value ->
            let
                deviceInEditor =
                    { device = value
                    , tabState = Tab.initialState
                    }
            in
            Editor.Device deviceInEditor

        Nothing ->
            Editor.NotFound



-- HANDLER


generateDevice : String -> Branch -> ( Device, Branch )
generateDevice salt branch =
    let
        device =
            Device.newDevice
                Device.Exchange
                salt
            <|
                Branch.createShortcut branch

        shortcut =
            Device.createShortcut device
    in
    ( device
    , { branch
        | shortcuts =
            Dict.insert
                shortcut.id
                shortcut
                branch.shortcuts
      }
    )


addDevice : Device -> Branch -> Model -> Model
addDevice device branch model =
    let
        updatedBranches =
            case model.branches of
                Just branches ->
                    Dict.insert
                        branch.id
                        branch
                        branches

                Nothing ->
                    Dict.singleton
                        branch.id
                        branch

        updatedDevices =
            case model.devices of
                Just devices ->
                    Just <|
                        Dict.insert device.id device devices

                Nothing ->
                    Just <|
                        Dict.singleton device.id device

        deviceInEditor =
            { device = device
            , tabState = Tab.initialState
            }
    in
    { model
        | devices = updatedDevices
        , branches = Just updatedBranches
        , editor = Editor.Device deviceInEditor
        , dashboard = Dashboard.Branches updatedBranches
    }


handleBranchGeneration : String -> Maybe Branches -> Branches
handleBranchGeneration salt dashboard =
    let
        branch =
            Branch.newBranch "եղո" salt
    in
    case dashboard of
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
    , body =
        [ Html.map DashboardMsg <|
            Dashboard.view model.dashboard
        , Html.map EditorMsg <|
            Editor.view model.editor
        ]
    }
