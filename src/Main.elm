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


pullBranches : D.Value -> Dashboard.Model
pullBranches value =
    handleBranchResult <|
        D.decodeValue decodeBranches value


handleBranchResult : Result D.Error Branches -> Dashboard.Model
handleBranchResult result =
    case result of
        Ok branches ->
            Dashboard.Branches branches

        Err _ ->
            Dashboard.Error


pullDevices : D.Value -> Maybe Devices
pullDevices value =
    handleDeviceResult <|
        D.decodeValue decodeDevices value


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


pushBranches : Dashboard.Model -> Cmd Msg
pushBranches dashboard =
    case dashboard of
        Dashboard.Branches branches ->
            saveBranches <| encodeBranches branches

        _ ->
            -- TODO handle possible errors
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
    { dashboard : Dashboard.Model
    , devices : Maybe Devices
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
        Editor.NotSelected
    , Cmd.none
    )



-- REQUEST


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
            ( handleDeviceGeneration salt branch model
            , Cmd.batch
                [ pushDevices model.devices
                , pushBranches model.dashboard
                ]
            )

        GenerateBranch salt ->
            let
                dashboard =
                    handleBranchGeneration salt model.dashboard
            in
            ( { model
                | dashboard = dashboard
              }
            , pushBranches dashboard
            )

        PullDevices encoded ->
            ( { model
                | devices = pullDevices encoded
              }
            , Cmd.none
            )

        PullBranches encoded ->
            ( { model
                | dashboard = pullBranches encoded
              }
            , Cmd.none
            )

        EditorMsg editorMsg ->
            case editorMsg of
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
                        | editor = openBranch id model.dashboard
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


openBranch : Branch.Identifier -> Dashboard.Model -> Editor.Model
openBranch id dashboard =
    let
        branch =
            case dashboard of
                Dashboard.Branches branches ->
                    Dict.get id branches

                Dashboard.Empty ->
                    Nothing

                Dashboard.Error ->
                    Nothing
    in
    case branch of
        Just value ->
            Editor.Branch value

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


handleDeviceGeneration : String -> Branch -> Model -> Model
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
            case model.dashboard of
                Dashboard.Branches branches ->
                    Dashboard.Branches <|
                        Dict.insert
                            updatedBranch.id
                            updatedBranch
                            branches

                _ ->
                    Dashboard.Branches <|
                        Dict.singleton
                            updatedBranch.id
                            updatedBranch

        updatedDevices =
            case model.devices of
                Just value ->
                    Just <|
                        Dict.insert device.id device value

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
        , dashboard = updatedBranches
        , editor = Editor.Device deviceInEditor
    }


handleBranchGeneration : String -> Dashboard.Model -> Dashboard.Model
handleBranchGeneration salt dashboard =
    let
        branch =
            Branch.newBranch "եղո" salt
    in
    case dashboard of
        Dashboard.Branches value ->
            Dashboard.Branches <|
                Dict.insert
                    branch.id
                    branch
                    value

        _ ->
            Dashboard.Branches <|
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
