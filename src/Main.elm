port module Main exposing (Model, init, main)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
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


decodeBranches : D.Value -> ( Maybe Branches, Dashboard.Model )
decodeBranches encoded =
    let
        decoder : D.Decoder Branches
        decoder =
            D.dict Branch.decoder
    in
    case D.decodeValue decoder encoded of
        Ok branches ->
            ( Just branches, Dashboard.Branches branches )

        Err message ->
            -- let
            --     log =
            --         Debug.log
            --             "There is no branches in LocalStorage"
            --             message
            -- in
            ( Nothing, Dashboard.Empty )


decodeDevices : D.Value -> Maybe Devices
decodeDevices encoded =
    let
        deviceDecoder : D.Decoder Devices
        deviceDecoder =
            D.dict Device.decoder
    in
    case D.decodeValue deviceDecoder encoded of
        Ok value ->
            Just value

        Err message ->
            -- let
            --     log =
            --         Debug.log
            --             "There is no Devices in LocalStorage"
            --             message
            -- in
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



-- type alias Flags =
--     { branches : D.Value
--     , devices : D.Value
--     }
-- SUBSCRIBE


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ loadDevices PullDevices
        , loadBranches PullBranches
        ]



-- FLAG


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Nothing
        Nothing
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

                newModel =
                    addDevice device container model
            in
            ( newModel
            , Cmd.batch
                [ pushDevices newModel.devices
                , pushBranches newModel.branches
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
                | devices = decodeDevices encoded
              }
            , Cmd.none
            )

        PullBranches encoded ->
            let
                ( branches, dashboard ) =
                    decodeBranches encoded
            in
            ( { model
                | branches = branches
                , dashboard = dashboard
              }
            , Cmd.none
            )

        EditorMsg editorMsg ->
            case editorMsg of
                Editor.BranchMsg branchMsg ->
                    case branchMsg of
                        Branch.NewDevice container ->
                            ( model, requestDeviceGeneration container )

                        _ ->
                            let
                                ( editorModel, saveMe ) =
                                    Editor.update editorMsg model.editor

                                ( newModel, cmdMsg ) =
                                    if saveMe == True then
                                        saveStuff editorModel model

                                    else
                                        ( model, Cmd.none )
                            in
                            ( { newModel | editor = editorModel }, cmdMsg )

                Editor.DeviceMsg deviceMsg ->
                    case deviceMsg of
                        Device.GoToContainer id ->
                            let
                                editor =
                                    case model.branches of
                                        Just branches ->
                                            case Dict.get id branches of
                                                Just branch ->
                                                    Editor.Branch
                                                        { branch = branch
                                                        , mode = Branch.Normal
                                                        }

                                                Nothing ->
                                                    Editor.NotFound

                                        Nothing ->
                                            Editor.NotFound
                            in
                            ( { model | editor = editor }, Cmd.none )

                        _ ->
                            let
                                ( editorModel, saveMe ) =
                                    Editor.update editorMsg model.editor

                                ( newModel, cmdMsg ) =
                                    if saveMe == True then
                                        saveStuff editorModel model

                                    else
                                        ( model, Cmd.none )
                            in
                            ( { newModel | editor = editorModel }, cmdMsg )

        DashboardMsg dashboardMsg ->
            case dashboardMsg of
                Dashboard.NewBranch ->
                    ( model, requestBranchGeneration )

                Dashboard.SelectBranch id ->
                    let
                        editor =
                            case model.branches of
                                Just branches ->
                                    case Dict.get id branches of
                                        Just branch ->
                                            Editor.Branch
                                                { branch = branch
                                                , mode = Branch.Normal
                                                }

                                        Nothing ->
                                            Editor.NotFound

                                Nothing ->
                                    Editor.NotFound
                    in
                    ( { model | editor = editor }, Cmd.none )

                Dashboard.SelectDevice id ->
                    let
                        editor =
                            case model.devices of
                                Just devices ->
                                    case Dict.get id devices of
                                        Just device ->
                                            Editor.Device <|
                                                Device.init
                                                    device

                                        Nothing ->
                                            Editor.NotFound

                                Nothing ->
                                    Editor.NotFound
                    in
                    ( { model | editor = editor }, Cmd.none )


mapDevices : String -> Device -> BranchShortcut.Shortcut -> Device
mapDevices deviceId device branchShortcut =
    if device.branch.id == branchShortcut.id then
        { device | branch = branchShortcut }

    else
        device


updateBranchRefs : Branch -> Maybe Devices -> Maybe Devices
updateBranchRefs branch value =
    let
        shortcut =
            Branch.createShortcut branch

        updateShortcuts : String -> Device -> Device
        updateShortcuts id device =
            mapDevices id device shortcut
    in
    case value of
        Just devices ->
            Just <|
                Dict.map
                    updateShortcuts
                    devices

        Nothing ->
            Nothing


updateDeviceRef : Device -> Maybe Branches -> Maybe Branches
updateDeviceRef device values =
    let
        shortcut =
            Device.createShortcut device
    in
    case values of
        Just branches ->
            case
                Dict.get
                    device.branch.id
                    branches
            of
                Just branch ->
                    let
                        updated =
                            { branch
                                | shortcuts =
                                    Dict.insert
                                        shortcut.id
                                        shortcut
                                        branch.shortcuts
                            }
                    in
                    Just <|
                        Dict.insert updated.id updated branches

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


saveStuff : Editor.Model -> Model -> ( Model, Cmd Msg )
saveStuff stuff model =
    case stuff of
        Editor.Branch viewModel ->
            case model.branches of
                Just branches ->
                    let
                        updated =
                            Dict.insert
                                viewModel.branch.id
                                viewModel.branch
                                branches

                        devices =
                            updateBranchRefs
                                viewModel.branch
                                model.devices
                    in
                    ( { model
                        | branches = Just updated
                        , devices = devices
                        , dashboard = Dashboard.Branches updated
                      }
                    , Cmd.batch
                        [ pushBranches <| Just updated
                        , pushDevices devices
                        ]
                    )

                Nothing ->
                    let
                        new =
                            Dict.singleton
                                viewModel.branch.id
                                viewModel.branch
                    in
                    ( { model
                        | branches = Just new
                        , dashboard = Dashboard.Branches new
                      }
                    , pushBranches <| Just new
                    )

        Editor.Device viewModel ->
            case model.devices of
                Just devices ->
                    let
                        updatedDevices =
                            Just <|
                                Dict.insert
                                    viewModel.device.id
                                    viewModel.device
                                    devices

                        updatedBranches =
                            updateDeviceRef
                                viewModel.device
                                model.branches

                        dashboard =
                            case updatedBranches of
                                Just values ->
                                    Dashboard.Branches values

                                Nothing ->
                                    Dashboard.Empty
                    in
                    ( { model
                        | devices = updatedDevices
                        , branches = updatedBranches
                        , dashboard = dashboard
                      }
                    , Cmd.batch
                        [ pushDevices <| updatedDevices
                        , pushBranches <| updatedBranches
                        ]
                    )

                Nothing ->
                    let
                        new =
                            Just <|
                                Dict.singleton
                                    viewModel.device.id
                                    viewModel.device

                        updatedBranches =
                            updateDeviceRef
                                viewModel.device
                                model.branches

                        dashboard =
                            case updatedBranches of
                                Just values ->
                                    Dashboard.Branches values

                                Nothing ->
                                    Dashboard.Empty
                    in
                    ( { model
                        | devices = new
                        , branches = updatedBranches
                        , dashboard = dashboard
                      }
                    , Cmd.batch
                        [ pushDevices <| new
                        , pushBranches <| updatedBranches
                        ]
                    )

        _ ->
            ( model, Cmd.none )


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
            Editor.Device <| Device.init value

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
                    Dict.insert device.id device devices

                Nothing ->
                    Dict.singleton device.id device
    in
    { model
        | devices = Just updatedDevices
        , branches = Just updatedBranches
        , editor = Editor.Device <| Device.init device
        , dashboard = Dashboard.Branches updatedBranches
    }


handleBranchGeneration : String -> Maybe Branches -> Branches
handleBranchGeneration salt dashboard =
    let
        branch =
            Branch.newBranch salt
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
        [ Grid.container []
            [ Grid.row []
                [ Grid.col [ Col.xs12, Col.mdAuto ]
                    [ Html.map DashboardMsg <|
                        Dashboard.view model.dashboard
                    ]
                , Grid.col []
                    [ Html.map EditorMsg <|
                        Editor.view model.editor
                    ]
                ]
            ]
        ]
    }
