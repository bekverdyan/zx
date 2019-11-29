port module Main exposing (Model, init, main)

-- import Bootstrap.ListGroup as ListGroup

import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Branch
import Branch.Shortcut as BranchShortcut
import Browser
import Debug
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
    { dashboard : Data
    , devices : Data
    , editor : Editor
    }


type Editor
    = NotSelected
    | BranchView Branch
    | DeviceView Device
    | NotFound


type Data
    = LoadedBranches Branches
    | LoadedDevices Devices
    | FirstBranch Branches
    | FirstDevice Devices
    | Loading
    | Error


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


pullBranches : D.Value -> Data
pullBranches value =
    handleBranchResult <|
        D.decodeValue decodeBranches value


handleBranchResult : Result D.Error Branches -> Data
handleBranchResult result =
    case result of
        Ok branches ->
            LoadedBranches branches

        Err _ ->
            Error


pullDevices : D.Value -> Data
pullDevices value =
    handleDeviceResult <|
        D.decodeValue decodeDevices value


handleDeviceResult : Result D.Error Devices -> Data
handleDeviceResult result =
    case result of
        Ok value ->
            LoadedDevices value

        Err message ->
            let
                log =
                    Debug.log "ERRORS: " message
            in
            Error


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        (pullBranches flags.branches)
        (pullDevices flags.devices)
        NotSelected
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
    | OpenDevice DeviceShortcut.Identifier
    | OpenBranch Branch.Identifier


pushBranches : Data -> Cmd Msg
pushBranches dashboard =
    case dashboard of
        LoadedBranches branches ->
            saveBranches <| encodeBranches branches

        FirstBranch branches ->
            saveBranches <| encodeBranches branches

        _ ->
            -- TODO handle possible errors
            Cmd.none


pushDevices : Data -> Cmd Msg
pushDevices devices =
    case devices of
        LoadedDevices value ->
            saveDevices <| encodeDevices value

        FirstDevice value ->
            saveDevices <| encodeDevices value

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDevice branch ->
            ( model, requestDeviceGeneration branch )

        NewBranch ->
            ( model, requestBranchGeneration )

        GenerateDevice ( salt, branch ) ->
            let
                ( devices, dashboard ) =
                    handleDeviceGeneration salt branch ( model.devices, model.dashboard )
            in
            ( { model
                | devices = devices
                , dashboard = dashboard
              }
            , Cmd.batch
                [ pushDevices devices
                , pushBranches dashboard
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

        OpenDevice deviceId ->
            let
                device =
                    case model.devices of
                        LoadedDevices values ->
                            Dict.get deviceId values

                        FirstDevice values ->
                            Dict.get deviceId values

                        _ ->
                            -- TODO handle possible cases
                            Nothing

                editor =
                    case device of
                        Just value ->
                            DeviceView value

                        Nothing ->
                            let
                                gag =
                                    Debug.log "device not found"
                            in
                            NotFound
            in
            ( { model | editor = editor }
            , Cmd.none
            )

        OpenBranch branchId ->
            let
                branch =
                    case model.dashboard of
                        LoadedBranches branches ->
                            Dict.get branchId branches

                        FirstBranch branches ->
                            Dict.get branchId branches

                        -- TODO handle possible states
                        _ ->
                            Nothing

                editor =
                    case branch of
                        Just value ->
                            BranchView value

                        Nothing ->
                            NotFound
            in
            ( { model | editor = editor }
            , Cmd.none
            )



-- HANDLER


handleDeviceGeneration : String -> Branch -> ( Data, Data ) -> ( Data, Data )
handleDeviceGeneration salt branch ( devices, dashboard ) =
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
            case dashboard of
                LoadedBranches branches ->
                    LoadedBranches <|
                        Dict.insert
                            updatedBranch.id
                            updatedBranch
                            branches

                FirstBranch branches ->
                    LoadedBranches <|
                        Dict.insert
                            updatedBranch.id
                            updatedBranch
                            branches

                _ ->
                    FirstBranch <|
                        Dict.singleton
                            updatedBranch.id
                            updatedBranch

        updatedDevices =
            case devices of
                LoadedDevices value ->
                    LoadedDevices <|
                        Dict.insert device.id device value

                FirstDevice value ->
                    LoadedDevices <|
                        Dict.insert
                            device.id
                            device
                            value

                _ ->
                    FirstDevice <|
                        Dict.singleton device.id device
    in
    ( updatedDevices, updatedBranches )


handleBranchGeneration : String -> Data -> Data
handleBranchGeneration salt dashboard =
    let
        branch =
            Branch.newBranch "եղո" salt
    in
    case dashboard of
        LoadedBranches value ->
            LoadedBranches <|
                Dict.insert
                    branch.id
                    branch
                    value

        _ ->
            FirstBranch <|
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
        [ viewDashboard model.dashboard
        , viewSelectedObject model.editor
        ]
    }


viewDashboard : Data -> Html Msg
viewDashboard dashboard =
    div []
        [ viewBranchesInDashboard dashboard
        , Button.button
            [ Button.dark
            , Button.attrs
                [ Spacing.ml1, onClick NewBranch ]
            ]
            [ text "Create Branch" ]
        ]


viewSelectedObject : Editor -> Html Msg
viewSelectedObject object =
    case object of
        NotSelected ->
            text "EMPTY VIEW"

        BranchView branch ->
            Branch.view
                (NewDevice branch)
                branch

        DeviceView device ->
            text device.id

        NotFound ->
            text "NOT FOUND"


viewDeviceShortcutWithMessage : DeviceShortcut -> Html Msg
viewDeviceShortcutWithMessage shortcut =
    DeviceShortcut.view (OpenDevice shortcut.id) shortcut


viewBranchInDashboardCmd : Branch -> Html Msg
viewBranchInDashboardCmd branch =
    Branch.viewInDashboard
        (OpenBranch branch.id)
        branch
        (ul [] <|
            List.map viewDeviceShortcutWithMessage <|
                Dict.values branch.shortcuts
        )


viewBranchesInDashboard : Data -> Html Msg
viewBranchesInDashboard dashboard =
    case dashboard of
        LoadedBranches branches ->
            ul [ class "tree" ] <|
                List.map
                    viewBranchInDashboardCmd
                    (Dict.values branches)

        FirstBranch branches ->
            ul [ class "tree" ] <|
                List.map
                    viewBranchInDashboardCmd
                    (Dict.values branches)

        Loading ->
            text "Please wait.."

        Error ->
            text "Error Loading branches"

        _ ->
            text "I don't know how to view this"
