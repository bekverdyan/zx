port module Main exposing (Model, init, main)

import Branch
import Branch.Shortcut as BranchShortcut
import Browser
import Crypto.Hash as Hash
import Debug
import Device
import Device.Shortcut as DeviceShortcut
import Dict
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
    { branches : Maybe Branch.Branches
    , devices : Maybe Device.Devices
    }


type alias Flags =
    { branches : D.Value
    , devices : D.Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        (Branch.pullBranches flags.branches)
      <|
        Device.pullDevices flags.devices
    , Cmd.none
    )


type Msg
    = GenerateDevice ( String, BranchShortcut )
    | GenerateBranch String
    | NewDevice ( BranchID, Branch.Branch )
    | NewBranch



-- UPDATE


requestDeviceGeneration : BranchShortcut -> Cmd Msg
requestDeviceGeneration shortcut =
    let
        constant =
            Random.constant shortcut
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDevice branch ->
            let
                shortcut =
                    Branch.createShortcut branch
            in
            ( model, requestDeviceGeneration shortcut )

        NewBranch ->
            ( model, requestBranchGeneration )

        GenerateDevice ( salt, branchShortcut ) ->
            let
                device =
                    Device.newDevice Device.Exchange salt branchShortcut
            in
            let
                devices =
                    case model.devices of
                        Just value ->
                            device :: value

                        Nothing ->
                            List.singleton device
            in
            ( { model | devices = Just devices }
            , let
                shortcut =
                    Device.createShortcut device
              in
              -- TODO update and save branch shortcuts
              -- let
              --     branch =
              -- let
              --   shortcuts =
              --       shortcut :: branch.shortcuts
              -- in
              saveDevices <| Device.encodeDevices devices
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
                                (Tuple.first branch)
                                (Tuple.second branch)
                                value

                        Nothing ->
                            Dict.singleton
                                (Tuple.first branch)
                                (Tuple.second branch)
            in
            ( { model | branches = Just branches }
            , saveBranches <| Branch.encode branches
            )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Դատարկ մարդ"
    , body = [ text "դիվայս" ]
    }
