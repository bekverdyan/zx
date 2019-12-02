module Device exposing (Device, DeviceType(..), Devices, Identifier, createShortcut, decoder, encode, idToString, newDevice, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Device.Counter as Counter
import Device.Setting as Setting
import Device.Shortcut as DeviceShortcut
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias ViewModel =
    { device : Device
    , tabState : Tab.State
    }


type alias Devices =
    List Device


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias DeviceShortcut =
    DeviceShortcut.Shortcut


type alias Device =
    { id : Identifier
    , name : Name
    , info : Info
    , branch : BranchShortcut
    , counters : Counter.Counters
    , settings : Setting.Settings
    }


type alias Identifier =
    String


type alias Name =
    String


type alias Info =
    ( Model, Version, SoftVersion )


type alias Model =
    String


type alias Version =
    String


type alias SoftVersion =
    String


type DeviceType
    = Washbox
    | Exchange


type Msg
    = TabMsg Tab.State



-- MAP


idToString : Identifier -> String
idToString id =
    id



-- INIT


init : Device -> ViewModel
init device =
    ViewModel device Tab.initialState



--CREATE


createShortcut : Device -> DeviceShortcut
createShortcut device =
    { id = device.id
    , name = device.name
    }


newIdentifier : String -> Identifier
newIdentifier salt =
    Hash.sha512_224 salt


newName : Name
newName =
    ""


newModel : Model
newModel =
    ""


newVersion : Version
newVersion =
    ""


newSoftVersion : SoftVersion
newSoftVersion =
    ""


newDevice : DeviceType -> String -> BranchShortcut.Shortcut -> Device
newDevice deviceType salt branch =
    let
        settings =
            case deviceType of
                Washbox ->
                    Setting.newChannels 0

                Exchange ->
                    Setting.newConfig
    in
    { id = newIdentifier salt
    , name = newName
    , info = infoOf newModel newVersion newSoftVersion
    , branch = branch
    , counters = Counter.newCounters []
    , settings = settings
    }



--ENCODE


encode : Device -> E.Value
encode device =
    E.object
        [ ( "id", E.string device.id )
        , ( "name", E.string device.name )
        , ( "info", encodeInfo device.info )
        , BranchShortcut.encode device.branch
        , ( "counters", Counter.encode device.counters )
        , ( "settings", Setting.encode device.settings )
        ]


encodeInfo : Info -> E.Value
encodeInfo ( model, version, softVersion ) =
    E.object
        [ ( "model", E.string model )
        , ( "version", E.string version )
        , ( "softVersion", E.string softVersion )
        ]



--DECODE


decoder : D.Decoder Device
decoder =
    D.map6 Device
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "info" decodeInfo)
        BranchShortcut.decoder
        (D.field "counters" Counter.decoder)
        (D.field "settings" Setting.decoder)


infoOf : Model -> Version -> SoftVersion -> Info
infoOf model version softVersion =
    ( model, version, softVersion )


decodeInfo : D.Decoder Info
decodeInfo =
    D.map3 infoOf
        (D.field "model" D.string)
        (D.field "version" D.string)
        (D.field "softVersion" D.string)



-- TODO view


view : Device -> Html msg
view device =
    Card.config []
        |> Card.header [ class "text-center" ]
            [ h3 [ Spacing.mt2 ] [ text device.id ]
            ]
        |> Card.block []
            [ Block.titleH4 [] [ viewInfo device.info ]
            , Block.text []
                [ viewTabs device.counters device.settings ]
            , Block.custom <|
                Button.button [ Button.primary ]
                    [ text "Go somewhere" ]
            ]
        |> Card.view


viewInfo : Info -> Html msg
viewInfo ( model, version, softVersion ) =
    Card.config []
        |> Card.listGroup
            [ ListGroup.li [ ListGroup.success ]
                [ text <| "Model: " ++ model ]
            , ListGroup.li [ ListGroup.info ]
                [ text <| "Version: " ++ version ]
            , ListGroup.li [ ListGroup.warning ]
                [ text <| "Soft Version: " ++ softVersion ]
            ]
        |> Card.view



-- TODO View counters and settings in Tabs


viewTabs : Counter.Counters -> Setting.Settings -> Html msg
viewTabs counters settings =
    div []
        [ label [] [ text "counters below: " ]
        , Counter.view counters
        , label [] [ text "settings below: " ]
        , Setting.view settings
        ]



-- TODO mappers
