module Device exposing (Device, DeviceType(..), Devices, Identifier, Msg, createShortcut, decoder, encode, idToString, newDevice, update)

import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Device.Counter as Counter
import Device.Setting as Setting
import Device.Shortcut as DeviceShortcut
import Json.Decode as D
import Json.Encode as E
import Random
import Random.Char as RandomChar
import Random.String as RandomString


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



-- UPDATE


type Msg
    = GenerateDevice String


update : Msg -> Maybe Device -> BranchShortcut.Shortcut -> ( Device, Maybe Msg )
update msg device branch =
    case msg of
        GenerateDevice salt ->
            ( newDevice Washbox salt branch, Nothing )



-- MAP


idToString : Identifier -> String
idToString id =
    id



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



-- TODO mappers
-- TODO view
