module Device exposing (Device, DeviceType(..), decoder, encode, newDevice)

import Branch.Shortcut as BranchShortcut
import Device.Counter as Counter
import Device.Setting as Setting
import Device.Shortcut as DeviceShortcut
import Json.Decode as D
import Json.Encode as E


type alias Device =
    { id : Identifier
    , name : Name
    , info : Info
    , branch : BranchShortcut.Shortcut
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



--CREATE


createShortcut : Device -> ( Identifier, DeviceShortcut.Shortcut )
createShortcut device =
    ( device.id
    , { name = device.name }
    )


newIdentifier : Identifier
newIdentifier =
    -- FIXME should be sha1
    "foo"


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


newDevice : DeviceType -> Device
newDevice deviceType =
    let
        settings =
            case deviceType of
                Washbox ->
                    Setting.newChannels 0

                Exchange ->
                    Setting.newConfig
    in
    { id = newIdentifier
    , name = newName
    , info = infoOf newModel newVersion newSoftVersion
    , branch =
        { id = newIdentifier
        , name = newName
        }
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
