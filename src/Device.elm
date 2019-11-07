module Device exposing (Device, decodeDevice, encodeDevice)

import Device.Counter as Counter
import Device.Setting as Setting
import Json.Decode as D
import Json.Encode as E


type alias Device =
    { id : Identifier
    , name : Name
    , info : Info
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



--CREATE


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


newDevice : Device
newDevice =
    { id = newIdentifier
    , name = newName
    , info = infoOf newModel newVersion newSoftVersion
    , counters = Counter.newCounters []
    , settings = Setting.newConfig
    }



--ENCODE


encodeDevice : Device -> E.Value
encodeDevice device =
    E.object
        [ ( "id", E.string device.id )
        , ( "name", E.string device.name )
        , ( "info", encodeInfo device.info )
        , ( "counters"
          , E.list Counter.encodeCounter device.counters
          )
        , ( "settings", Setting.encodeSettings device.settings )
        ]


encodeInfo : Info -> E.Value
encodeInfo ( model, version, softVersion ) =
    E.object
        [ ( "model", E.string model )
        , ( "version", E.string version )
        , ( "softVersion", E.string softVersion )
        ]



--DECODE


decodeDevice : D.Decoder Device
decodeDevice =
    D.map5 Device
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "info" decodeInfo)
        (D.field "counters" <|
            D.list Counter.decodeCounter
        )
        (D.field "settings" Setting.decodeSettings)


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
