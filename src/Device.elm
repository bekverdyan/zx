module Device exposing (Device, encodeDevice)

import Device.Counter as Counter
import Device.Setting as Setting
import Json.Decode as D
import Json.Encode as E


type alias Device =
    { id : Identifier
    , name : Name
    , info : Info
    , counters : Counters
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


type alias Counters =
    List Counter.Counter



--CREATOR


newIdentifier : Identifier
newIdentifier =
    -- FIXME should be sha1
    "foo"


newDevice : Identifier -> Name -> Info -> Counters -> Setting.Settings -> Device
newDevice id name info counters settings =
    { id = id
    , name = name
    , info = info
    , counters = counters
    , settings = settings
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
    D.map5 newDevice
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "info" decodeInfo)
        (D.field "counters" <|
            D.list Counter.decodeCounter
        )
        (D.field "settings" Setting.decodeSettings)


newInfo : Model -> Version -> SoftVersion -> Info
newInfo model version softVersion =
    ( model, version, softVersion )


decodeInfo : D.Decoder Info
decodeInfo =
    D.map3 newInfo
        (D.field "model" D.string)
        (D.field "version" D.string)
        (D.field "softVersion" D.string)



-- TODO encoders and decoders
-- TODO creators and setters
-- TODO view
