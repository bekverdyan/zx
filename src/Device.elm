module Device exposing (Device)

import Device.Counter as Counter
import Device.Setting as Setting


type alias Device =
    { id : Identifier
    , info : Info
    , counters : Counter.Counters
    , settings : Setting.Settings
    }


type alias Identifier =
    String


newIdentifier : Identifier
newIdentifier =
    -- FIXME should be sha1
    "foo"


type alias Info =
    ( Model, Version, SoftVersion )


type alias Model =
    String


type alias Version =
    String


type alias SoftVersion =
    String



-- TODO finish up Parameters
-- TODO creators and setters
-- TODO encoders and decoders
-- TODO view
