module DeviceManager exposing (Identifier, newIdentifier)


type alias Identifier =
    String


newIdentifier : Identifier
newIdentifier =
    -- FIXME should be sha1
    "foo"
