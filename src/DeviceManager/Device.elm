module DeviceManager.Device exposing (Device)

import DeviceManager as DM


type alias Device =
    { id : DM.Identifier
    , info : Info
    , counters : List Counter
    , settings : Settings
    }


type alias Info =
    String


type Counter
    = Counter ( Name, Pointer )


type alias Name =
    String


type alias Pointer =
    ( Daily, Total )


type alias Daily =
    Int


type alias Total =
    Int


type Settings
    = Channels ( Actual, Defined )
    | Config Parameters


type alias Parameters =
    { setter1 : Int
    , setter2 : Int
    }


type alias Actual =
    Int


type alias Defined =
    List Ingredient


type Ingredient
    = Ingredient ( Index, List Component )


type alias Index =
    Int


type Component
    = Component ( Resource, Setter )


type alias Setter =
    Int


type Resource
    = Resource ( Name, Unit )


type alias Unit =
    String



-- TODO finish up Parameters
-- TODO creators and setters
-- TODO encoders and decoders
-- TODO view
