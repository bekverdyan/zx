module DeviceManager.Device exposing (Device)

import DeviceManager as DM


type alias Device =
    { id : DM.Identifier
    , info : Info
    , counters : List Counter
    , channels : Setting
    }


type alias Info =
    String


type Counter
    = Counter ( Name, Pointer )


type alias Name =
    String


type alias Pointer =
    Int


type Setting
    = Channels (List Ingredient)
    | Config Parameters


type alias Parameters =
    { setter1 : Int
    , setter2 : Int
    }


type Ingredient
    = Ingredient (List Component)


type Component
    = Component ( Resource, Setter )


type alias Setter =
    Int


type Resource
    = Resource ( Name, Unit )


type alias Unit =
    String



-- TODO encoders and decoders
-- TODO finish up Parameters
-- TODO creators and setters
-- TODO view
