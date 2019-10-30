module Device.Setting exposing (Settings(..))


type Settings
    = Channels ( Actual, Defined )
    | Config Parameters



-- CHANNEL


type alias Actual =
    Int


type alias Defined =
    List Component


type Component
    = Component ( Index, List Ingredient )


type alias Index =
    Int


type Ingredient
    = Ingredient ( Resource, Setter )


type alias Setter =
    Int


type Resource
    = Resource ( Name, Unit )


type alias Unit =
    String


type alias Name =
    String



-- CONFIG


type alias Parameters =
    { setter1 : Int
    , setter2 : Int
    }
