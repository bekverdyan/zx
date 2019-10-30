module Device.Counter exposing (Counters)


type alias Counters =
    List Counter


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
