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
    = Ingredient ( Resource, Portion )


type alias Portion =
    Int


type Resource
    = Resource ( Name, Unit )


type alias Unit =
    String


type alias Name =
    String



-- CONFIG


type alias Parameters =
    { coinNominal : Int
    , hopper : Faze
    , hopperCoinNominal : Int
    , hopperMode : Faze
    , billValidator : Faze
    , billNominal : List Int
    , rfidReader1 : Faze
    , rfidReader2 : Faze
    , dispenser : Faze
    , cardOut : Faze
    , cardPrice : Int
    , network : Faze
    , deviceId : String
    , serverCode : String
    , bonusPercent : Int
    , bonusThreshold : Int
    }


type Faze
    = Enabled
    | Disabled
    | CcTalk
    | Pulse
    | Mode_1
    | Mode_2
    | CRT_531
    | TCD_820M
    | ToGate
    | FullOut
    | None
    | RS_485
    | Can
    | Ethernet
    | WiFi
