module Device.Setting exposing (Settings(..), decodeSettings)

import Json.Decode as D
import Json.Encode as E



-- MODEL


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
    { variables : Variables
    , switches : Switches
    }


type alias Variables =
    { coinNominal : Int
    , hopperCoinNominal : Int
    , billNominal : List Int
    , cardPrice : Int
    , deviceId : String
    , serverCode : String
    , bonusPercent : Int
    , bonusThreshold : Int
    }


type alias Switches =
    { hopper : Hopper
    , hopperMode : HopperMode
    , billValidator : BillValidator
    , rfidReader1 : RfidReader1
    , rfidReader2 : RfidReader2
    , dispenser : Dispenser
    , cardOut : CardOut
    , network : Network
    }


type Hopper
    = Hopper Faze


type HopperMode
    = HopperMode Faze


type BillValidator
    = BillValidator Faze


type RfidReader1
    = RfidReader1 Faze


type RfidReader2
    = RfidReader2 Faze


type Dispenser
    = Dispenser Faze


type CardOut
    = CardOut Faze


type Network
    = Network Faze


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
    | NoFaze



--DECODE


newChannels : Actual -> Defined -> Settings
newChannels actual defined =
    Channels ( actual, defined )


decodeSettings : D.Decoder Settings
decodeSettings =
    D.oneOf [ decodeChannels, decodeConfig ]


decodeChannels : D.Decoder Settings
decodeChannels =
    D.map2 newChannels
        (D.field "actual" D.int)
        (D.field "defined" <| D.list decodeComponent)


newComponent : Int -> List Ingredient -> Component
newComponent index ingredients =
    Component ( index, ingredients )


decodeComponent : D.Decoder Component
decodeComponent =
    D.map2 newComponent
        (D.field "index" D.int)
        (D.field "ingredients" <| D.list decodeIngredient)


newIngredient : Resource -> Portion -> Ingredient
newIngredient resource portion =
    Ingredient ( resource, portion )


newResource : Name -> Unit -> Resource
newResource name unit =
    Resource ( name, unit )


decodeIngredient : D.Decoder Ingredient
decodeIngredient =
    D.map2 newIngredient
        (D.field "resource" decodeResource)
        (D.field "portion" D.int)


decodeResource : D.Decoder Resource
decodeResource =
    D.map2 newResource
        (D.field "name" D.string)
        (D.field "unit" D.string)


newConfig : Parameters -> Settings
newConfig parameters =
    Config parameters


decodeConfig : D.Decoder Settings
decodeConfig =
    D.map newConfig
        (D.field "parameters" decodeParameters)


decodeParameters : D.Decoder Parameters
decodeParameters =
    D.map2 Parameters
        (D.field "variables" decodeVariables)
        (D.field "switches" decodeSwitches)


decodeVariables : D.Decoder Variables
decodeVariables =
    D.map8 Variables
        (D.field "coinNominal" D.int)
        (D.field "hopperCoinNominal" D.int)
        (D.field "billNominal" <| D.list D.int)
        (D.field "cardPrice" D.int)
        (D.field "deviceId" D.string)
        (D.field "serverCode" D.string)
        (D.field "bonusPercent" D.int)
        (D.field "bonusThreshold" D.int)


newHopper : Int -> Hopper
newHopper value =
    case value of
        0 ->
            Hopper Disabled

        1 ->
            Hopper CcTalk

        2 ->
            Hopper Pulse

        _ ->
            Hopper NoFaze


decodeHopper : D.Decoder Hopper
decodeHopper =
    D.map newHopper (D.field "hopper" D.int)


newHopperMode : Int -> HopperMode
newHopperMode value =
    case value of
        0 ->
            HopperMode Mode_1

        1 ->
            HopperMode Mode_2

        _ ->
            HopperMode NoFaze


decodeHopperMode : D.Decoder HopperMode
decodeHopperMode =
    D.map newHopperMode (D.field "hopperMode" D.int)


newBillValidator : Int -> BillValidator
newBillValidator value =
    case value of
        0 ->
            BillValidator Disabled

        1 ->
            BillValidator CcTalk

        _ ->
            BillValidator NoFaze


decodeBillValidator : D.Decoder BillValidator
decodeBillValidator =
    D.map newBillValidator (D.field "billValidator" D.int)


newRfidReader1 : Int -> RfidReader1
newRfidReader1 value =
    case value of
        0 ->
            RfidReader1 Disabled

        1 ->
            RfidReader1 Enabled

        _ ->
            RfidReader1 NoFaze


decodeRfidReader1 : D.Decoder RfidReader1
decodeRfidReader1 =
    D.map newRfidReader1 (D.field "rfidReader1" D.int)


newRfidReader2 : Int -> RfidReader2
newRfidReader2 value =
    case value of
        0 ->
            RfidReader2 Disabled

        1 ->
            RfidReader2 Enabled

        _ ->
            RfidReader2 NoFaze


decodeRfidReader2 : D.Decoder RfidReader2
decodeRfidReader2 =
    D.map newRfidReader2 (D.field "rfidReader2" D.int)


newDispenser : Int -> Dispenser
newDispenser value =
    case value of
        0 ->
            Dispenser Disabled

        1 ->
            Dispenser CRT_531

        2 ->
            Dispenser TCD_820M

        _ ->
            Dispenser NoFaze


decodeDispenser : D.Decoder Dispenser
decodeDispenser =
    D.map newDispenser (D.field "dispenser" D.int)


newCardOut : Int -> CardOut
newCardOut value =
    case value of
        0 ->
            CardOut ToGate

        1 ->
            CardOut FullOut

        _ ->
            CardOut NoFaze


decodeCardOut : D.Decoder CardOut
decodeCardOut =
    D.map newCardOut (D.field "cardOut" D.int)


newNetwork : Int -> Network
newNetwork value =
    case value of
        0 ->
            Network None

        1 ->
            Network RS_485

        2 ->
            Network Can

        3 ->
            Network Ethernet

        4 ->
            Network WiFi

        _ ->
            Network NoFaze


decodeNetwork : D.Decoder Network
decodeNetwork =
    D.map newNetwork (D.field "network" D.int)


decodeSwitches : D.Decoder Switches
decodeSwitches =
    D.map8 Switches
        decodeHopper
        decodeHopperMode
        decodeBillValidator
        decodeRfidReader1
        decodeRfidReader2
        decodeDispenser
        decodeCardOut
        decodeNetwork



-- ENCODE
-- encodeHopper : Hopper -> E.Value
-- encodeHopper hopper =
--   let
--       value = fazeToNumber
--   in
--   case fazeToNumber =
-- TODO finish me..


encodeSwitches : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> E.Value
encodeSwitches hopper hopperMode billValidator rfidReader1 rfidReader2 dispenser cardOut network =
    E.object
        [ ( "hopper", E.int hopper )
        , ( "hommerMode", E.int hopperMode )
        , ( "billValidator", E.int billValidator )
        , ( "rfidReader1", E.int rfidReader1 )
        , ( "rfidReader2", E.int rfidReader2 )
        , ( "dispenser", E.int dispenser )
        , ( "cardOut", E.int cardOut )
        , ( "network", E.int network )
        ]


numberOfHopperFaze : Hopper -> Maybe Int
numberOfHopperFaze hopper =
    case hopper of
        Hopper Disabled ->
            Just 0

        Hopper CcTalk ->
            Just 1

        Hopper Pulse ->
            Just 2

        _ ->
            Nothing


numberOfHopperModeFaze : HopperMode -> Maybe Int
numberOfHopperModeFaze hopperMode =
    case hopperMode of
        HopperMode Mode_1 ->
            Just 0

        HopperMode Mode_2 ->
            Just 1

        _ ->
            Nothing


numberOfBillValidatorFaze : BillValidator -> Maybe Int
numberOfBillValidatorFaze billValidator =
    case billValidator of
        BillValidator Disabled ->
            Just 0

        BillValidator CcTalk ->
            Just 1

        _ ->
            Nothing


numberOfRfidReader1Faze : RfidReader1 -> Maybe Int
numberOfRfidReader1Faze reader =
    case reader of
        RfidReader1 Disabled ->
            Just 0

        RfidReader1 Enabled ->
            Just 1

        _ ->
            Nothing


numberOfRfidReader2Faze : RfidReader2 -> Maybe Int
numberOfRfidReader2Faze reader =
    case reader of
        RfidReader2 Disabled ->
            Just 0

        RfidReader2 Enabled ->
            Just 1

        _ ->
            Nothing


numberOfDispenserFaze : Dispenser -> Maybe Int
numberOfDispenserFaze dispenser =
    case dispenser of
        Dispenser Disabled ->
            Just 0

        Dispenser CRT_531 ->
            Just 1

        Dispenser TCD_820M ->
            Just 2

        _ ->
            Nothing


numberOfCardOutFaze : CardOut -> Maybe Int
numberOfCardOutFaze cardOut =
    case cardOut of
        CardOut ToGate ->
            Just 0

        CardOut FullOut ->
            Just 1

        _ ->
            Nothing


numberOfNetworkFaze : Network -> Maybe Int
numberOfNetworkFaze network =
    case network of
        Network None ->
            Just 0

        Network RS_485 ->
            Just 1

        Network Can ->
            Just 2

        Network Ethernet ->
            Just 3

        Network WiFi ->
            Just 4

        _ ->
            Nothing


fazeToNumber : Faze -> Maybe Int
fazeToNumber faze =
    case faze of
        Enabled ->
            Just 1

        Disabled ->
            Just 0

        CcTalk ->
            Just 1

        Pulse ->
            Just 2

        Mode_1 ->
            Just 0

        Mode_2 ->
            Just 1

        CRT_531 ->
            Just 1

        TCD_820M ->
            Just 2

        ToGate ->
            Just 0

        FullOut ->
            Just 1

        None ->
            Just 0

        RS_485 ->
            Just 1

        Can ->
            Just 2

        Ethernet ->
            Just 3

        WiFi ->
            Just 4

        NoFaze ->
            Nothing
