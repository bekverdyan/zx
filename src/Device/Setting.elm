module Device.Setting exposing (Settings, decoder, encode, newChannels, newConfig)

import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as EncodeExtra



-- MODEL


type Settings
    = Channels ( Actual, Defined )
    | Config Parameters



-- CHANNEL


type alias Actual =
    Int


type alias Defined =
    List Channel


type alias Channel =
    ( Index, List Ingredient )


type alias Index =
    Int


type alias Ingredient =
    ( Resource, Portion )


type alias Portion =
    Int


type alias Resource =
    ( Name, Unit )


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



--CREATE


newChannels : Actual -> Settings
newChannels actual =
    Channels ( actual, [] )


newVariables : Variables
newVariables =
    { coinNominal = 0
    , hopperCoinNominal = 0
    , billNominal = []
    , cardPrice = 0
    , deviceId = ""
    , serverCode = ""
    , bonusPercent = 0
    , bonusThreshold = 0
    }


newSwitches : Switches
newSwitches =
    { hopper = Hopper NoFaze
    , hopperMode = HopperMode NoFaze
    , billValidator = BillValidator NoFaze
    , rfidReader1 = RfidReader1 NoFaze
    , rfidReader2 = RfidReader2 NoFaze
    , dispenser = Dispenser NoFaze
    , cardOut = CardOut NoFaze
    , network = Network NoFaze
    }


newParameters : Parameters
newParameters =
    { variables = newVariables
    , switches = newSwitches
    }


newConfig : Settings
newConfig =
    configOf newParameters



--DECODE


channelsOf : Actual -> Defined -> Settings
channelsOf actual defined =
    Channels ( actual, defined )


decoder : D.Decoder Settings
decoder =
    D.oneOf [ decodeChannels, decodeConfig ]


decodeChannels : D.Decoder Settings
decodeChannels =
    D.map2 channelsOf
        (D.field "actual" D.int)
        (D.field "defined" <| D.list decodeComponent)


componentOf : Int -> List Ingredient -> Channel
componentOf index ingredients =
    ( index, ingredients )


decodeComponent : D.Decoder Channel
decodeComponent =
    D.map2 componentOf
        (D.field "index" D.int)
        (D.field "ingredients" <| D.list decodeIngredient)


newIngredient : Resource -> Portion -> Ingredient
newIngredient resource portion =
    ( resource, portion )


newResource : Name -> Unit -> Resource
newResource name unit =
    ( name, unit )


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


configOf : Parameters -> Settings
configOf parameters =
    Config parameters


decodeConfig : D.Decoder Settings
decodeConfig =
    D.map configOf
        decodeParameters


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


encodeVariables : Variables -> E.Value
encodeVariables variables =
    E.object
        [ ( "coinNominal", E.int variables.coinNominal )
        , ( "hopperCoinNominal"
          , E.int variables.hopperCoinNominal
          )
        , ( "billNominal", E.list E.int variables.billNominal )
        , ( "cardPrice", E.int variables.cardPrice )
        , ( "deviceId", E.string variables.deviceId )
        , ( "serverCode", E.string variables.serverCode )
        , ( "bonusPercent", E.int variables.bonusPercent )
        ]


encodeHopperFaze : Hopper -> E.Value
encodeHopperFaze hopper =
    case hopper of
        Hopper Disabled ->
            E.int 0

        Hopper CcTalk ->
            E.int 1

        Hopper Pulse ->
            E.int 2

        _ ->
            E.null


encodeHopperModeFaze : HopperMode -> E.Value
encodeHopperModeFaze hopperMode =
    case hopperMode of
        HopperMode Mode_1 ->
            E.int 0

        HopperMode Mode_2 ->
            E.int 1

        _ ->
            E.null


encodeBillValidatorFaze : BillValidator -> E.Value
encodeBillValidatorFaze billValidator =
    case billValidator of
        BillValidator Disabled ->
            E.int 0

        BillValidator CcTalk ->
            E.int 1

        _ ->
            E.null


encodeRfidReader1Faze : RfidReader1 -> E.Value
encodeRfidReader1Faze reader =
    case reader of
        RfidReader1 Disabled ->
            E.int 0

        RfidReader1 Enabled ->
            E.int 1

        _ ->
            E.null


encodeRfidReader2Faze : RfidReader2 -> E.Value
encodeRfidReader2Faze reader =
    case reader of
        RfidReader2 Disabled ->
            E.int 0

        RfidReader2 Enabled ->
            E.int 1

        _ ->
            E.null


encodeDispenserFaze : Dispenser -> E.Value
encodeDispenserFaze dispenser =
    case dispenser of
        Dispenser Disabled ->
            E.int 0

        Dispenser CRT_531 ->
            E.int 1

        Dispenser TCD_820M ->
            E.int 2

        _ ->
            E.null


encodeCardOutFaze : CardOut -> E.Value
encodeCardOutFaze cardOut =
    case cardOut of
        CardOut ToGate ->
            E.int 0

        CardOut FullOut ->
            E.int 1

        _ ->
            E.null


encodeNetworkFaze : Network -> E.Value
encodeNetworkFaze network =
    case network of
        Network None ->
            E.int 0

        Network RS_485 ->
            E.int 1

        Network Can ->
            E.int 2

        Network Ethernet ->
            E.int 3

        Network WiFi ->
            E.int 4

        _ ->
            E.null


encodeSwitches : Switches -> E.Value
encodeSwitches switches =
    E.object
        [ ( "hopper", encodeHopperFaze switches.hopper )
        , ( "hopperMode"
          , encodeHopperModeFaze switches.hopperMode
          )
        , ( "billValidator"
          , encodeBillValidatorFaze switches.billValidator
          )
        , ( "rfidReader1"
          , encodeRfidReader1Faze switches.rfidReader1
          )
        , ( "rfidReader2"
          , encodeRfidReader2Faze switches.rfidReader2
          )
        , ( "dispenser", encodeDispenserFaze switches.dispenser )
        , ( "cardOut", encodeCardOutFaze switches.cardOut )
        , ( "network", encodeNetworkFaze switches.network )
        ]


encodeParameters : Parameters -> E.Value
encodeParameters parameters =
    E.object
        [ ( "variables", encodeVariables parameters.variables )
        , ( "switches", encodeSwitches parameters.switches )
        ]


encodeResource : Resource -> E.Value
encodeResource resource =
    E.object
        [ ( "name", E.string <| Tuple.first resource ) ]


encodeIngredient : Ingredient -> E.Value
encodeIngredient ingredient =
    E.object
        [ ( "resource", encodeResource <| Tuple.first ingredient )
        , ( "portion", E.int <| Tuple.second ingredient )
        ]


encodeChannel : Channel -> E.Value
encodeChannel channel =
    E.object
        [ ( String.fromInt <| Tuple.first channel
          , E.list encodeIngredient <| Tuple.second channel
          )
        ]


encodeDefined : Defined -> E.Value
encodeDefined defined =
    E.list encodeChannel defined


encodeChannels : ( Actual, Defined ) -> E.Value
encodeChannels channels =
    E.object
        [ ( "actual", E.int <| Tuple.first channels )
        , ( "defined", encodeDefined <| Tuple.second channels )
        ]


encode : Settings -> E.Value
encode settings =
    let
        value =
            case settings of
                Channels ( actual, defined ) ->
                    encodeChannels ( actual, defined )

                Config parameters ->
                    encodeParameters parameters
    in
    value
