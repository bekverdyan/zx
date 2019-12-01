module Device.Setting exposing (Settings, decoder, encode, newChannels, newConfig)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


decodeHopperFaze : Maybe Int -> Hopper
decodeHopperFaze value =
    case value of
        Just 0 ->
            Hopper Disabled

        Just 1 ->
            Hopper CcTalk

        Just 2 ->
            Hopper Pulse

        Just _ ->
            Hopper NoFaze

        Nothing ->
            Hopper NoFaze


decodeHopper : D.Decoder Hopper
decodeHopper =
    let
        maybeValue =
            D.field "hopper" <| D.maybe D.int
    in
    D.map decodeHopperFaze maybeValue



-- D.map newHopper (D.field "hopper" D.nullable)


decodeHopperModeFaze : Maybe Int -> HopperMode
decodeHopperModeFaze value =
    case value of
        Just 0 ->
            HopperMode Mode_1

        Just 1 ->
            HopperMode Mode_2

        Just _ ->
            HopperMode NoFaze

        Nothing ->
            HopperMode NoFaze


decodeHopperMode : D.Decoder HopperMode
decodeHopperMode =
    D.map decodeHopperModeFaze <|
        D.field "hopperMode" <|
            D.maybe D.int


decodeBillValidatorFaze : Maybe Int -> BillValidator
decodeBillValidatorFaze value =
    case value of
        Just 0 ->
            BillValidator Disabled

        Just 1 ->
            BillValidator CcTalk

        Just _ ->
            BillValidator NoFaze

        Nothing ->
            BillValidator NoFaze


decodeBillValidator : D.Decoder BillValidator
decodeBillValidator =
    D.map decodeBillValidatorFaze <|
        D.field "billValidator" <|
            D.maybe D.int


decodeRfidReader1Faze : Maybe Int -> RfidReader1
decodeRfidReader1Faze value =
    case value of
        Just 0 ->
            RfidReader1 Disabled

        Just 1 ->
            RfidReader1 Enabled

        Just _ ->
            RfidReader1 NoFaze

        Nothing ->
            RfidReader1 NoFaze


decodeRfidReader1 : D.Decoder RfidReader1
decodeRfidReader1 =
    D.map decodeRfidReader1Faze <|
        D.field "rfidReader1" <|
            D.maybe D.int


decodeRfidReader2Faze : Maybe Int -> RfidReader2
decodeRfidReader2Faze value =
    case value of
        Just 0 ->
            RfidReader2 Disabled

        Just 1 ->
            RfidReader2 Enabled

        Just _ ->
            RfidReader2 NoFaze

        Nothing ->
            RfidReader2 NoFaze


decodeRfidReader2 : D.Decoder RfidReader2
decodeRfidReader2 =
    D.map decodeRfidReader2Faze <|
        D.field "rfidReader2" <|
            D.maybe D.int


decodeDispenserFaze : Maybe Int -> Dispenser
decodeDispenserFaze value =
    case value of
        Just 0 ->
            Dispenser Disabled

        Just 1 ->
            Dispenser CRT_531

        Just 2 ->
            Dispenser TCD_820M

        Just _ ->
            Dispenser NoFaze

        Nothing ->
            Dispenser NoFaze


decodeDispenser : D.Decoder Dispenser
decodeDispenser =
    D.map decodeDispenserFaze <|
        D.field "dispenser" <|
            D.maybe D.int


decodeCardOutFaze : Maybe Int -> CardOut
decodeCardOutFaze value =
    case value of
        Just 0 ->
            CardOut ToGate

        Just 1 ->
            CardOut FullOut

        Just _ ->
            CardOut NoFaze

        Nothing ->
            CardOut NoFaze


decodeCardOut : D.Decoder CardOut
decodeCardOut =
    D.map decodeCardOutFaze <|
        D.field "cardOut" <|
            D.maybe D.int


decodeNetworkFaze : Maybe Int -> Network
decodeNetworkFaze value =
    case value of
        Just 0 ->
            Network None

        Just 1 ->
            Network RS_485

        Just 2 ->
            Network Can

        Just 3 ->
            Network Ethernet

        Just 4 ->
            Network WiFi

        Just _ ->
            Network NoFaze

        Nothing ->
            Network NoFaze


decodeNetwork : D.Decoder Network
decodeNetwork =
    D.map decodeNetworkFaze <|
        D.field "network" <|
            D.maybe D.int


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
        , ( "bonusThreshold", E.int variables.bonusThreshold )
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
    case settings of
        Channels ( actual, defined ) ->
            encodeChannels ( actual, defined )

        Config parameters ->
            encodeParameters parameters



-- VIEW


view : Settings -> Html msg
view settings =
    case settings of
        Channels ( actual, defined ) ->
            text "actual and defined"

        Config parameters ->
            text "parameters"



-- TODO mappers
