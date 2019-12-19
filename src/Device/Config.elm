module Device.Config exposing
    ( Model
    , Msg
    , decoder
    , encode
    , newConfig
    , update
    , view
    )

import Bootstrap.Alert as Alert
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card as Card
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Model =
    { parameters : Parameters
    , mode : Mode
    }


type Mode
    = Normal
      -- VARIABLES
    | EditCoinNominal String
    | EditHopperCoinNominal String
    | EditCardPrice String
    | EditDeviceId String
      -- SWITCHES
    | EditHopper Faze
    | EditHopperMode Faze
    | EditBillValidator Faze
    | EditRfidReader1 Faze
    | EditRfidReader2 Faze


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
    { hopper : Faze
    , hopperMode : Faze
    , billValidator : Faze
    , rfidReader1 : Faze
    , rfidReader2 : Faze
    , dispenser : Faze
    , cardOut : Faze
    , network : Faze
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
    | NoFaze


fazeToString : Faze -> String
fazeToString faze =
    case faze of
        Enabled ->
            "Enabled"

        Disabled ->
            "Disabled"

        CcTalk ->
            "CcTalk"

        Pulse ->
            "Pulse"

        Mode_1 ->
            "Mode_1"

        Mode_2 ->
            "Mode_2"

        CRT_531 ->
            "CRT_531"

        TCD_820M ->
            "TCD_820M"

        ToGate ->
            "ToGate"

        FullOut ->
            "FullOut"

        None ->
            "None"

        RS_485 ->
            "RS_485"

        Can ->
            "Can"

        Ethernet ->
            "Ethernet"

        WiFi ->
            "WiFi"

        NoFaze ->
            "NoFaze"



--CREATE


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
    { hopper = NoFaze
    , hopperMode = NoFaze
    , billValidator = NoFaze
    , rfidReader1 = NoFaze
    , rfidReader2 = NoFaze
    , dispenser = NoFaze
    , cardOut = NoFaze
    , network = NoFaze
    }


newParameters : Parameters
newParameters =
    { variables = newVariables
    , switches = newSwitches
    }


newConfig : Model
newConfig =
    Model newParameters Normal



--DECODE


fromParameters : Variables -> Switches -> Model
fromParameters variables switches =
    Model (Parameters variables switches) Normal


decoder : D.Decoder Model
decoder =
    D.map2 fromParameters
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


decodeHopper : Maybe Int -> Faze
decodeHopper value =
    case value of
        Just 0 ->
            Disabled

        Just 1 ->
            CcTalk

        Just 2 ->
            Pulse

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeHopperMode : Maybe Int -> Faze
decodeHopperMode value =
    case value of
        Just 0 ->
            Mode_1

        Just 1 ->
            Mode_2

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeBillValidator : Maybe Int -> Faze
decodeBillValidator value =
    case value of
        Just 0 ->
            Disabled

        Just 1 ->
            CcTalk

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeRfidReader1 : Maybe Int -> Faze
decodeRfidReader1 value =
    case value of
        Just 0 ->
            Disabled

        Just 1 ->
            Enabled

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeRfidReader2 : Maybe Int -> Faze
decodeRfidReader2 value =
    case value of
        Just 0 ->
            Disabled

        Just 1 ->
            Enabled

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeDispenser : Maybe Int -> Faze
decodeDispenser value =
    case value of
        Just 0 ->
            Disabled

        Just 1 ->
            CRT_531

        Just 2 ->
            TCD_820M

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeCardOut : Maybe Int -> Faze
decodeCardOut value =
    case value of
        Just 0 ->
            ToGate

        Just 1 ->
            FullOut

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeNetwork : Maybe Int -> Faze
decodeNetwork value =
    case value of
        Just 0 ->
            None

        Just 1 ->
            RS_485

        Just 2 ->
            Can

        Just 3 ->
            Ethernet

        Just 4 ->
            WiFi

        Just _ ->
            NoFaze

        Nothing ->
            NoFaze


decodeSwitches : D.Decoder Switches
decodeSwitches =
    D.map8 Switches
        (D.map decodeHopper <|
            D.field "hopper" <|
                D.maybe D.int
        )
        (D.map decodeHopperMode <|
            D.field "hopperMode" <|
                D.maybe D.int
        )
        (D.map decodeBillValidator <|
            D.field "billValidator" <|
                D.maybe D.int
        )
        (D.map decodeRfidReader1 <|
            D.field "rfidReader1" <|
                D.maybe D.int
        )
        (D.map decodeRfidReader2 <|
            D.field "rfidReader2" <|
                D.maybe D.int
        )
        (D.map decodeDispenser <|
            D.field "dispenser" <|
                D.maybe D.int
        )
        (D.map decodeCardOut <|
            D.field "cardOut" <|
                D.maybe D.int
        )
        (D.map decodeNetwork <|
            D.field "network" <|
                D.maybe D.int
        )



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


encodeHopper : Faze -> ( String, E.Value )
encodeHopper hopper =
    case hopper of
        Disabled ->
            ( "hopper", E.int 0 )

        CcTalk ->
            ( "hopper", E.int 1 )

        Pulse ->
            ( "hopper", E.int 2 )

        _ ->
            ( "hopper", E.null )


encodeHopperMode : Faze -> ( String, E.Value )
encodeHopperMode hopperMode =
    case hopperMode of
        Mode_1 ->
            ( "hopperMode", E.int 0 )

        Mode_2 ->
            ( "hopperMode", E.int 1 )

        _ ->
            ( "hopperMode", E.null )


encodeBillValidator : Faze -> ( String, E.Value )
encodeBillValidator billValidator =
    case billValidator of
        Disabled ->
            ( "billValidator", E.int 0 )

        CcTalk ->
            ( "billValidator", E.int 1 )

        _ ->
            ( "billValidator", E.null )


encodeRfidReader1 : Faze -> ( String, E.Value )
encodeRfidReader1 reader =
    case reader of
        Disabled ->
            ( "rfidReader1", E.int 0 )

        Enabled ->
            ( "rfidReader1", E.int 1 )

        _ ->
            ( "rfidReader1", E.null )


encodeRfidReader2 : Faze -> ( String, E.Value )
encodeRfidReader2 reader =
    case reader of
        Disabled ->
            ( "rfidReader2", E.int 0 )

        Enabled ->
            ( "rfidReader2", E.int 1 )

        _ ->
            ( "rfidReader2", E.null )


encodeDispenser : Faze -> ( String, E.Value )
encodeDispenser dispenser =
    case dispenser of
        Disabled ->
            ( "dispenser", E.int 0 )

        CRT_531 ->
            ( "dispenser", E.int 1 )

        TCD_820M ->
            ( "dispenser", E.int 2 )

        _ ->
            ( "dispenser", E.null )


encodeCardOut : Faze -> ( String, E.Value )
encodeCardOut cardOut =
    case cardOut of
        ToGate ->
            ( "cardOut", E.int 0 )

        FullOut ->
            ( "cardOut", E.int 1 )

        _ ->
            ( "cardOut", E.null )


encodeNetwork : Faze -> ( String, E.Value )
encodeNetwork network =
    case network of
        None ->
            ( "network", E.int 0 )

        RS_485 ->
            ( "network", E.int 1 )

        Can ->
            ( "network", E.int 2 )

        Ethernet ->
            ( "network", E.int 3 )

        WiFi ->
            ( "network", E.int 4 )

        _ ->
            ( "network", E.null )


encodeSwitches : Switches -> E.Value
encodeSwitches switches =
    E.object
        [ encodeHopper switches.hopper
        , encodeHopperMode switches.hopperMode
        , encodeBillValidator switches.billValidator
        , encodeRfidReader1 switches.rfidReader1
        , encodeRfidReader2 switches.rfidReader2
        , encodeDispenser switches.dispenser
        , encodeCardOut switches.cardOut
        , encodeNetwork switches.network
        ]


encodeParameters : Parameters -> E.Value
encodeParameters parameters =
    E.object
        [ ( "variables", encodeVariables parameters.variables )
        , ( "switches", encodeSwitches parameters.switches )
        ]


encode : Model -> E.Value
encode model =
    encodeParameters model.parameters



-- UPDATE


type Msg
    = NormalMode
      -- VARIABLES
      --Coin nominal
    | EditCoinNominalMode
    | InputCoinNominal String
    | SaveCoinNominal String
      --Hopper coin nominal
    | EditHopperCoinNominalMode
    | InputHopperCoinNominal String
    | SaveHopperCoinNominal String
      --Card price
    | EditCardPriceMode
    | InputCardPrice String
    | SaveCardPrice String
      --Device ID
    | EditModeDeviceId
    | InputDeviceId String
    | SaveDeviceId String
      -- SWITCHES
      --Hopper
    | EditModeHopper
    | SetHopper Faze
      --Hopper mode
    | EditModeHopperMode
    | SetHopperMode Faze
      --Bill validator
    | EditModeBillValidator
    | SetBillValidator Faze
      --Rfid reader 1
    | EditModeRfidReader1
    | SetRfidReader1 Faze
      -- rfid reader 2
    | EditModeRfidReader2
    | SetRfidReader2 Faze


update : Msg -> Model -> ( Model, Bool )
update msg model =
    let
        parametersOrig =
            model.parameters

        variablesOrig =
            parametersOrig.variables

        switchesOrig =
            parametersOrig.switches
    in
    case msg of
        NormalMode ->
            ( { model | mode = Normal }, False )

        EditCoinNominalMode ->
            let
                coinNominal =
                    variablesOrig.coinNominal
            in
            ( { model
                | mode =
                    EditCoinNominal <|
                        String.fromInt coinNominal
              }
            , False
            )

        SaveCoinNominal nominal ->
            let
                parsed =
                    case String.toInt nominal of
                        Just value ->
                            value

                        Nothing ->
                            0

                variables =
                    { variablesOrig | coinNominal = parsed }
            in
            ( { model
                | parameters =
                    { parametersOrig
                        | variables = variables
                    }
                , mode = Normal
              }
            , True
            )

        InputCoinNominal nominal ->
            ( { model
                | mode = EditCoinNominal nominal
              }
            , False
            )

        EditHopperCoinNominalMode ->
            let
                hopperCoinNominal =
                    variablesOrig.hopperCoinNominal
            in
            ( { model
                | mode =
                    EditHopperCoinNominal <|
                        String.fromInt hopperCoinNominal
              }
            , False
            )

        SaveHopperCoinNominal nominal ->
            let
                parsed =
                    case String.toInt nominal of
                        Just value ->
                            value

                        Nothing ->
                            0

                variables =
                    { variablesOrig | hopperCoinNominal = parsed }
            in
            ( { model
                | parameters =
                    { parametersOrig
                        | variables = variables
                    }
                , mode = Normal
              }
            , True
            )

        InputHopperCoinNominal nominal ->
            ( { model
                | mode = EditHopperCoinNominal nominal
              }
            , False
            )

        EditCardPriceMode ->
            let
                cardPrice =
                    variablesOrig.cardPrice
            in
            ( { model
                | mode =
                    EditCardPrice <|
                        String.fromInt cardPrice
              }
            , False
            )

        SaveCardPrice price ->
            let
                parsed =
                    case String.toInt price of
                        Just value ->
                            value

                        Nothing ->
                            0

                variables =
                    { variablesOrig | cardPrice = parsed }
            in
            ( { model
                | parameters =
                    { parametersOrig
                        | variables = variables
                    }
                , mode = Normal
              }
            , True
            )

        InputCardPrice price ->
            ( { model
                | mode = EditCardPrice price
              }
            , False
            )

        SaveDeviceId id ->
            ( { model
                | parameters =
                    { parametersOrig
                        | variables =
                            { variablesOrig
                                | deviceId = id
                            }
                    }
                , mode = Normal
              }
            , True
            )

        InputDeviceId editable ->
            ( { model | mode = EditDeviceId editable }, False )

        EditModeDeviceId ->
            ( { model
                | mode =
                    EditDeviceId
                        variablesOrig.deviceId
              }
            , False
            )

        EditModeHopper ->
            let
                faze =
                    model.parameters.switches.hopper
            in
            ( { model | mode = EditHopper faze }, False )

        SetHopper faze ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | hopper = faze
                            }
                    }
                , mode = Normal
              }
            , True
            )

        EditModeHopperMode ->
            let
                faze =
                    model.parameters.switches.hopperMode
            in
            ( { model | mode = EditHopperMode faze }, False )

        SetHopperMode faze ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | hopperMode = faze
                            }
                    }
                , mode = Normal
              }
            , True
            )

        EditModeBillValidator ->
            let
                faze =
                    model.parameters.switches.billValidator
            in
            ( { model | mode = EditBillValidator faze }, False )

        SetBillValidator faze ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | billValidator = faze
                            }
                    }
                , mode = Normal
              }
            , True
            )

        EditModeRfidReader1 ->
            ( { model
                | mode =
                    EditRfidReader1
                        switchesOrig.rfidReader1
              }
            , False
            )

        SetRfidReader1 reader ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | rfidReader1 = reader
                            }
                    }
                , mode = Normal
              }
            , True
            )

        EditModeRfidReader2 ->
            ( { model
                | mode =
                    EditRfidReader2
                        switchesOrig.rfidReader2
              }
            , False
            )

        SetRfidReader2 reader ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | rfidReader2 = reader
                            }
                    }
                , mode = Normal
              }
            , True
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        variables =
            model.parameters.variables

        switches =
            model.parameters.switches
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Card.config []
                    |> Card.listGroup
                        [ viewCoinNominal
                            variables.coinNominal
                            model.mode
                        , viewHopperCoinNominal
                            variables.hopperCoinNominal
                            model.mode
                        , viewCardPrice
                            variables.cardPrice
                            model.mode
                        , viewDeviceId
                            variables.deviceId
                            model.mode
                        ]
                    |> Card.view
                ]
            , Grid.col []
                [ Card.config []
                    |> Card.listGroup
                        [ viewHopper
                            switches.hopper
                            model.mode
                        , viewHopperMode
                            switches.hopperMode
                            model.mode
                        , viewBillValidator
                            switches.billValidator
                            model.mode
                        , viewRfidReader1
                            switches.rfidReader1
                            model.mode
                        , viewRfidReader2
                            switches.rfidReader2
                            model.mode
                        ]
                    |> Card.view
                ]
            ]
        ]


viewCoinNominal : Int -> Mode -> ListGroup.Item Msg
viewCoinNominal nominal mode =
    case mode of
        EditCoinNominal editable ->
            viewCoinNominalEditMode editable

        _ ->
            viewCoinNominalNormalMode
                nominal


viewCoinNominalNormalMode : Int -> ListGroup.Item Msg
viewCoinNominalNormalMode nominal =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditCoinNominalMode
                ]
            ]
            [ h4 []
                [ text "Coin nominal: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <|
                        String.fromInt nominal
                    ]
                ]
            ]
        ]


viewCoinNominalEditMode : String -> ListGroup.Item Msg
viewCoinNominalEditMode editable =
    ListGroup.li [ ListGroup.warning ]
        [ InputGroup.config
            (InputGroup.text
                [ Input.id "coiNominalInput"
                , Input.attrs [ Spacing.mAuto ]
                , Input.onInput InputCoinNominal
                , Input.placeholder "Coin nominal"
                , Input.value editable
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button
                    [ Button.success
                    , Button.onClick <|
                        SaveCoinNominal editable
                    ]
                    [ text "Save" ]
                , InputGroup.button
                    [ Button.warning
                    , Button.onClick NormalMode
                    ]
                    [ text "Cancel" ]
                ]
            |> InputGroup.view
        ]


viewHopperCoinNominal : Int -> Mode -> ListGroup.Item Msg
viewHopperCoinNominal nominal mode =
    case mode of
        EditHopperCoinNominal editable ->
            viewHopperCoinNominalEditMode editable

        _ ->
            viewHopperCoinNominalNormalMode
                nominal


viewHopperCoinNominalNormalMode : Int -> ListGroup.Item Msg
viewHopperCoinNominalNormalMode nominal =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditHopperCoinNominalMode
                ]
            ]
            [ h4 []
                [ text <|
                    "Hopper coin nominal: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| String.fromInt nominal
                    ]
                ]
            ]
        ]


viewHopperCoinNominalEditMode : String -> ListGroup.Item Msg
viewHopperCoinNominalEditMode editable =
    ListGroup.li [ ListGroup.warning ]
        [ InputGroup.config
            (InputGroup.text
                [ Input.id "hopperCoiNominalInput"
                , Input.attrs [ Spacing.mAuto ]
                , Input.placeholder "Hopper coin nominal"
                , Input.onInput InputHopperCoinNominal
                , Input.value editable
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button
                    [ Button.success
                    , Button.onClick <|
                        SaveHopperCoinNominal editable
                    ]
                    [ text "Save" ]
                , InputGroup.button
                    [ Button.warning
                    , Button.onClick NormalMode
                    ]
                    [ text "Cancel" ]
                ]
            |> InputGroup.view
        ]


viewCardPrice : Int -> Mode -> ListGroup.Item Msg
viewCardPrice nominal mode =
    case mode of
        EditCardPrice editable ->
            viewCardPriceEditMode editable

        _ ->
            viewCardPriceNormalMode
                nominal


viewCardPriceNormalMode : Int -> ListGroup.Item Msg
viewCardPriceNormalMode price =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditCardPriceMode
                ]
            ]
            [ h4 []
                [ text <|
                    "Card price: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| String.fromInt price ]
                ]
            ]
        ]


viewCardPriceEditMode : String -> ListGroup.Item Msg
viewCardPriceEditMode editable =
    ListGroup.li [ ListGroup.warning ]
        [ InputGroup.config
            (InputGroup.text
                [ Input.id "cardPriceInput"
                , Input.attrs [ Spacing.mAuto ]
                , Input.placeholder "Card price"
                , Input.onInput InputCardPrice
                , Input.value editable
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button
                    [ Button.success
                    , Button.onClick <|
                        SaveCardPrice editable
                    ]
                    [ text "Save" ]
                , InputGroup.button
                    [ Button.warning
                    , Button.onClick NormalMode
                    ]
                    [ text "Cancel" ]
                ]
            |> InputGroup.view
        ]


viewDeviceId : String -> Mode -> ListGroup.Item Msg
viewDeviceId id mode =
    case mode of
        EditDeviceId editable ->
            viewDeviceIdEditMode editable

        _ ->
            viewDeviceIdNormalMode id


viewDeviceIdNormalMode : String -> ListGroup.Item Msg
viewDeviceIdNormalMode id =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditModeDeviceId
                ]
            ]
            [ h4 []
                [ text "Device ID: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text id ]
                ]
            ]
        ]


viewDeviceIdEditMode : String -> ListGroup.Item Msg
viewDeviceIdEditMode editable =
    ListGroup.li [ ListGroup.warning ]
        [ InputGroup.config
            (InputGroup.text
                [ Input.id "deviceIdInput"
                , Input.attrs [ Spacing.mAuto ]
                , Input.placeholder "Device ID"
                , Input.onInput InputDeviceId
                , Input.value editable
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button
                    [ Button.success
                    , Button.onClick <|
                        SaveDeviceId editable
                    ]
                    [ text "Save" ]
                , InputGroup.button
                    [ Button.warning
                    , Button.onClick NormalMode
                    ]
                    [ text "Cancel" ]
                ]
            |> InputGroup.view
        ]


viewHopper : Faze -> Mode -> ListGroup.Item Msg
viewHopper faze mode =
    case mode of
        EditHopper editable ->
            viewHopperEditMode editable

        _ ->
            viewHopperNormalMode faze


viewHopperNormalMode : Faze -> ListGroup.Item Msg
viewHopperNormalMode faze =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditModeHopper
                ]
            ]
            [ h4 []
                [ text <| "Hopper: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| fazeToString faze ]
                ]
            ]
        ]


viewHopperEditMode : Faze -> ListGroup.Item Msg
viewHopperEditMode faze =
    ListGroup.li [ ListGroup.warning ]
        [ ButtonGroup.radioButtonGroup []
            [ ButtonGroup.radioButton
                (faze == Disabled)
                [ Button.danger
                , Button.onClick <| SetHopper Disabled
                ]
                [ text "Disable" ]
            , ButtonGroup.radioButton
                (faze == CcTalk)
                [ Button.danger
                , Button.onClick <| SetHopper CcTalk
                ]
                [ text "CcTalk" ]
            , ButtonGroup.radioButton
                (faze == Pulse)
                [ Button.danger
                , Button.onClick <| SetHopper Pulse
                ]
                [ text "Pulse" ]
            ]
        ]


viewHopperMode : Faze -> Mode -> ListGroup.Item Msg
viewHopperMode faze mode =
    case mode of
        EditHopperMode editable ->
            viewHopperModeEditMode editable

        _ ->
            viewHopperModeNormalMode faze


viewHopperModeNormalMode : Faze -> ListGroup.Item Msg
viewHopperModeNormalMode faze =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditModeHopperMode
                ]
            ]
            [ h4 []
                [ text <|
                    "Hopper mode: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| fazeToString faze ]
                ]
            ]
        ]


viewHopperModeEditMode : Faze -> ListGroup.Item Msg
viewHopperModeEditMode faze =
    ListGroup.li [ ListGroup.warning ]
        [ ButtonGroup.radioButtonGroup []
            [ ButtonGroup.radioButton
                (faze == Mode_1)
                [ Button.danger
                , Button.onClick <| SetHopperMode Mode_1
                ]
                [ text "Mode 1" ]
            , ButtonGroup.radioButton
                (faze == Mode_2)
                [ Button.danger
                , Button.onClick <| SetHopperMode Mode_2
                ]
                [ text "Mode 2" ]
            ]
        ]


viewBillValidator : Faze -> Mode -> ListGroup.Item Msg
viewBillValidator faze mode =
    case mode of
        EditBillValidator editable ->
            viewBillValidatorEditMode editable

        _ ->
            viewBillValidatorNormalMode faze


viewBillValidatorNormalMode : Faze -> ListGroup.Item Msg
viewBillValidatorNormalMode faze =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditModeBillValidator
                ]
            ]
            [ h4 []
                [ text <|
                    "Bill validator: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| fazeToString faze ]
                ]
            ]
        ]


viewBillValidatorEditMode : Faze -> ListGroup.Item Msg
viewBillValidatorEditMode faze =
    ListGroup.li [ ListGroup.warning ]
        [ ButtonGroup.radioButtonGroup []
            [ ButtonGroup.radioButton
                (faze == Disabled)
                [ Button.danger
                , Button.onClick <| SetBillValidator Disabled
                ]
                [ text "Disable" ]
            , ButtonGroup.radioButton
                (faze == CcTalk)
                [ Button.danger
                , Button.onClick <| SetBillValidator CcTalk
                ]
                [ text "CcTalk" ]
            ]
        ]


viewRfidReader1 : Faze -> Mode -> ListGroup.Item Msg
viewRfidReader1 faze mode =
    case mode of
        EditRfidReader1 editable ->
            viewRfidReader1EditMode editable

        _ ->
            viewRfidReader1NormalMode faze


viewRfidReader1NormalMode : Faze -> ListGroup.Item Msg
viewRfidReader1NormalMode faze =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditModeRfidReader1
                ]
            ]
            [ h4 []
                [ text <|
                    "RfidReader1: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| fazeToString faze ]
                ]
            ]
        ]


viewRfidReader1EditMode : Faze -> ListGroup.Item Msg
viewRfidReader1EditMode faze =
    ListGroup.li [ ListGroup.warning ]
        [ ButtonGroup.radioButtonGroup []
            [ ButtonGroup.radioButton
                (faze == Disabled)
                [ Button.danger
                , Button.onClick <| SetRfidReader1 Disabled
                ]
                [ text "Disable" ]
            , ButtonGroup.radioButton
                (faze == Enabled)
                [ Button.danger
                , Button.onClick <| SetRfidReader1 Enabled
                ]
                [ text "Enable" ]
            ]
        ]


viewRfidReader2 : Faze -> Mode -> ListGroup.Item Msg
viewRfidReader2 faze mode =
    case mode of
        EditRfidReader2 editable ->
            viewRfidReader2EditMode editable

        _ ->
            viewRfidReader2NormalMode faze


viewRfidReader2NormalMode : Faze -> ListGroup.Item Msg
viewRfidReader2NormalMode faze =
    ListGroup.li [ ListGroup.info ]
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick EditModeRfidReader2
                ]
            ]
            [ h4 []
                [ text <|
                    "RfidReader2: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| fazeToString faze ]
                ]
            ]
        ]


viewRfidReader2EditMode : Faze -> ListGroup.Item Msg
viewRfidReader2EditMode faze =
    ListGroup.li [ ListGroup.warning ]
        [ ButtonGroup.radioButtonGroup []
            [ ButtonGroup.radioButton
                (faze == Disabled)
                [ Button.danger
                , Button.onClick <| SetRfidReader2 Disabled
                ]
                [ text "Disable" ]
            , ButtonGroup.radioButton
                (faze == Enabled)
                [ Button.danger
                , Button.onClick <| SetRfidReader2 Enabled
                ]
                [ text "Enable" ]
            ]
        ]



-- TODO mappers
