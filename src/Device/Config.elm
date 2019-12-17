module Device.Config exposing (Model, Msg, decoder, encode, newConfig, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
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
    | EditCoinNominal String


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
    | EditCoinNominalMode
    | SaveCoinNominal String
    | InputCoinNominal String
    | SetHopper Faze


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        NormalMode ->
            ( { model | mode = Normal }, False )

        EditCoinNominalMode ->
            let
                coinNominal =
                    model.parameters.variables.coinNominal
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
                parametersOrig =
                    model.parameters

                variablesOrig =
                    parametersOrig.variables

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

        SetHopper faze ->
            let
                switchesOrig =
                    model.parameters.switches

                switches =
                    { switchesOrig | hopper = faze }

                parametersOrig =
                    model.parameters
            in
            ( { model
                | parameters =
                    { parametersOrig
                        | switches = switches
                    }
              }
            , True
            )



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ viewVariables
                    model.parameters
                    model.mode
                ]

            -- , Grid.col [] [ text "2 of 2" ]
            ]
        ]


viewVariables : Parameters -> Mode -> Html Msg
viewVariables parameters mode =
    Table.simpleTable
        ( Table.simpleThead
            []
        , Table.tbody []
            [ Table.tr []
                [ Table.td []
                    [ case mode of
                        EditCoinNominal nominal ->
                            viewCoinNominalEditMode
                                nominal

                        _ ->
                            viewCoinNominalNormalMode
                                parameters.variables.coinNominal
                    ]
                ]
            , Table.tr []
                [ Table.td []
                    [ viewHopper
                        parameters.switches.hopper
                    ]
                ]

            -- , Table.tr []
            --     [ Table.td [] [ text "Dude" ]
            --     , Table.td [] [ text "Dude" ]
            --     , Table.td [] [ text "Dude" ]
            --     ]
            ]
        )


viewCoinNominalNormalMode : Int -> Html Msg
viewCoinNominalNormalMode coinNominal =
    div []
        [ Alert.simpleSecondary []
            [ text "Coin nominal: "
            , text <| String.fromInt coinNominal
            , Button.button
                [ Button.dark
                , Button.attrs
                    [ Spacing.ml1
                    , onClick EditCoinNominalMode
                    ]
                ]
                [ text "Edit" ]
            ]
        ]


viewCoinNominalEditMode : String -> Html Msg
viewCoinNominalEditMode editable =
    div []
        [ Alert.simpleWarning []
            [ text "Coin nominal"
            , InputGroup.config
                (InputGroup.text
                    [ Input.id "coiNominalInput"
                    , Input.onInput InputCoinNominal
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
        ]


viewHopper : Faze -> Html Msg
viewHopper faze =
    Alert.simpleSecondary []
        [ text "Hopper"
        , ButtonGroup.radioButtonGroup []
            [ ButtonGroup.radioButton
                (faze == Disabled)
                [ Button.primary
                , Button.onClick <| SetHopper Disabled
                ]
                [ text "Disabled" ]
            , ButtonGroup.radioButton
                (faze == CcTalk)
                [ Button.primary
                , Button.onClick <| SetHopper CcTalk
                ]
                [ text "CcTalk" ]
            , ButtonGroup.radioButton
                (faze == Pulse)
                [ Button.primary
                , Button.onClick <| SetHopper Pulse
                ]
                [ text "Pulse" ]
            ]
        ]



-- TODO mappers
