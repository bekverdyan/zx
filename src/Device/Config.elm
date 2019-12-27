module Device.Config exposing
    ( Model
    , Msg
    , decoder
    , encode
    , newConfig
    , update
    , view
    )

import Array as Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria as Aria
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
    | EditBillNominal Int
    | EditCardPrice String
    | EditDeviceId String
    | EditServerCode String
    | EditBonusPercent String
    | EditBonusThreshold String
      -- SWITCHES
    | EditHopper Faze
    | EditHopperMode Faze
    | EditBillValidator Faze
    | EditRfidReader1 Faze
    | EditRfidReader2 Faze
    | EditDispenser Faze
    | EditCardOut Faze
    | EditNetwork Faze


type alias Parameters =
    { variables : Variables
    , switches : Switches
    }


type alias Array =
    Array.Array Int


type alias Variables =
    { coinNominal : Int
    , hopperCoinNominal : Int
    , billNominal : Array
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
    , billNominal = Array.repeat 10 0
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
        (D.field "billNominal" <| D.array D.int)
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
        , ( "billNominal", E.array E.int variables.billNominal )
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
      --Bill nominal
    | EditModeBillNominal
    | InputBillNominal Int
    | SaveBillNominal Int
      --Card price
    | EditCardPriceMode
    | InputCardPrice String
    | SaveCardPrice String
      --Device ID
    | EditModeDeviceId
    | InputDeviceId String
    | SaveDeviceId String
      --Server code
    | EditModeServerCode
    | InputServerCode String
    | SaveServerCode String
      --Bonus percent
    | EditModeBonusPercent
    | InputBonusPercent String
    | SaveBonusPercent String
      --Bonus threshold
    | EditModeBonusThreshold
    | InputBonusThreshold String
    | SaveBonusThreshold String
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
      --Dispenser
    | EditModeDispenser
    | SetDispenser Faze
      --Card out
    | EditModeCardOut
    | SetCardOut Faze
      --Network
    | EditModeNetwork
    | SetNetwork Faze


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

        EditModeBillNominal ->
            ( { model | mode = EditBillNominal 0 }, False )

        InputBillNominal editable ->
            ( { model | mode = EditBillNominal editable }, False )

        SaveBillNominal nominal ->
            ( { model | mode = Normal }, True )

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

        EditModeServerCode ->
            ( { model
                | mode =
                    EditServerCode
                        variablesOrig.serverCode
              }
            , False
            )

        InputServerCode editable ->
            ( { model
                | mode = EditServerCode editable
              }
            , False
            )

        SaveServerCode code ->
            ( { model
                | parameters =
                    { parametersOrig
                        | variables =
                            { variablesOrig
                                | serverCode = code
                            }
                    }
                , mode = Normal
              }
            , True
            )

        EditModeBonusPercent ->
            ( { model
                | mode =
                    EditBonusPercent <|
                        String.fromInt
                            variablesOrig.bonusPercent
              }
            , False
            )

        SaveBonusPercent percent ->
            let
                parsed =
                    case String.toInt percent of
                        Just value ->
                            value

                        Nothing ->
                            0
            in
            ( { model
                | parameters =
                    { parametersOrig
                        | variables =
                            { variablesOrig
                                | bonusPercent = parsed
                            }
                    }
                , mode = Normal
              }
            , True
            )

        InputBonusPercent editable ->
            ( { model
                | mode = EditBonusPercent editable
              }
            , False
            )

        EditModeBonusThreshold ->
            ( { model
                | mode =
                    EditBonusThreshold <|
                        String.fromInt
                            variablesOrig.bonusThreshold
              }
            , False
            )

        SaveBonusThreshold threshold ->
            let
                parsed =
                    case String.toInt threshold of
                        Just value ->
                            value

                        Nothing ->
                            0
            in
            ( { model
                | parameters =
                    { parametersOrig
                        | variables =
                            { variablesOrig
                                | bonusThreshold = parsed
                            }
                    }
                , mode = Normal
              }
            , True
            )

        InputBonusThreshold editable ->
            ( { model
                | mode = EditBonusThreshold editable
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

        EditModeDispenser ->
            ( { model
                | mode =
                    EditDispenser
                        switchesOrig.dispenser
              }
            , False
            )

        SetDispenser dispenser ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | dispenser = dispenser
                            }
                    }
                , mode = Normal
              }
            , True
            )

        EditModeCardOut ->
            ( { model
                | mode =
                    EditCardOut
                        switchesOrig.cardOut
              }
            , False
            )

        SetCardOut cardOut ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | cardOut = cardOut
                            }
                    }
                , mode = Normal
              }
            , True
            )

        EditModeNetwork ->
            ( { model
                | mode =
                    EditNetwork
                        switchesOrig.network
              }
            , False
            )

        SetNetwork network ->
            ( { model
                | parameters =
                    { parametersOrig
                        | switches =
                            { switchesOrig
                                | network = network
                            }
                    }
                , mode = Normal
              }
            , True
            )


view : Model -> Html Msg
view model =
    let
        variables =
            model.parameters.variables

        switches =
            model.parameters.switches
    in
    div [ class "l-content" ]
        [ div [ class "pricing-tables pure-g" ]
            [ div [ class "pure-u-1 pure-u-md-1-2" ]
                [ div
                    [ class "pricing-table pricing-table-free" ]
                    [ div [ class "pricing-table-header" ] []
                    , viewVariables variables model.mode
                    ]
                ]
            , div [ class "pure-u-1 pure-u-md-1-2" ]
                [ div
                    [ class "pricing-table pricing-table-free" ]
                    [ div [ class "pricing-table-header" ] []
                    , viewSwitches switches model.mode
                    ]
                ]
            ]
        ]


viewVariables : Variables -> Mode -> Html Msg
viewVariables variables mode =
    ul [ class "pricing-table-list" ]
        [ viewCoinNominal variables.coinNominal mode
        , viewHopperCoinNominal variables.hopperCoinNominal mode
        , viewBillNominal variables.billNominal
        , viewCardPrice variables.cardPrice mode
        , viewDeviceId variables.deviceId mode
        , viewServerCode variables.serverCode mode
        , viewBonusPercent variables.bonusPercent mode
        , viewBonusThreshold variables.bonusThreshold mode
        ]


viewSwitches : Switches -> Mode -> Html Msg
viewSwitches switches mode =
    ul [ class "pricing-table-list" ]
        [ viewHopper switches.hopper mode
        , viewHopperMode switches.hopperMode mode
        , viewBillValidator switches.billValidator mode
        , viewRfidReader1 switches.rfidReader1 mode
        , viewRfidReader2 switches.rfidReader2 mode
        , viewDispenser switches.dispenser mode
        , viewCardOut switches.cardOut mode
        , viewNetwork switches.network mode
        ]


viewCoinNominal : Int -> Mode -> Html Msg
viewCoinNominal nominal mode =
    case mode of
        EditCoinNominal editable ->
            viewCoinNominalEditMode editable

        _ ->
            viewCoinNominalNormalMode
                nominal


viewCoinNominalNormalMode : Int -> Html Msg
viewCoinNominalNormalMode nominal =
    li []
        [ label [] [ text "Coin nominal: " ]
        , label []
            [ a [ onClick EditCoinNominalMode ]
                [ text <| String.fromInt nominal ]
            ]
        ]


viewCoinNominalEditMode : String -> Html Msg
viewCoinNominalEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "coinNominal"
                , placeholder "Coin nominal"
                , onInput InputCoinNominal
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SaveCoinNominal editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewHopperCoinNominal : Int -> Mode -> Html Msg
viewHopperCoinNominal nominal mode =
    case mode of
        EditHopperCoinNominal editable ->
            viewHopperCoinNominalEditMode editable

        _ ->
            viewHopperCoinNominalNormalMode
                nominal


viewHopperCoinNominalNormalMode : Int -> Html Msg
viewHopperCoinNominalNormalMode nominal =
    li []
        [ label [] [ text "Hoppen coin nominal: " ]
        , label []
            [ a [ onClick EditHopperCoinNominalMode ]
                [ text <| String.fromInt nominal ]
            ]
        ]


viewHopperCoinNominalEditMode : String -> Html Msg
viewHopperCoinNominalEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "hopperCoinNominal"
                , placeholder "Hopper coin nominal"
                , onInput InputHopperCoinNominal
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SaveHopperCoinNominal editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewBillNominal : Array -> Html Msg
viewBillNominal nominal =
    li []
        [ label [] [ text "Bill nominal: " ]
        , label []
            [ a [ onClick EditModeBillNominal ]
                [ text <|
                    String.fromInt <|
                        arrayToInt nominal
                ]
            ]
        ]



--
-- viewBillNominalEditMode : Array -> Html Msg
-- viewBillNominalEditMode editable =
--     li []
--         [ InputGroup.config
--             (InputGroup.text
--                 [ Input.id "hopperCoiNominalInput"
--                 , Input.attrs [ Spacing.mAuto ]
--                 , Input.placeholder "Hopper coin nominal"
--                 , Input.onInput InputBillNominal
--                 , Input.value editable
--                 ]
--             )
--             |> InputGroup.successors
--                 [ InputGroup.button
--                     [ Button.success
--                     , Button.onClick <|
--                         SaveBillNominal editable
--                     ]
--                     [ text "Save" ]
--                 , InputGroup.button
--                     [ Button.warning
--                     , Button.onClick NormalMode
--                     ]
--                     [ text "Cancel" ]
--                 ]
--             |> InputGroup.view
--         ]


viewEditableBillNominal : Array -> Html Msg
viewEditableBillNominal nominal =
    Html.form [ class "pure-form" ]
        [ fieldset []
            [ input
                [ type_ "number"
                , size 1
                , value <| viewNominalMember <| Array.get 0 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 1 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 2 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 3 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 6 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 5 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 6 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 7 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 8 nominal
                ]
                []
            , input
                [ type_ "text"
                , size 1
                , value <| viewNominalMember <| Array.get 9 nominal
                ]
                []
            ]
        ]


viewNominalMember : Maybe Int -> String
viewNominalMember member =
    case member of
        Just value ->
            String.fromInt value

        Nothing ->
            ""


viewCardPrice : Int -> Mode -> Html Msg
viewCardPrice nominal mode =
    case mode of
        EditCardPrice editable ->
            viewCardPriceEditMode editable

        _ ->
            viewCardPriceNormalMode
                nominal


viewCardPriceNormalMode : Int -> Html Msg
viewCardPriceNormalMode price =
    li []
        [ label [] [ text "Card price: " ]
        , label []
            [ a [ onClick EditCardPriceMode ]
                [ text <| String.fromInt price ]
            ]
        ]


viewCardPriceEditMode : String -> Html Msg
viewCardPriceEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "cardPrice"
                , placeholder "Card price"
                , onInput InputCardPrice
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SaveCardPrice editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewDeviceId : String -> Mode -> Html Msg
viewDeviceId id mode =
    case mode of
        EditDeviceId editable ->
            viewDeviceIdEditMode editable

        _ ->
            viewDeviceIdNormalMode id


viewDeviceIdNormalMode : String -> Html Msg
viewDeviceIdNormalMode id =
    li []
        [ label [] [ text "Device ID: " ]
        , label []
            [ a [ onClick EditModeDeviceId ]
                [ text <|
                    if String.isEmpty <| String.trim id then
                        "NotSet"

                    else
                        id
                ]
            ]
        ]


viewDeviceIdEditMode : String -> Html Msg
viewDeviceIdEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "deviceId"
                , placeholder "Device ID"
                , onInput InputDeviceId
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SaveDeviceId editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewServerCode : String -> Mode -> Html Msg
viewServerCode code mode =
    case mode of
        EditServerCode editable ->
            viewServerCodeEditMode editable

        _ ->
            viewServerCodeNormalMode code


viewServerCodeNormalMode : String -> Html Msg
viewServerCodeNormalMode code =
    li []
        [ label [] [ text "Server code: " ]
        , label []
            [ a [ onClick EditModeServerCode ]
                [ text <|
                    if String.isEmpty <| String.trim code then
                        "NotSet"

                    else
                        code
                ]
            ]
        ]


viewServerCodeEditMode : String -> Html Msg
viewServerCodeEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "serverCode"
                , placeholder "Server code"
                , onInput InputServerCode
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SaveServerCode editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewBonusPercent : Int -> Mode -> Html Msg
viewBonusPercent percent mode =
    case mode of
        EditBonusPercent editable ->
            viewBonusPercentEditMode editable

        _ ->
            viewBonusPercentNormalMode
                percent


viewBonusPercentNormalMode : Int -> Html Msg
viewBonusPercentNormalMode percent =
    li []
        [ label [] [ text "Bonus percent: " ]
        , label []
            [ a [ onClick EditModeBonusPercent ]
                [ text <| String.fromInt percent ]
            ]
        ]


viewBonusPercentEditMode : String -> Html Msg
viewBonusPercentEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "bonusPercent"
                , placeholder "Bonus percent"
                , onInput InputBonusPercent
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SaveBonusPercent editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewBonusThreshold : Int -> Mode -> Html Msg
viewBonusThreshold threshold mode =
    case mode of
        EditBonusThreshold editable ->
            viewBonusThresholdEditMode editable

        _ ->
            viewBonusThresholdNormalMode
                threshold


viewBonusThresholdNormalMode : Int -> Html Msg
viewBonusThresholdNormalMode threshold =
    li []
        [ label [] [ text "Bonus threshold: " ]
        , label []
            [ a [ onClick EditModeBonusThreshold ]
                [ text <| String.fromInt threshold ]
            ]
        ]


viewBonusThresholdEditMode : String -> Html Msg
viewBonusThresholdEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "bonusThreshold"
                , placeholder "Bonus threshold"
                , onInput InputBonusThreshold
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SaveBonusThreshold editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewHopper : Faze -> Mode -> Html Msg
viewHopper faze mode =
    case mode of
        EditHopper editable ->
            viewHopperEditMode editable

        _ ->
            viewHopperNormalMode faze


viewHopperNormalMode : Faze -> Html Msg
viewHopperNormalMode faze =
    li []
        [ label [] [ text "Hopper: " ]
        , label []
            [ a [ onClick EditModeHopper ]
                [ text <| fazeToString faze ]
            ]
        ]


viewHopperEditMode : Faze -> Html Msg
viewHopperEditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Disabled ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetHopper Disabled
                ]
                [ text "Disable" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                CcTalk ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetHopper CcTalk
                ]
                [ text "CcTalk" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Pulse ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetHopper Pulse
                ]
                [ text "Pulse" ]
            ]
        ]


viewHopperMode : Faze -> Mode -> Html Msg
viewHopperMode faze mode =
    case mode of
        EditHopperMode editable ->
            viewHopperModeEditMode editable

        _ ->
            viewHopperModeNormalMode faze


viewHopperModeNormalMode : Faze -> Html Msg
viewHopperModeNormalMode faze =
    li []
        [ label [] [ text "Hopper mode: " ]
        , label []
            [ a [ onClick EditModeHopperMode ]
                [ text <| fazeToString faze ]
            ]
        ]


viewHopperModeEditMode : Faze -> Html Msg
viewHopperModeEditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Mode_1 ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetHopperMode Mode_1
                ]
                [ text "Mode 1" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Mode_2 ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetHopperMode Mode_2
                ]
                [ text "Mode 2" ]
            ]
        ]


viewBillValidator : Faze -> Mode -> Html Msg
viewBillValidator faze mode =
    case mode of
        EditBillValidator editable ->
            viewBillValidatorEditMode editable

        _ ->
            viewBillValidatorNormalMode faze


viewBillValidatorNormalMode : Faze -> Html Msg
viewBillValidatorNormalMode faze =
    li []
        [ label [] [ text "Bill validator: " ]
        , label []
            [ a [ onClick EditModeBillValidator ]
                [ text <| fazeToString faze ]
            ]
        ]


viewBillValidatorEditMode : Faze -> Html Msg
viewBillValidatorEditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Disabled ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetBillValidator Disabled
                ]
                [ text "Disable" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                CcTalk ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetBillValidator CcTalk
                ]
                [ text "CcTalk" ]
            ]
        ]


viewRfidReader1 : Faze -> Mode -> Html Msg
viewRfidReader1 faze mode =
    case mode of
        EditRfidReader1 editable ->
            viewRfidReader1EditMode editable

        _ ->
            viewRfidReader1NormalMode faze


viewRfidReader1NormalMode : Faze -> Html Msg
viewRfidReader1NormalMode faze =
    li []
        [ label [] [ text "RfidReader1: " ]
        , label []
            [ a [ onClick EditModeRfidReader1 ]
                [ text <| fazeToString faze ]
            ]
        ]


viewRfidReader1EditMode : Faze -> Html Msg
viewRfidReader1EditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Disabled ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetRfidReader1 Disabled
                ]
                [ text "Disable" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Enabled ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetRfidReader1 Enabled
                ]
                [ text "Enable" ]
            ]
        ]


viewRfidReader2 : Faze -> Mode -> Html Msg
viewRfidReader2 faze mode =
    case mode of
        EditRfidReader2 editable ->
            viewRfidReader2EditMode editable

        _ ->
            viewRfidReader2NormalMode faze


viewRfidReader2NormalMode : Faze -> Html Msg
viewRfidReader2NormalMode faze =
    li []
        [ label [] [ text "RfidReader2: " ]
        , label []
            [ a [ onClick EditModeRfidReader2 ]
                [ text <| fazeToString faze ]
            ]
        ]


viewRfidReader2EditMode : Faze -> Html Msg
viewRfidReader2EditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Disabled ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetRfidReader2 Disabled
                ]
                [ text "Disable" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Enabled ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetRfidReader2 Enabled
                ]
                [ text "Enable" ]
            ]
        ]


viewDispenser : Faze -> Mode -> Html Msg
viewDispenser faze mode =
    case mode of
        EditDispenser editable ->
            viewDispenserEditMode editable

        _ ->
            viewDispenserNormalMode faze


viewDispenserNormalMode : Faze -> Html Msg
viewDispenserNormalMode faze =
    li []
        [ label [] [ text "Dispenser: " ]
        , label []
            [ a [ onClick EditModeDispenser ]
                [ text <| fazeToString faze ]
            ]
        ]


viewDispenserEditMode : Faze -> Html Msg
viewDispenserEditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Disabled ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetDispenser Disabled
                ]
                [ text "Disable" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                CRT_531 ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetDispenser CRT_531
                ]
                [ text "CRT 531" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                TCD_820M ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetDispenser TCD_820M
                ]
                [ text "TCD 820M" ]
            ]
        ]


viewCardOut : Faze -> Mode -> Html Msg
viewCardOut faze mode =
    case mode of
        EditCardOut editable ->
            viewCardOutEditMode editable

        _ ->
            viewCardOutNormalMode faze


viewCardOutNormalMode : Faze -> Html Msg
viewCardOutNormalMode faze =
    li []
        [ label [] [ text "Card out: " ]
        , label []
            [ a [ onClick EditModeCardOut ]
                [ text <| fazeToString faze ]
            ]
        ]


viewCardOutEditMode : Faze -> Html Msg
viewCardOutEditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                ToGate ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetCardOut ToGate
                ]
                [ text "ToGate" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                FullOut ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetCardOut FullOut
                ]
                [ text "FullOut" ]
            ]
        ]


viewNetwork : Faze -> Mode -> Html Msg
viewNetwork faze mode =
    case mode of
        EditNetwork editable ->
            viewNetworkEditMode editable

        _ ->
            viewNetworkNormalMode faze


viewNetworkNormalMode : Faze -> Html Msg
viewNetworkNormalMode faze =
    li []
        [ label [] [ text "Network: " ]
        , label []
            [ a [ onClick EditModeNetwork ]
                [ text <| fazeToString faze ]
            ]
        ]


viewNetworkEditMode : Faze -> Html Msg
viewNetworkEditMode faze =
    li []
        [ div
            [ class "pure-button-group"
            , Aria.role "group"
            , Aria.ariaLabel "..."
            ]
            [ button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                None ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetNetwork None
                ]
                [ text "None" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                RS_485 ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetNetwork RS_485
                ]
                [ text "RS 485" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Can ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetNetwork Can
                ]
                [ text "Can" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                Ethernet ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetNetwork Ethernet
                ]
                [ text "Ethernet" ]
            , button
                [ class <|
                    "pure-button"
                        ++ (case faze of
                                WiFi ->
                                    " button-secondary"
                                        ++ " pure-button-active"

                                _ ->
                                    " button-warning"
                           )
                , onClick <| SetNetwork WiFi
                ]
                [ text "WiFi" ]
            ]
        ]



-- TODO mappers


multiply : Int -> Int -> Int
multiply index digit =
    ((10 - index) * 10) * digit


arrayToInt : Array -> Int
arrayToInt array =
    List.sum <|
        Array.toList <|
            Array.indexedMap
                multiply
                array


intToArray : Int -> Array
intToArray value =
    if value < 10000000000 && value >= 0 then
        Array.repeat 10 0

    else
        Array.repeat 10 0
