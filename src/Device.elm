module Device exposing
    ( Device
    , Identifier
    , Model
    , Msg(..)
    , Type(..)
    , createShortcut
    , decoder
    , encode
    , idToString
    , init
    , newDevice
    , update
    , view
    )

import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Debug
import Device.Channel as Channel
import Device.Config as Config
import Device.Counter as Counter
import Device.Shortcut as DeviceShortcut
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { device : Device
    , mode : Mode
    , controlPanel : ControlPanel
    }


type Mode
    = Normal
    | NameEdit String


type ControlPanel
    = Settings Settings
    | Counters Counter.Model


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias DeviceShortcut =
    DeviceShortcut.Shortcut


type alias Device =
    { id : Identifier
    , name : Name
    , info : Info
    , branch : BranchShortcut
    , counters : Counter.Model
    , settings : Settings
    }


type alias Identifier =
    String


type alias Name =
    String


type alias Info =
    ( DeviceModel, Version, SoftVersion )


type alias DeviceModel =
    String


type alias Version =
    String


type alias SoftVersion =
    String


type Settings
    = Channels Channel.Model
    | Configs Config.Model


type Type
    = Washbox
    | Exchange



-- MAP


idToString : Identifier -> String
idToString id =
    id



-- INIT


init : Device -> Model
init device =
    Model device Normal (Settings device.settings)



--CREATE


createShortcut : Device -> DeviceShortcut
createShortcut device =
    { id = device.id
    , name = device.name
    }


newIdentifier : String -> Identifier
newIdentifier salt =
    Hash.sha512_224 salt


newDeviceModel : DeviceModel
newDeviceModel =
    ""


newVersion : Version
newVersion =
    ""


newSoftVersion : SoftVersion
newSoftVersion =
    ""


newDevice : Type -> String -> BranchShortcut.Shortcut -> Device
newDevice deviceType salt branch =
    let
        id =
            newIdentifier salt

        settings =
            case deviceType of
                Washbox ->
                    Channels <| Channel.init 0

                Exchange ->
                    Configs Config.newConfig
    in
    { id = id
    , name = String.slice 0 7 <| idToString id
    , info = infoOf newDeviceModel newVersion newSoftVersion
    , branch = branch
    , counters = Counter.newCounters []
    , settings = settings
    }



--ENCODE


encode : Device -> E.Value
encode device =
    let
        settingsEncoder =
            case device.settings of
                Channels channels ->
                    Channel.encode channels

                Configs configs ->
                    Config.encode configs
    in
    E.object
        [ ( "id", E.string device.id )
        , ( "name", E.string device.name )
        , ( "info", encodeInfo device.info )
        , BranchShortcut.encode device.branch
        , ( "counters", Counter.encode device.counters )
        , ( "settings", settingsEncoder )
        ]


encodeInfo : Info -> E.Value
encodeInfo ( model, version, softVersion ) =
    E.object
        [ ( "model", E.string model )
        , ( "version", E.string version )
        , ( "softVersion", E.string softVersion )
        ]



--DECODE


decoder : D.Decoder Device
decoder =
    D.map6 Device
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "info" decodeInfo)
        BranchShortcut.decoder
        (D.field "counters" Counter.decoder)
        (D.field "settings" <|
            D.oneOf
                [ decodeConfigs
                , decodeChannels
                ]
        )


decodeConfigs : D.Decoder Settings
decodeConfigs =
    D.map Configs
        Config.decoder


decodeChannels : D.Decoder Settings
decodeChannels =
    D.map Channels
        Channel.decoder


infoOf : DeviceModel -> Version -> SoftVersion -> Info
infoOf model version softVersion =
    ( model, version, softVersion )


decodeInfo : D.Decoder Info
decodeInfo =
    D.map3 infoOf
        (D.field "model" D.string)
        (D.field "version" D.string)
        (D.field "softVersion" D.string)



-- UPDATE


type Msg
    = NameEditMode
    | SetName String
    | NormalMode
    | NameInput String
    | SettingsMsg TabMsg
    | GoToContainer BranchShortcut.Identifier


type TabMsg
    = WashBoxMsg Channel.Msg
    | ExchangeMsg Config.Msg
    | CountersMsg Counter.Msg
    | ToSettings
    | ToCounters


updateSettings : TabMsg -> Model -> ( Model, Bool )
updateSettings tabMsg model =
    let
        deviceOrig =
            model.device
    in
    case tabMsg of
        WashBoxMsg channelMsg ->
            case model.device.settings of
                Channels channelsOrig ->
                    let
                        ( channels, saveMe ) =
                            Channel.update
                                channelMsg
                                channelsOrig

                        device =
                            { deviceOrig
                                | settings = Channels channels
                            }
                    in
                    ( { model
                        | device = device
                        , controlPanel = Settings device.settings
                      }
                    , saveMe
                    )

                _ ->
                    -- let
                    --     log =
                    --         Debug.log
                    --             "Operation not permited"
                    --             " !"
                    -- in
                    ( model, False )

        ExchangeMsg exchangeMsg ->
            case model.device.settings of
                Configs configsOrig ->
                    let
                        ( configs, saveMe ) =
                            Config.update exchangeMsg configsOrig

                        device =
                            { deviceOrig
                                | settings = Configs configs
                            }
                    in
                    ( { model
                        | device = device
                        , controlPanel = Settings device.settings
                      }
                    , saveMe
                    )

                _ ->
                    -- let
                    --     log =
                    --         Debug.log
                    --             "Operation not permited"
                    --             " !"
                    -- in
                    ( model, False )

        CountersMsg countersMsg ->
            let
                ( counters, saveMe ) =
                    Counter.update
                        countersMsg
                        model.device.counters

                device =
                    { deviceOrig | counters = counters }
            in
            ( { model | device = device }, saveMe )

        ToSettings ->
            ( { model
                | controlPanel = Settings deviceOrig.settings
                , mode = Normal
              }
            , False
            )

        ToCounters ->
            ( { model
                | controlPanel = Counters deviceOrig.counters
                , mode = Normal
              }
            , False
            )


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        SettingsMsg panelMsg ->
            updateSettings panelMsg model

        NormalMode ->
            ( { model | mode = Normal }, False )

        NameInput editable ->
            ( { model | mode = NameEdit editable }, False )

        SetName name ->
            let
                device =
                    model.device
            in
            ( { model
                | device =
                    { device | name = name }
                , mode = Normal
              }
            , True
            )

        NameEditMode ->
            ( { model
                | mode = NameEdit model.device.name
              }
            , False
            )

        GoToContainer id ->
            ( model, False )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ div [ class "header" ]
            [ case model.mode of
                NameEdit editable ->
                    viewNameEditMode editable

                _ ->
                    viewNameNormalMode model.device.name
            ]
        , div [ class "content" ]
            [ viewContainerAndInfo model
            , viewControlPanel model.controlPanel
            ]
        ]


viewContainerAndInfo : Model -> Html Msg
viewContainerAndInfo model =
    let
        ( deviceModel, version, softVersion ) =
            model.device.info

        container =
            model.device.branch

        textView : String -> String
        textView value =
            if String.isEmpty value then
                "NotSet"

            else
                value
    in
    Html.form [ class "pure-form pure-form-stacked" ]
        [ fieldset []
            [ div [ class "pure-g" ]
                [ div [ class "pure-u-1 pure-u-md-1-4" ]
                    [ label
                        [ for "container"
                        , class "pure-u-23-24"
                        , onClick <|
                            GoToContainer container.id
                        ]
                        [ text container.name ]
                    ]

                -- , div [ class "pure-u-1 pure-u-md-1-4" ]
                --     [ label
                --         [ for "deviceModel"
                --         , class "pure-u-23-24"
                --         ]
                --         [ text <|
                --             textView deviceModel
                --         ]
                --     ]
                -- , div [ class "pure-u-1 pure-u-md-1-4" ]
                --     [ label
                --         [ for "version"
                --         , class "pure-u-23-24"
                --         ]
                --         [ text <|
                --             textView version
                --         ]
                --     ]
                -- , div [ class "pure-u-1 pure-u-md-1-4" ]
                --     [ label
                --         [ for "softVersion"
                --         , class "pure-u-23-24"
                --         ]
                --         [ text <|
                --             textView softVersion
                --         ]
                --     ]
                ]
            ]
        ]


viewNameNormalMode : String -> Html Msg
viewNameNormalMode name =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ h2 []
                [ label
                    [ for "branchName"
                    , onClick NameEditMode
                    ]
                    [ text name ]
                ]
            , span
                [ class "pure-form-message-inline" ]
                [ text "Click to edit" ]
            ]
        ]


viewNameEditMode : String -> Html Msg
viewNameEditMode editable =
    Html.form
        [ class "pure-form" ]
        [ fieldset []
            [ input
                [ id "deviceName"
                , placeholder "Device name"
                , onInput NameInput
                , value editable
                ]
                []
            , button
                [ type_ "submit"
                , class "pure-button button-warning"
                , onClick <| SetName editable
                ]
                [ text "Save" ]
            , button
                [ class "pure-button button-secondary"
                , onClick NormalMode
                ]
                [ text "Cancel" ]
            ]
        ]


viewControlPanel : ControlPanel -> Html Msg
viewControlPanel model =
    case model of
        Settings settings ->
            div []
                [ div [ class "pure-menu pure-menu-horizontal" ]
                    [ ul [ class "pure-menu-list" ]
                        [ li [ class "pure-menu-item" ]
                            [ a
                                [ href "#"
                                , class <|
                                    "pure-menu-link"
                                        ++ " pure-menu-selected"
                                , onClick <| SettingsMsg ToSettings
                                ]
                                [ text "Settings" ]
                            ]
                        , li [ class "pure-menu-item" ]
                            [ a
                                [ href "#"
                                , class "pure-menu-link"
                                , onClick <| SettingsMsg ToCounters
                                ]
                                [ text "Counters" ]
                            ]
                        ]
                    ]
                , case settings of
                    Channels channels ->
                        Html.map
                            (SettingsMsg
                                << WashBoxMsg
                            )
                        <|
                            Channel.view channels

                    Configs parameters ->
                        Html.map
                            (SettingsMsg
                                << ExchangeMsg
                            )
                        <|
                            Config.view parameters
                ]

        Counters counters ->
            div []
                [ div [ class "pure-menu pure-menu-horizontal" ]
                    [ ul [ class "pure-menu-list" ]
                        [ li [ class "pure-menu-item" ]
                            [ a
                                [ href "#"
                                , class "pure-menu-link"
                                , onClick <| SettingsMsg ToSettings
                                ]
                                [ text "Settings" ]
                            ]
                        , li [ class "pure-menu-item" ]
                            [ a
                                [ href "#"
                                , class "pure-menu-link pure-menu-selected"
                                , onClick <| SettingsMsg ToCounters
                                ]
                                [ text "Counters" ]
                            ]
                        ]
                    ]
                , Html.map
                    (SettingsMsg
                        << CountersMsg
                    )
                  <|
                    Counter.view counters
                ]



-- TODO mappers
