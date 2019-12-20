module Device exposing
    ( Device
    ,  Identifier
       -- , Mode(..)

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

import Bootstrap.Alert as Alert
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
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
    , tabState : Tab.State
    , mode : Mode
    }


type Mode
    = Normal
    | NameEdit String


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
    Model device Tab.initialState Normal


subscriptions : Model -> Sub Msg
subscriptions model =
    Tab.subscriptions model.tabState
        (SettingsMsg
            << TabStateMsg
        )



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
                    Channels <| Channel.newChannels 0

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
    | TabStateMsg Tab.State


updateSettings : TabMsg -> Model -> ( Model, Bool )
updateSettings tabMsg model =
    case tabMsg of
        TabStateMsg state ->
            ( { model | tabState = state }, False )

        WashBoxMsg channelMsg ->
            case model.device.settings of
                Channels channelsOrig ->
                    let
                        ( channels, saveMe ) =
                            Channel.update
                                channelMsg
                                channelsOrig

                        deviceOrig =
                            model.device

                        device =
                            { deviceOrig
                                | settings = Channels channels
                            }
                    in
                    ( { model | device = device }, saveMe )

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

                        deviceOrig =
                            model.device

                        device =
                            { deviceOrig
                                | settings = Configs configs
                            }
                    in
                    ( { model | device = device }, saveMe )

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

                deviceOrig =
                    model.device

                device =
                    { deviceOrig | counters = counters }
            in
            ( { model | device = device }, saveMe )


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
    Card.config []
        |> Card.header [ class "text-center" ]
            [ h3 [ Spacing.mt2 ]
                [ case model.mode of
                    Normal ->
                        viewNameNormalMode model.device.name

                    NameEdit editable ->
                        viewNameEditMode editable
                ]
            ]
        |> Card.block []
            [ Block.titleH4 [] [ viewCommon model ]
            , Block.text []
                [ viewSettings model ]
            ]
        |> Card.view


viewCommon : Model -> Html Msg
viewCommon model =
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
    div []
        [ Button.button
            [ Button.roleLink
            , Button.attrs
                [ Spacing.ml1
                , onClick <| GoToContainer container.id
                ]
            ]
            [ h4 []
                [ text "Container: "
                , Badge.badgeDark [ Spacing.ml1 ]
                    [ text <| container.name ]
                ]
            ]
        , h4 []
            [ text "Model: "
            , Badge.pillDanger [ Spacing.ml1 ]
                [ text <| textView deviceModel ]
            ]
        , h4 []
            [ text "Version: "
            , Badge.pillDanger [ Spacing.ml1 ]
                [ text <| textView version ]
            ]
        , h4 []
            [ text "Soft version: "
            , Badge.pillDanger [ Spacing.ml1 ]
                [ text <| textView softVersion ]
            ]
        ]


viewNameNormalMode : String -> Html Msg
viewNameNormalMode name =
    div []
        [ h3 []
            [ Badge.badgeLight
                [ Spacing.ml1
                , onClick NameEditMode
                ]
                [ text name ]
            ]
        ]


viewNameEditMode : String -> Html Msg
viewNameEditMode editable =
    div []
        [ Alert.simpleWarning []
            [ InputGroup.config
                (InputGroup.text
                    [ Input.id "nameInput"
                    , Input.onInput NameInput
                    , Input.value editable
                    ]
                )
                |> InputGroup.successors
                    [ InputGroup.button
                        [ Button.success
                        , Button.onClick <| SetName editable
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


viewSettings : Model -> Html Msg
viewSettings model =
    let
        settingsView =
            case model.device.settings of
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

        countersView =
            Html.map
                (SettingsMsg
                    << CountersMsg
                )
            <|
                Counter.view model.device.counters
    in
    Tab.config
        (SettingsMsg
            << TabStateMsg
        )
        |> Tab.items
            [ Tab.item
                { id = "settings"
                , link = Tab.link [] [ text "Settings" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ]
                        [ h4 [] [ text "" ]
                        , p [] [ settingsView ]
                        ]
                }
            , Tab.item
                { id = "counters"
                , link = Tab.link [] [ text "Counters" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ]
                        [ h4 [] [ text "" ]
                        , p [] [ countersView ]
                        ]
                }
            ]
        |> Tab.view model.tabState



-- TODO mappers
