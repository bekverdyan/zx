module Device exposing (Device, DeviceType(..), Identifier, Msg(..), ViewModel, createShortcut, decoder, encode, idToString, newDevice, update, view)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Debug
import Device.Counter as Counter
import Device.Setting as Setting
import Device.Shortcut as DeviceShortcut
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias ViewModel =
    { device : Device
    , tabState : Tab.State
    }


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias DeviceShortcut =
    DeviceShortcut.Shortcut


type alias Device =
    { id : Identifier
    , name : Name
    , info : Info
    , branch : BranchShortcut
    , counters : Counter.Counters
    , settings : Setting.Settings
    }


type alias Identifier =
    String


type alias Name =
    String


type alias Info =
    ( Model, Version, SoftVersion )


type alias Model =
    String


type alias Version =
    String


type alias SoftVersion =
    String


type DeviceType
    = Washbox
    | Exchange



-- MAP


idToString : Identifier -> String
idToString id =
    id



-- INIT


init : Device -> ViewModel
init device =
    ViewModel device Tab.initialState



--CREATE


createShortcut : Device -> DeviceShortcut
createShortcut device =
    { id = device.id
    , name = device.name
    }


newIdentifier : String -> Identifier
newIdentifier salt =
    Hash.sha512_224 salt


newName : Name
newName =
    ""


newModel : Model
newModel =
    ""


newVersion : Version
newVersion =
    ""


newSoftVersion : SoftVersion
newSoftVersion =
    ""


newDevice : DeviceType -> String -> BranchShortcut.Shortcut -> Device
newDevice deviceType salt branch =
    let
        settings =
            case deviceType of
                Washbox ->
                    Setting.newChannels 0

                Exchange ->
                    Setting.newConfig
    in
    { id = newIdentifier salt
    , name = newName
    , info = infoOf newModel newVersion newSoftVersion
    , branch = branch
    , counters = Counter.newCounters []
    , settings = settings
    }



--ENCODE


encode : Device -> E.Value
encode device =
    E.object
        [ ( "id", E.string device.id )
        , ( "name", E.string device.name )
        , ( "info", encodeInfo device.info )
        , BranchShortcut.encode device.branch
        , ( "counters", Counter.encode device.counters )
        , ( "settings", Setting.encode device.settings )
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
        (D.field "settings" Setting.decoder)


infoOf : Model -> Version -> SoftVersion -> Info
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
    = DeviceTabMsg Tab.State


update : Msg -> ViewModel -> ( ViewModel, Bool )
update msg viewModel =
    case msg of
        DeviceTabMsg state ->
            ( { viewModel | tabState = state }, False )



-- VIEW


view : ViewModel -> Html Msg
view model =
    Card.config []
        |> Card.header [ class "text-center" ]
            [ h3 [ Spacing.mt2 ] [ text model.device.id ]
            ]
        |> Card.block []
            [ Block.titleH4 [] [ viewInfo model.device.info ]
            , Block.text []
                [ viewTabs
                    model.device.counters
                    model.device.settings
                    model.tabState
                ]
            , Block.custom <|
                Button.button [ Button.primary ]
                    [ text "Go somewhere" ]
            ]
        |> Card.view


viewInfo : Info -> Html Msg
viewInfo ( model, version, softVersion ) =
    Card.config []
        |> Card.listGroup
            [ ListGroup.li [ ListGroup.success ]
                [ text <| "Model: " ++ model ]
            , ListGroup.li [ ListGroup.info ]
                [ text <| "Version: " ++ version ]
            , ListGroup.li [ ListGroup.warning ]
                [ text <| "Soft Version: " ++ softVersion ]
            ]
        |> Card.view



-- TODO View counters and settings in Tabs


viewTabs :
    Counter.Counters
    -> Setting.Settings
    -> Tab.State
    -> Html Msg
viewTabs counters settings state =
    Tab.config
        DeviceTabMsg
        |> Tab.items
            [ Tab.item
                { id = "counters"
                , link = Tab.link [] [ text "Counters" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ]
                        [ h4 [] [ text "Exo" ]
                        , p [] [ Counter.view counters ]
                        ]
                }
            , Tab.item
                { id = "settings"
                , link = Tab.link [] [ text "Settings" ]
                , pane =
                    Tab.pane [ Spacing.mt3 ]
                        [ h4 [] [ text "Exo" ]
                        , p [] [ Setting.view settings ]
                        ]
                }
            ]
        |> Tab.view state



-- TODO mappers
