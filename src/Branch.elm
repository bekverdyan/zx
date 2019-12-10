module Branch exposing (Branch, Identifier, Mode(..), Model, Msg(..), createShortcut, decoder, encode, idToString, newBranch, update, view, viewInDashboard)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Utilities.Spacing as Spacing
import Branch.Shortcut as BranchShortcut
import Crypto.Hash as Hash
import Device.Shortcut as DeviceShortcut
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { branch : Branch
    , mode : Mode
    }


type Mode
    = Normal
    | NameEdit


type alias Branch =
    { id : Identifier
    , name : Name
    , shortcuts : Shortcuts
    }


type alias Identifier =
    String


type alias Name =
    String


type alias BranchShortcut =
    BranchShortcut.Shortcut


type alias Shortcuts =
    Dict.Dict DeviceShortcut.Identifier DeviceShortcut.Shortcut



-- CREATE


createShortcut : Branch -> BranchShortcut
createShortcut branch =
    { id = branch.id, name = branch.name }



-- CREATE


newIdentifier : String -> Identifier
newIdentifier salt =
    Hash.sha512_224 salt


newBranch : String -> String -> Branch
newBranch name salt =
    { id = newIdentifier salt
    , name = name
    , shortcuts = Dict.empty
    }



-- ENCODE


encode : Branch -> E.Value
encode branch =
    E.object
        [ ( "id", E.string branch.id )
        , ( "name", E.string branch.name )
        , ( "shortcuts", encodeShortcuts branch.shortcuts )
        ]


encodeShortcuts : Shortcuts -> E.Value
encodeShortcuts shortcuts =
    E.dict idToString DeviceShortcut.encode shortcuts


idToString : Identifier -> String
idToString id =
    id



-- DECODER


decoder : D.Decoder Branch
decoder =
    D.map3 Branch
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "shortcuts" decodeShortcuts)


decodeShortcuts : D.Decoder Shortcuts
decodeShortcuts =
    D.dict DeviceShortcut.decoder



-- MAP
-- TODO deal with expose necessary functions


addShortcut : DeviceShortcut.Shortcut -> Shortcuts -> Shortcuts
addShortcut shortcut shortcuts =
    Dict.insert
        shortcut.id
        shortcut
        shortcuts


removeShortcut : Identifier -> Shortcuts -> Shortcuts
removeShortcut id shortcuts =
    Dict.remove id shortcuts



-- UPDATE


type Msg
    = NameEditMode
    | SetName String
    | NormalMode
    | NewDevice Branch
    | NewName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameEditMode ->
            let
                gag =
                    Debug.log "yaxq" model.mode
            in
            ( { model | mode = NameEdit }, Cmd.none )

        SetName name ->
            let
                branch =
                    model.branch
            in
            ( { model
                | branch =
                    { branch
                        | name = name
                    }
                , mode = Normal
              }
            , Cmd.none
            )

        NormalMode ->
            ( { model | mode = Normal }, Cmd.none )

        NewDevice branch ->
            ( model, Cmd.none )

        NewName name ->
            let
                branch =
                    model.branch
            in
            ( { model
                | branch = { branch | name = name }
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    Card.config []
        |> Card.headerH3 [] [ text model.branch.id ]
        |> Card.block []
            [ Block.titleH3 []
                [ case model.mode of
                    Normal ->
                        viewNormalModeName model

                    NameEdit ->
                        viewEditModeName model
                ]
            , Block.text [] [ text "" ]
            , Block.custom <|
                Button.button
                    [ Button.primary
                    , Button.attrs
                        [ onClick <|
                            NewDevice model.branch
                        ]
                    ]
                    [ text "New Device" ]
            ]
        |> Card.footer [] [ text "" ]
        |> Card.view


viewNormalModeName : Model -> Html Msg
viewNormalModeName model =
    div []
        [ Alert.simpleSecondary []
            [ text model.branch.name
            , Button.button
                [ Button.dark
                , Button.attrs
                    [ Spacing.ml1
                    , onClick NameEditMode
                    ]
                ]
                [ text "Edit" ]
            ]
        ]


viewEditModeName : Model -> Html Msg
viewEditModeName model =
    div []
        [ Alert.simpleWarning []
            [ InputGroup.config
                (InputGroup.text
                    [ Input.attrs [ value model.branch.name ]
                    , Input.placeholder "Branch name"
                    ]
                )
                |> InputGroup.view
            , Button.button
                [ Button.success
                , Button.attrs
                    [ Spacing.ml1
                    , onClick <| SetName model.branch.name
                    ]
                ]
                [ text "Save" ]
            , Button.button
                [ Button.warning
                , Button.attrs
                    [ Spacing.ml1
                    , onClick NormalMode
                    ]
                ]
                [ text "Cancel" ]
            ]
        ]


viewInDashboard : msg -> Branch -> Html msg -> Html msg
viewInDashboard openBranchCmd branch shortcuts =
    li [ id branch.id ]
        [ label [ attribute "for" branch.id ]
            [ a [ onClick openBranchCmd ] [ text branch.name ] ]
        , input
            [ attribute "checked" ""
            , attribute "id" branch.id
            , attribute "value" ""
            , attribute "type" "checkbox"
            ]
            []
        , shortcuts
        ]
