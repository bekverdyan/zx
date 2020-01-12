module Assets.Component exposing
    ( Model
    , Msg
    , decoder
    , encode
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria as Aria
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { component : Component
    , mode : Mode
    }


type Mode
    = Normal
    | NameEdit Name
    | UnitEdit (Maybe Unit)
    | MultiEdit ( Name, Maybe Unit )
    | Remove Name
    | InChannel
    | InCounter


type alias RowComponent =
    { name : Name
    , unit : Maybe Unit
    }


type alias Component =
    ( Name, Unit )


type alias Name =
    String


type Unit
    = Liter Float
    | Gram Float
    | Kilowatt Float
    | Meter Float


encode : Model -> E.Value
encode model =
    let
        ( name, unit ) =
            model.component

        ( unitStr, value ) =
            getUnitValues unit
    in
    E.object
        [ ( "name", E.string name )
        , ( "unit", E.string unitStr )
        , ( "value", E.float value )
        ]


getUnitValues : Unit -> ( String, Float )
getUnitValues unit =
    case unit of
        Liter value ->
            ( "Liter", value )

        Gram value ->
            ( "Gram", value )

        Kilowatt value ->
            ( "Kilowatt", value )

        Meter value ->
            ( "Meter", value )


decoder : D.Decoder (Maybe Component)
decoder =
    D.map3 createComponent
        (D.field "name" D.string)
        (D.field "unit" D.string)
        (D.field "value" D.float)


createComponent : String -> String -> Float -> Maybe Component
createComponent name unit value =
    case unit of
        "Liter" ->
            Just ( name, Liter value )

        "Gram" ->
            Just ( name, Gram value )

        "Kilowatt" ->
            Just ( name, Kilowatt value )

        "Meter" ->
            Just ( name, Meter value )

        _ ->
            Nothing


type Msg
    = ToNameEditMode
    | InputName Name
    | ToUnitEditMode
    | SelectUnit Unit
    | SaveComponent ( Name, Unit )
    | Cancel
    | NewComponent
    | DeleteComponent Name


type Action
    = NoOp
    | Save
    | Delete


update : Msg -> Model -> ( Model, Action )
update msg model =
    case msg of
        ToNameEditMode ->
            ( { model
                | mode =
                    NameEdit <|
                        Tuple.first
                            model.component
              }
            , NoOp
            )

        InputName editable ->
            ( { model
                | mode =
                    NameEdit
                        editable
              }
            , NoOp
            )

        ToUnitEditMode ->
            ( { model
                | mode =
                    UnitEdit <|
                        Just <|
                            Tuple.second
                                model.component
              }
            , NoOp
            )

        SelectUnit selected ->
            ( { model
                | mode =
                    UnitEdit <| Just selected
              }
            , NoOp
            )

        SaveComponent ( name, unit ) ->
            ( { model
                | component =
                    ( name
                    , unit
                    )
              }
            , Save
            )

        Cancel ->
            ( { model | mode = Normal }, NoOp )

        NewComponent ->
            ( { model
                | mode =
                    MultiEdit ( "", Nothing )
              }
            , NoOp
            )

        DeleteComponent name ->
            ( model, Delete )



-- VIEW UNIT


view : Model -> Html Msg
view model =
    let
        mode =
            model.mode
    in
    case mode of
        MultiEdit ( editable, selected ) ->
            Html.form
                [ class "pure-form" ]
                [ fieldset []
                    [ viewNameInput
                        editable
                    , viewUnitDropdown
                        selected
                    , viewMutatorButton
                        (case selected of
                            Just unit ->
                                if isValidName editable then
                                    Functional
                                        ( editable, unit )

                                else
                                    Disabled

                            Nothing ->
                                Disabled
                        )
                        Save
                    , viewCancelButton
                    ]
                ]

        _ ->
            li []
                [ viewName
                    (Tuple.first model.component)
                    model.mode
                    (Tuple.second model.component)
                , viewUnit
                    (Just <|
                        Tuple.second model.component
                    )
                    (Tuple.first model.component)
                    model.mode
                ]


viewName : Name -> Mode -> Unit -> Html Msg
viewName name mode unit =
    case mode of
        NameEdit editable ->
            viewNameEditMode
                name
                (Just unit)

        _ ->
            viewNameNormalMode name


viewNameNormalMode : Name -> Html Msg
viewNameNormalMode name =
    li []
        [ label []
            [ a
                [ class "pure-button"
                , href "#"
                , onClick ToNameEditMode
                ]
                [ text <|
                    if isValidName name then
                        name

                    else
                        " "
                ]
            ]
        ]


viewNameInput : Name -> Html Msg
viewNameInput editable =
    input
        [ id "name"
        , placeholder "Component Name"
        , onInput InputName
        , value editable
        ]
        []


viewNameEditMode : Name -> Maybe Unit -> Html Msg
viewNameEditMode editable selected =
    Html.form
        [ class "pure-form" ]
        [ viewNameInput editable
        , viewMutatorButton
            (if isValidName editable then
                case selected of
                    Just unit ->
                        Functional ( editable, unit )

                    Nothing ->
                        Disabled

             else
                Disabled
            )
            Save
        , viewCancelButton
        ]


viewUnit : Maybe Unit -> Name -> Mode -> Html Msg
viewUnit selected name mode =
    case mode of
        UnitEdit unit ->
            viewUnitEditMode unit name

        _ ->
            viewUnitNormalMode selected


viewUnitNormalMode : Maybe Unit -> Html Msg
viewUnitNormalMode selected =
    li []
        [ label []
            [ a
                [ class "pure-button"
                , href "#"
                , onClick ToUnitEditMode
                ]
                [ text <|
                    case selected of
                        Just unit ->
                            unitToString unit

                        Nothing ->
                            " "
                ]
            ]
        ]


viewUnitDropdown : Maybe Unit -> Html Msg
viewUnitDropdown selected =
    let
        units =
            [ viewUnitMember (Liter 0) selected
            , viewUnitMember (Gram 0) selected
            , viewUnitMember (Kilowatt 0) selected
            , viewUnitMember (Meter 0) selected
            ]

        options =
            case selected of
                Just _ ->
                    units

                Nothing ->
                    option
                        [ Aria.ariaSelected "true" ]
                        []
                        :: units
    in
    select [ id "units" ] options


viewUnitEditMode : Maybe Unit -> Name -> Html Msg
viewUnitEditMode selected name =
    li []
        [ viewUnitDropdown selected
        , viewMutatorButton
            (case selected of
                Just unit ->
                    Functional ( name, unit )

                Nothing ->
                    Disabled
            )
            Save
        , viewCancelButton
        ]


viewUnitMember : Unit -> Maybe Unit -> Html Msg
viewUnitMember member selected =
    let
        attributes =
            [ onClick <| SelectUnit <| Liter 0 ]
    in
    option
        (case selected of
            Just unit ->
                if member == unit then
                    Aria.ariaSelected "true" :: attributes

                else
                    attributes

            Nothing ->
                attributes
        )
        [ text <| unitToString member ]



-- VIEW BUTTONS


type ButtonState
    = Functional ( Name, Unit )
    | Disabled


viewMutatorButton : ButtonState -> Action -> Html Msg
viewMutatorButton state action =
    case state of
        Functional ( name, unit ) ->
            let
                ( buttonName, buttonStyle, buttonMsg ) =
                    case action of
                        Save ->
                            ( "Save"
                            , "pure-button"
                                ++ " pure-button-primary"
                            , SaveComponent ( name, unit )
                            )

                        Delete ->
                            ( "Delete"
                            , "pure-button"
                                ++ " pure-button-primary"
                            , DeleteComponent name
                            )

                        NoOp ->
                            ( " "
                            , "pure-button"
                                ++ " pure-button-primary"
                            , Cancel
                            )
            in
            button
                [ type_ "submit"
                , class buttonStyle
                , onClick buttonMsg
                ]
                [ text buttonName ]

        Disabled ->
            let
                buttonName =
                    case action of
                        Save ->
                            "Save"

                        Delete ->
                            "Delete"

                        NoOp ->
                            " "
            in
            button
                [ type_ "submit"
                , class <|
                    "pure-button"
                        ++ " pure-button-primary"
                , disabled True
                ]
                [ text buttonName ]


viewCancelButton : Html Msg
viewCancelButton =
    button
        [ class "pure-button button-secondary"
        , onClick Cancel
        ]
        [ text "Cancel" ]



-- MAP


unitToString : Unit -> String
unitToString unit =
    case unit of
        Liter _ ->
            "Liter"

        Gram _ ->
            "Gram"

        Kilowatt _ ->
            "Kilowatt"

        Meter _ ->
            "Meter"


isValidName : String -> Bool
isValidName name =
    not <|
        String.isEmpty <|
            String.trim
                name
