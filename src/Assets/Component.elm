module Assets.Component exposing
    ( Component
    , Model
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


type Model
    = Defined ( Component, Mode )
    | New State


type State
    = Closed
    | Open ( Name, Maybe Unit )
    | Created ( Name, Unit )


type Mode
    = Normal
    | NameEdit Name
    | UnitEdit Unit
    | Remove Name


type alias Component =
    ( Name, Unit )


type alias Name =
    String


type Unit
    = Liter Float
    | Gram Float
    | Kilowatt Float
    | Meter Float


encode : Component -> E.Value
encode ( name, unit ) =
    let
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
    | ToRemoveMode
    | DeleteComponent Name
    | OpenComponentCreator


type Action
    = NoOp
    | Save
    | Delete


update : Msg -> Model -> ( Model, Action )
update msg model =
    case msg of
        ToNameEditMode ->
            case model of
                Defined ( component, mode ) ->
                    let
                        ( name, _ ) =
                            component
                    in
                    ( Defined ( component, NameEdit name )
                    , NoOp
                    )

                New mode ->
                    ( model, NoOp )

        InputName editable ->
            case model of
                Defined ( component, mode ) ->
                    let
                        ( name, _ ) =
                            component
                    in
                    ( Defined
                        ( component
                        , case mode of
                            Remove _ ->
                                Remove editable

                            _ ->
                                NameEdit editable
                        )
                    , NoOp
                    )

                New mode ->
                    case mode of
                        Open ( name, unit ) ->
                            ( New <|
                                Open ( editable, unit )
                            , NoOp
                            )

                        _ ->
                            ( model, NoOp )

        ToUnitEditMode ->
            case model of
                Defined ( component, mode ) ->
                    let
                        ( _, unit ) =
                            component
                    in
                    ( Defined
                        ( component, UnitEdit unit )
                    , NoOp
                    )

                New mode ->
                    ( model, NoOp )

        SelectUnit selected ->
            case model of
                Defined ( component, mode ) ->
                    ( Defined
                        ( component, UnitEdit selected )
                    , NoOp
                    )

                New mode ->
                    case mode of
                        Open ( name, _ ) ->
                            ( New <|
                                Open
                                    ( name
                                    , Just selected
                                    )
                            , NoOp
                            )

                        _ ->
                            ( model, NoOp )

        SaveComponent ( name, unit ) ->
            case model of
                Defined _ ->
                    let
                        component =
                            ( name, unit )
                    in
                    ( Defined <|
                        ( component, Normal )
                    , Save
                    )

                New _ ->
                    ( New <|
                        Created ( name, unit )
                    , Save
                    )

        Cancel ->
            case model of
                Defined ( component, _ ) ->
                    ( Defined <|
                        ( component, Normal )
                    , NoOp
                    )

                New _ ->
                    ( New Closed, NoOp )

        ToRemoveMode ->
            case model of
                Defined ( ( name, unit ), _ ) ->
                    ( Defined
                        ( ( name, unit ), Remove name )
                    , NoOp
                    )

                _ ->
                    ( model, NoOp )

        DeleteComponent name ->
            case model of
                Defined ( component, _ ) ->
                    ( Defined <|
                        ( component, Normal )
                    , Delete
                    )

                New _ ->
                    ( model, NoOp )

        OpenComponentCreator ->
            case model of
                New _ ->
                    ( New <|
                        Open ( "", Nothing )
                    , NoOp
                    )

                _ ->
                    ( model, NoOp )



-- VIEW UNIT


view : Model -> Html Msg
view model =
    case model of
        Defined ( component, mode ) ->
            viewDefined ( component, mode )

        New mode ->
            viewNew mode


viewNew : State -> Html Msg
viewNew mode =
    case mode of
        Open ( editable, selected ) ->
            div []
                [ Html.form
                    [ class "pure-form" ]
                    [ viewFieldset editable selected ]
                ]

        _ ->
            div []
                [ button
                    [ class <|
                        "pure-button"
                            ++ " pure-button-warning"
                    , onClick OpenComponentCreator
                    ]
                    [ text "Add component" ]
                ]


viewFieldset : Name -> Maybe Unit -> Html Msg
viewFieldset editable selected =
    fieldset []
        [ viewNameInput
            InputName
            editable
        , viewUnitDropdown
            SelectUnit
            selected
        , viewSaveButton
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
        , viewCancelButton Cancel
        ]


viewDefined : ( Component, Mode ) -> Html Msg
viewDefined ( ( name, unit ), mode ) =
    case mode of
        Remove editable ->
            Html.form
                [ class "pure-form" ]
                [ viewNameInput InputName editable
                , viewDeleteButton <|
                    if editable == name then
                        Functional ( editable, unit )

                    else
                        Disabled
                , viewCancelButton Cancel
                ]

        _ ->
            li [] <|
                List.append
                    [ viewName name mode unit
                    , viewUnit unit name mode
                    ]
                <|
                    case mode of
                        Normal ->
                            [ button
                                [ class <|
                                    "pure-button"
                                        ++ " button-secondary"
                                , onClick ToRemoveMode
                                ]
                                [ text "Remove" ]
                            ]

                        _ ->
                            []


viewName : Name -> Mode -> Unit -> Html Msg
viewName name mode unit =
    case mode of
        NameEdit editable ->
            viewNameEditMode name unit

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


viewNameInput : (Name -> msg) -> Name -> Html msg
viewNameInput inputMsg editable =
    input
        [ id "name"
        , placeholder "Component Name"
        , onInput inputMsg
        , value editable
        ]
        []


viewNameEditMode : Name -> Unit -> Html Msg
viewNameEditMode editable unit =
    Html.form
        [ class "pure-form" ]
        [ viewNameInput InputName editable
        , viewSaveButton <|
            if isValidName editable then
                Functional ( editable, unit )

            else
                Disabled
        , viewCancelButton Cancel
        ]


viewUnit : Unit -> Name -> Mode -> Html Msg
viewUnit selected name mode =
    case mode of
        UnitEdit unit ->
            viewUnitEditMode selected name

        _ ->
            viewUnitNormalMode selected


viewUnitNormalMode : Unit -> Html Msg
viewUnitNormalMode selected =
    li []
        [ label []
            [ a
                [ class "pure-button"
                , href "#"
                , onClick ToUnitEditMode
                ]
                [ text <| unitToString selected
                ]
            ]
        ]


viewUnitDropdown : (Unit -> msg) -> Maybe Unit -> Html msg
viewUnitDropdown selectMsg selected =
    let
        units =
            [ viewUnitMember selectMsg (Liter 0) selected
            , viewUnitMember selectMsg (Gram 0) selected
            , viewUnitMember selectMsg (Kilowatt 0) selected
            , viewUnitMember selectMsg (Meter 0) selected
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


viewUnitEditMode : Unit -> Name -> Html Msg
viewUnitEditMode selected name =
    li []
        [ viewUnitDropdown SelectUnit <| Just selected
        , viewSaveButton <|
            Functional ( name, selected )
        , viewCancelButton Cancel
        ]


viewUnitMember :
    (Unit -> msg)
    -> Unit
    -> Maybe Unit
    -> Html msg
viewUnitMember selectMsg member selected =
    let
        attributes =
            [ onClick <| selectMsg <| member ]
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


viewDeleteButton : ButtonState -> Html Msg
viewDeleteButton state =
    button
        (type_ "submit"
            :: (case state of
                    Functional ( name, _ ) ->
                        [ class <|
                            "pure-button"
                                ++ " pure-button-primary"
                        , onClick <|
                            DeleteComponent name
                        ]

                    Disabled ->
                        [ class <|
                            "pure-button"
                                ++ " pure-button-primary"
                        , disabled True
                        ]
               )
        )
        [ text "Delete" ]


viewSaveButton : ButtonState -> Html Msg
viewSaveButton state =
    button
        (type_ "submit"
            :: (case state of
                    Functional ( name, unit ) ->
                        [ class <|
                            "pure-button"
                                ++ " pure-button-primary"
                        , onClick <|
                            SaveComponent ( name, unit )
                        ]

                    Disabled ->
                        [ class <|
                            "pure-button"
                                ++ " pure-button-primary"
                        , disabled True
                        ]
               )
        )
        [ text "Save" ]


viewCancelButton : msg -> Html msg
viewCancelButton cancel =
    button
        [ class "pure-button button-secondary"
        , onClick cancel
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
