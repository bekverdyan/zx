module Assets.Component exposing
    ( Model
    , Msg
    , Name
    , Unit
    , decoder
    , encode
    , isValidName
    , update
    , view
    , viewCancelButton
    , viewNameInput
    , viewUnitDropdown
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
            let
                ( _, unit ) =
                    model.component
            in
            ( { model
                | mode = UnitEdit unit
              }
            , NoOp
            )

        SelectUnit selected ->
            ( { model
                | mode = UnitEdit selected
              }
            , NoOp
            )

        SaveComponent ( name, unit ) ->
            ( { model
                | component = ( name, unit )
              }
            , Save
            )

        Cancel ->
            ( { model | mode = Normal }, NoOp )

        DeleteComponent name ->
            ( model, Delete )



-- VIEW UNIT


view : Model -> Html Msg
view model =
    let
        mode =
            model.mode

        ( name, unit ) =
            model.component
    in
    li []
        [ viewName name mode unit
        , viewUnit unit name mode
        ]


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
