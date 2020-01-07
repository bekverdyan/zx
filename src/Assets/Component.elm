module Assets.Component exposing
    ( Model
    , Msg
    , decoder
    , encode
    ,  update
       -- , view

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
    , readyToSave : ( Bool, Bool )
    }


type Mode
    = Normal
    | NameEdit ( String, ReadyToSave )
    | UnitEdit ( Maybe Unit, ReadyToSave )
    | MultiEdit ( RowComponent, ReadyToSave )


type ReadyToSave
    = Yes
    | No


type alias RowComponent =
    { name : String
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



-- decoder : D.Decoder (Maybe Model)
-- decoder =
--     case decodeComponent of
--         Just component ->
--             Model component ""


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
    = InputName String
    | SelectUnit (Maybe Unit)
    | SaveComponent ( Name, Unit )
    | NewComponent


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        InputName editable ->
            let
                readyToSave =
                    ( isValidName editable
                    , Tuple.second model.readyToSave
                    )
            in
            ( { model
                | readyToSave = readyToSave
                , mode = NameEdit ( editable, No )
              }
            , False
            )

        SelectUnit selected ->
            let
                readyToSave =
                    ( Tuple.first model.readyToSave
                    , case selected of
                        Just unit ->
                            True

                        Nothing ->
                            False
                    )
            in
            ( { model
                | mode = UnitEdit ( selected, No )
                , readyToSave = readyToSave
              }
            , False
            )

        SaveComponent ( name, unit ) ->
            ( { model
                | component =
                    ( name
                    , unit
                    )
              }
            , True
            )

        NewComponent ->
            ( model, False )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ class "pure-form" ]
            [ fieldset []
                [ legend []
                    [ text "Create new Component" ]
                ]
            , input
                [ type_ "text"
                , placeholder "Component name"

                -- , value model.typedName
                , onInput InputName
                ]
                []
            , viewUnit <| Tuple.second model.component
            , case model.mode of
                MultiEdit component ->
                    if saveAllowed model.readyToSave then
                        viewCreateButton <|
                            Functional
                                ( component.name
                                , Maybe.withDefault 0 component.unit
                                )

                    else
                        viewCreateButton Disabled

                _ ->
                    viewCreateButton Disabled

            -- , case model.selectedUnit of
            --     Just unit ->
            --         if isValidName model.typedName then
            --             viewCreateButton <|
            --                 Functional
            --                     ( model.typedName
            --                     , unit
            --                     )
            --
            --         else
            --             viewCreateButton Disabled
            --
            --     _ ->
            --         viewCreateButton Disabled
            ]
        ]


saveAllowed : ( Bool, Bool ) -> Bool
saveAllowed ( name, unit ) =
    False


type ButtonState
    = Functional ( Name, Unit )
    | Disabled


viewCreateButton : ButtonState -> Html Msg
viewCreateButton state =
    case state of
        Functional ( name, unit ) ->
            button
                [ type_ "submit"
                , class <|
                    "pure-button"
                        ++ " pure-button-primary"
                , onClick <|
                    SaveComponent ( name, unit )
                ]
                [ text "Create" ]

        Disabled ->
            button
                [ type_ "submit"
                , class <|
                    "pure-button"
                        ++ " pure-button-primary"
                , disabled True
                ]
                [ text "Create" ]



-- viewComponent


viewUnit : Maybe Unit -> Mode -> Html Msg
viewUnit selected mode =
    case mode of
        UnitEdit unit ->
            viewUnitEditMode unit

        _ ->
            viewUnitNormalMode selected


viewUnitNormalMode : Maybe Unit -> Html Msg
viewUnitNormalMode selected =
    li []
        [ label []
            [ case selected of
                Just unit ->
                    text <| unitToString unit

                Nothing ->
                    text ""
            ]
        ]


viewUnitEditMode : Maybe Unit -> Html Msg
viewUnitEditMode selected =
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
                        [ onClick <| SelectUnit Nothing
                        , Aria.ariaSelected "true"
                        ]
                        []
                        :: units
    in
    li []
        [ select [ id "units" ] options ]


viewUnitMember : Unit -> Maybe Unit -> Html Msg
viewUnitMember member selected =
    let
        attributes =
            [ onClick <| SelectUnit <| Just <| Liter 0 ]
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
