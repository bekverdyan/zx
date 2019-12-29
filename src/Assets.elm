module Assets exposing
    ( Component
    , Model
    , Msg
    , decodeComponent
    , encodeComponent
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


type Model
    = Component ComponentModel


type alias ComponentModel =
    { component : Component
    , typedName : String
    , selectedUnit : Maybe Unit
    , validName : Bool
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


encodeComponent : Component -> E.Value
encodeComponent ( name, unit ) =
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


decodeComponent : D.Decoder (Maybe Component)
decodeComponent =
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
    | SelectLiter Unit
    | SelectGram Unit
    | SelectKilowatt Unit
    | SelectMeter Unit
    | CreateComponent


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        InputName editable ->
            case model of
                Component componentModel ->
                    let
                        validName =
                            isValidName
                                componentModel.typedName
                    in
                    ( Component
                        { componentModel
                            | typedName = editable
                            , validName = validName
                        }
                    , False
                    )

        SelectLiter liter ->
            case model of
                Component componentModel ->
                    ( Component
                        { componentModel
                            | selectedUnit = Just liter
                        }
                    , False
                    )

        SelectGram gram ->
            case model of
                Component componentModel ->
                    ( Component
                        { componentModel
                            | selectedUnit = Just gram
                        }
                    , False
                    )

        SelectKilowatt kilowatt ->
            case model of
                Component compnentModel ->
                    ( Component
                        { compnentModel
                            | selectedUnit = Just kilowatt
                        }
                    , False
                    )

        SelectMeter meter ->
            case model of
                Component componentModel ->
                    ( Component
                        { componentModel
                            | selectedUnit = Just meter
                        }
                    , False
                    )

        CreateComponent ->
            case model of
                Component componentModel ->
                    let
                        editable =
                            componentModel.typedName
                    in
                    case componentModel.selectedUnit of
                        Just unit ->
                            ( Component
                                { componentModel
                                    | component =
                                        ( editable
                                        , unit
                                        )
                                }
                            , True
                            )

                        _ ->
                            ( model, False )


isValidName : String -> Bool
isValidName name =
    not <|
        String.isEmpty <|
            String.trim
                name



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Component componentModel ->
            div []
                [ Html.form [ class "pure-form" ]
                    [ fieldset []
                        [ legend []
                            [ text "Create new Component" ]
                        ]
                    , input
                        [ type_ "text"
                        , placeholder "Component name"
                        , value componentModel.typedName
                        , onInput InputName
                        ]
                        []
                    , select [ id "state" ]
                        [ option
                            [ onClick <|
                                SelectLiter <|
                                    Liter 0
                            ]
                            [ text "Liter" ]
                        , option
                            [ onClick <|
                                SelectGram <|
                                    Gram 0
                            ]
                            [ text "Gram" ]
                        , option
                            [ onClick <|
                                SelectKilowatt <|
                                    Kilowatt 0
                            ]
                            [ text "Kilowatt" ]
                        , option
                            [ onClick <|
                                SelectMeter <|
                                    Meter 0
                            ]
                            [ text "Meter" ]
                        ]
                    , case componentModel.selectedUnit of
                        Just unit ->
                            if isValidName componentModel.typedName then
                                viewCreateButton Functional

                            else
                                viewCreateButton Disabled

                        _ ->
                            viewCreateButton Disabled
                    ]
                ]


type ButtonState
    = Functional
    | Disabled


viewCreateButton : ButtonState -> Html Msg
viewCreateButton state =
    case state of
        Functional ->
            button
                [ type_ "submit"
                , class <|
                    "pure-button"
                        ++ " pure-button-primary"
                , onClick CreateComponent
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
