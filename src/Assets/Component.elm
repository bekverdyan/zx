module Assets.Component exposing
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


type alias Model =
    { component : Component
    , mode : Mode

    -- , typedName : String
    -- , selectedUnit : Maybe Unit
    -- , validName : Bool
    }


type Mode
    = Normal
    | NameEdit String
    | UnitEdit (Maybe Unit)
    | NewComponent RowComponent


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
    encodeComponent model.component


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



-- decoder : D.Decoder (Maybe Model)
-- decoder =
--     case decodeComponent of
--         Just component ->
--             Model component ""


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
    | CreateComponent ( Name, Unit )


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        InputName editable ->
            let
                validName =
                    isValidName
                        model.typedName
            in
            ( { model
                | typedName = editable
                , validName = validName
              }
            , False
            )

        SelectLiter liter ->
            ( { model
                | selectedUnit = Just liter
              }
            , False
            )

        SelectGram gram ->
            ( { model
                | selectedUnit = Just gram
              }
            , False
            )

        SelectKilowatt kilowatt ->
            ( { model
                | selectedUnit = Just kilowatt
              }
            , False
            )

        SelectMeter meter ->
            ( { model
                | selectedUnit = Just meter
              }
            , False
            )

        CreateComponent ( name, unit ) ->
            ( { model
                | component =
                    ( name
                    , unit
                    )
              }
            , True
            )


isValidName : String -> Bool
isValidName name =
    not <|
        String.isEmpty <|
            String.trim
                name



-- VIEW


viewUnitSelection : Maybe Unit -> Html Msg
viewUnitselection unit =
  let

  in

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
                , value model.typedName
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
            , case model.selectedUnit of
                Just unit ->
                    if isValidName model.typedName then
                        viewCreateButton <|
                            Functional
                                ( model.typedName
                                , unit
                                )

                    else
                        viewCreateButton Disabled

                _ ->
                    viewCreateButton Disabled
            ]
        ]


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
                , onClick <| CreateComponent ( name, unit )
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
