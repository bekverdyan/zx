module Assets.NewComponent exposing
    ( Model
    , Msg
    , update
    , view
    )

import Assets.Component as Component
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Model
    = Closed
    | Open ( Name, Maybe Unit )
    | Created ( Name, Unit )


type alias Name =
    Component.Name


type alias Unit =
    Component.Unit



-- UPDATE


type Msg
    = EditComponent
    | InputName Name
    | SelectUnit Unit
    | SaveComponent ( Name, Unit )
    | Cancel


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        EditComponent ->
            ( Open ( "", Nothing ), False )

        InputName editable ->
            case model of
                Open ( _, unit ) ->
                    ( Open ( editable, unit ), False )

                _ ->
                    ( Closed, False )

        SelectUnit selected ->
            case model of
                Open ( name, _ ) ->
                    ( Open ( name, Just selected ), False )

                _ ->
                    ( Closed, False )

        SaveComponent ( name, unit ) ->
            ( Created ( name, unit ), True )

        Cancel ->
            ( Closed, False )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
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
                    ]
                    [ text "Add component" ]
                ]


viewFieldset : Name -> Maybe Unit -> Html Msg
viewFieldset editable selected =
    fieldset []
        [ Component.viewNameInput
            InputName
            editable
        , Component.viewUnitDropdown
            SelectUnit
            selected
        , viewMutatorButton
            (case selected of
                Just unit ->
                    if Component.isValidName editable then
                        Functional
                            ( editable, unit )

                    else
                        Disabled

                Nothing ->
                    Disabled
            )
        , Component.viewCancelButton Cancel
        ]


type ButtonState
    = Functional ( Name, Unit )
    | Disabled


viewMutatorButton : ButtonState -> Html Msg
viewMutatorButton state =
    case state of
        Functional ( name, unit ) ->
            button
                [ type_ "submit"
                , class <|
                    "pure-button"
                        ++ " pure-button-primary"
                , onClick <| SaveComponent ( name, unit )
                ]
                [ text "Save" ]

        Disabled ->
            button
                [ type_ "submit"
                , class <|
                    "pure-button"
                        ++ " pure-button-primary"
                , disabled True
                ]
                [ text "Save" ]
