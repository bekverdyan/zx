module Editor exposing (Model(..), Msg(..), update, view)

import Assets.Component as Component
import Branch as Branch
import Debug
import Device as Device
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Model
    = NotSelected
    | Branch Branch.Model
    | Device Device.Model
    | NotFound
    | Components ( Component.Model, Dict Component.Name Component.Model )


type alias Content =
    { device : Device.Model
    , branch : Branch
    }


type alias Branch =
    Branch.Branch


type alias Device =
    Device.Device


type Msg
    = DeviceMsg Device.Msg
    | BranchMsg Branch.Msg
    | ComponentMsg Component


type Component
    = Creator Component.Msg
    | Defined ( Component.Name, Component.Msg )


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        BranchMsg branchMsg ->
            case model of
                Branch branch ->
                    let
                        ( updated, saveMe ) =
                            Branch.update branchMsg branch
                    in
                    ( Branch updated, saveMe )

                _ ->
                    -- let
                    --     gag =
                    --         Debug.log "This should not happen" "!"
                    -- in
                    ( NotSelected, False )

        DeviceMsg deviceMsg ->
            case model of
                Device device ->
                    let
                        ( updated, saveMe ) =
                            Device.update deviceMsg device
                    in
                    ( Device updated, saveMe )

                _ ->
                    -- let
                    --     gag =
                    --         Debug.log "This should not happen" "!"
                    -- in
                    ( NotSelected, False )

        ComponentMsg componentMsg ->
            case model of
                Components ( creator, components ) ->
                    ( model, False )

                _ ->
                    ( model, False )


view : Model -> Html Msg
view model =
    case model of
        NotSelected ->
            div [ id "main" ] [ h1 [] [ text "Initial" ] ]

        NotFound ->
            div [ id "main" ] [ h1 [] [ text "Not found" ] ]

        Branch branch ->
            Html.map BranchMsg <|
                Branch.view branch

        Device viewModel ->
            Html.map DeviceMsg <|
                Device.view viewModel

        Components ( creator, components ) ->
            div []
                [ Html.map
                    (ComponentMsg << Creator)
                  <|
                    Component.view creator
                , viewComponents components
                ]


viewComponents :
    Dict Component.Name Component.Model
    -> Html Msg
viewComponents components =
    table [ class "pure-table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Unit" ]
                , th [] [ text "Delete" ]
                ]
            ]
        , tbody [] <|
            List.map mapComponentMsg <|
                Dict.toList components
        ]


mapComponentMsg : ( Component.Name, Component.Model ) -> Html Msg
mapComponentMsg ( name, model ) =
    let
        wrapMsg : Component.Msg -> Msg
        wrapMsg componentMsg =
            ComponentMsg <| Defined ( name, componentMsg )
    in
    Html.map wrapMsg <| Component.view model



-- INIT


initComponents :
    List Component.Component
    -> Dict.Dict Component.Name Component.Model
initComponents components =
    let
        toDictItem :
            Int
            -> Component.Component
            -> ( Component.Name, Component.Model )
        toDictItem index ( name, unit ) =
            ( name, Component.init index ( name, unit ) )
    in
    Dict.fromList <| List.indexedMap toDictItem components



-- MAP
-- componentsToViewList :
--     Dict Component.Name Component.Model
--     -> List ( Int, Component.Name, Component.Model )
-- componentsToViewList components =
--     List.indexedMap markIndex (Dict.toList components)
--
--
-- markIndex :
--     Int
--     -> ( Component.Name, Component.Model )
--     -> ( Int, Component.Name, Component.Model )
-- markIndex index ( name, model ) =
--     ( index, name, model )
