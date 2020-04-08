module Chapter5.SortableTable exposing (Config, Msg, Model, init, update, view)

import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Html.Events exposing (onClick)

--- MODEL
type alias Model =
    { sortColumn : Maybe String
    , reversed : Bool
    }

type Msg
    = ClickColumn String

init: Model
init = Model Nothing False


--- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickColumn columnName ->
            if model.sortColumn == Just columnName then
                { model | reversed = not model.reversed }

            else
                { model | sortColumn = Just columnName, reversed = False }


type alias Config a =
    { columns : List String
    , toValue : String -> a -> String
    }

--- VIEW
view : Config a -> Model -> List a -> Html Msg
view config model items =
    table []
        [ thead [] [ headerRow config model ]
        , tbody [] (List.map (bodyRow config) (sort config model items))
        ]

sort : Config a -> Model -> List a -> List a
sort config model items =
    case model.sortColumn of
        Just column ->
            List.sortBy (config.toValue column) items
                |> (if model.reversed then
                        List.reverse

                    else
                        identity
                   )

        Nothing ->
            items


headerRow : Config a -> Model -> Html Msg
headerRow config model =
    tr [] (
        List.map (
            \columnId ->
                let
                    label =
                        columnId
                            ++ (case ( model.sortColumn == Just columnId, model.reversed ) of
                                    ( True, False ) ->
                                        "(↓)"

                                    ( True, True ) ->
                                        "(↑)"

                                    _ ->
                                        ""
                                )
                in
                th [ onClick (ClickColumn columnId) ] [ text label ]
        ) config.columns)


bodyRow : Config a -> a -> Html msg
bodyRow config item =
    tr [] (
        List.map (
            \columnId -> td [] [ text (config.toValue columnId item) ]
        )
        config.columns
    )

