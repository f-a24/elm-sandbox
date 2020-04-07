module Chapter3.Main exposing (add, main, output)

import Browser
import Html exposing (Html, a, button, div, form, h1, input, li, text, ul)
import Html.Attributes exposing (disabled, href, value)
import Html.Events exposing (onClick, onInput, onSubmit)

-- １行コメント
{-
   複数行コメント
-}

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


--- MODEL

type alias Model =
    { count : Int
    , input : String
    , memos : List String
    }

init : Model
init =
    { count = 0, input = "", memos = [] }

--- UPDATE

type Msg
    = Increment
    | Decrement
    | Input String
    | Submit

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        Input input ->
            { model | input = input }

        Submit ->
            { model
                | input = ""
                , memos = model.input :: model.memos
            }


--- VIEW

view : Model -> Html Msg
view model =
    div []
        [ header
        , content
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        , form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button [ disabled (String.length model.input < 1) ] [ text "Submit" ]
            ]
        , ul [] (List.map viewMemo model.memos)
        ]

header : Html msg
header =
    h1 [] [ text "Useful Links" ]

content : Html msg
content =
    ul []
        [ linkItem "https://elm-lang.org/" "Homepage"
        , linkItem "https://package.elm-lang.org/" "Packages"
        , linkItem "https://ellie-app.com/" "Playground"
        ]

linkItem : String -> String -> Html msg
linkItem url text_ =
    li [] [ a [ href url ] [ text text_ ] ]

viewMemo : String -> Html msg
viewMemo memo =
    li [] [ text memo ]

output : String
output =
    "1 + 1 = " ++ String.fromInt (add 1 1)

add : number -> number -> number
add a b =
    a + b
