module Chapter5.Page.User exposing (Model, Msg, init, update, view)

import Chapter5.GitHub as GitHub exposing (Repo)
import Html exposing (Html, a, li, ul, text)
import Html.Attributes exposing (href)
import Http
import Url.Builder


-- MODEL

type alias Model = State

type State
    = Init
    | Loaded (List Repo)
    | Error Http.Error

init : String -> ( Model, Cmd Msg )
init userName =
    ( Init
    , GitHub.getRepos
        GotRepos
        userName
    )


-- UPDATE

type Msg
    = GotRepos (Result Http.Error (List Repo))

update : Msg -> ( Model, Cmd Msg )
update msg =
    case msg of
        GotRepos (Ok repos) ->
            ( Loaded repos, Cmd.none )

        GotRepos (Err err) ->
            ( Error err, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Init ->
            text "Loading..."

        Loaded repos ->
            ul []
                (repos
                    |> List.map
                        (\repo ->
                            viewLink (Url.Builder.absolute [ repo.owner, repo.name ] [])
                        )
                )

        Error e ->
            text (Debug.toString e)

viewLink : String -> Html Msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
