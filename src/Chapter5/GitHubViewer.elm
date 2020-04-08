module Chapter5.GitHubViewer exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, h1, text)
import Html.Attributes exposing (href)
import Chapter4.Route as Route exposing (Route)
import Url

import Chapter5.Page.Repo as PRepo
import Chapter5.Page.Top as PTop
import Chapter5.Page.User as PUser


--- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


--- MODEL

type alias Model =
    { key: Nav.Key
    , page: Page
    }

type Page
    = NotFound
    | TopPage PTop.Model
    | UserPage PUser.Model
    | RepoPage PRepo.Model

init: () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    Model key (TopPage PTop.init)
        |> goTo(Route.parse url)


--- UPDATE

type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | TopMsg PTop.Msg
    | RepoMsg PRepo.Msg
    | UserMsg PUser.Msg

update : Msg -> Model ->  ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->

                    ( model, Nav.load href )

        UrlChanged url ->
            goTo (Route.parse url) model

        TopMsg topMsg ->
            case model.page of
                TopPage topModel ->
                    let
                        ( newTopModel, topCmd ) =
                            PTop.update topMsg topModel
                    in
                    ( { model | page = TopPage newTopModel }
                    , Cmd.map TopMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )

        RepoMsg repoMsg ->
            case model.page of
                RepoPage repoModel ->
                    let
                        ( newRepoModel, topCmd ) =
                            PRepo.update repoMsg repoModel
                    in
                    ( { model | page = RepoPage newRepoModel }
                    , Cmd.map RepoMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )

        UserMsg userMsg ->
            case model.page of
                UserPage _ ->
                    let
                        ( newUserModel, topCmd ) =
                            PUser.update userMsg
                    in
                    ( { model | page = UserPage newUserModel }
                    , Cmd.map UserMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )

goTo: Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            ( { model | page = TopPage PTop.init }
            , Cmd.none
            )

        Just (Route.User userName) ->
            let
                ( userModel, userCmd ) =
                    PUser.init userName
            in
            ( { model | page = UserPage userModel }
            , Cmd.map UserMsg userCmd
            )

        Just (Route.Repo userName projectName) ->
            let
                ( repoModel, repoCmd ) =
                    PRepo.init userName projectName
            in
            ( { model | page = RepoPage repoModel }
            , Cmd.map RepoMsg repoCmd
            )


--- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


--- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "My GitHub View"
    , body =
        [ a [ href "/" ][ h1 [] [text "My GitHub Viewer" ] ]
        , case model.page of
            NotFound ->
                viewNotFound

            TopPage topPageModel ->
                PTop.view topPageModel
                    |> Html.map TopMsg

            UserPage userPageModel ->
                PUser.view userPageModel
                    |> Html.map UserMsg

            RepoPage repoPageModel ->
                PRepo.view repoPageModel
                    |> Html.map RepoMsg
        ]
    }

viewNotFound : Html Msg
viewNotFound =
    text "Not Found"
