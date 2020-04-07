module Chapter4.UrlParser exposing (urlToRoute)

import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, top)
import Url.Parser.Query as Q

type Route
    = Top
    | Login
    | Articles (Maybe String)
    | Article Int
    | ArticleSetting Int

routeParser: Parser (Route -> a) a
routeParser =
    oneOf
        [ map Top top
        , map Login (s "login")
        , map Articles (s "articles" <?> Q.string "search")
        , map Article (s "articles" </> int)
        , map ArticleSetting (s "articles" </> int </> s "settings")
        ]

urlToRoute: Url -> Maybe Route
urlToRoute url =
    Url.Parser.parse routeParser url
