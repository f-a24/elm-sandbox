module Chapter4.Route exposing (Route(..), parse)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, string, top)

type Route
    = Top
    | User String
    | Repo String String

parse : Url -> Maybe Route
parse url =
    Url.Parser.parse parser url

parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top top
        , map User string
        , map Repo (string </> string)]
