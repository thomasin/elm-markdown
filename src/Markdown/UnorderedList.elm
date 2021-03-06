module Markdown.UnorderedList exposing (parser)

import Helpers
import Markdown.ListItem as ListItem exposing (ListItem)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser (List ListItem)
parser =
    let
        parseSubsequentItems listMarker firstItem =
            loop [] (statementsHelp listMarker firstItem)
    in
    succeed parseSubsequentItems
        |= backtrackable listMarkerParser
        |. oneOrMore Helpers.isSpaceOrTab
        |= ListItem.parser
        |> andThen identity


listMarkerParser : Parser (Token Parser.Problem)
listMarkerParser =
    Advanced.oneOf
        [ succeed Token.minus
            |. symbol Token.minus
        , succeed Token.plus
            |. symbol Token.plus
        , succeed Token.asterisk
            |. symbol Token.asterisk
        ]


singleItemParser : Token Parser.Problem -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. backtrackable (symbol listMarker)
        |= itemBody


itemBody : Parser ListItem
itemBody =
    oneOf
        [ succeed identity
            |. backtrackable (oneOrMore Helpers.isSpaceOrTab)
            |= ListItem.parser
        , succeed (ListItem.PlainItem "")
            |. Advanced.symbol Token.newline
        ]


statementsHelp : Token Parser.Problem -> ListItem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp listMarker firstItem revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= singleItemParser listMarker
        , succeed (Done (firstItem :: List.reverse revStmts))
        ]
