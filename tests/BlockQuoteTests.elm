module BlockQuoteTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (..)
import Markdown.Parser exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse =
    Markdown.Parser.parse


suite : Test
suite =
    describe "block quote continuation"
        [ test "setext heading after >" <|
            \() ->
                """> Foo
Foo
> ===
> Foo
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ BlockQuote
                                [ Heading H1 [Text "Foo\nFoo"]
                                , Paragraph [Text "Foo"]
                                ]
                            ]
                        )
        , test "setext line on continuation line" <|
            \() ->
                """> Foo
> Foo
===
> Foo
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ BlockQuote
                                [ Paragraph [Text "Foo\nFoo\n===\nFoo"]
                                ]
                            ]
                        )
        , test "setext line & heading on continuation lines" <|
            \() ->
                """> Foo
Foo
===
> Foo
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ BlockQuote
                                [ Paragraph [Text "Foo\nFoo\n===\nFoo"]
                                ]
                            ]
                        )
        ]
