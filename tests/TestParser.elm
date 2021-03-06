module TestParser exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Parser exposing (parseVarTerm, parseLambdaTerm, parseTerm, ParseError, ParseResult)
import Tokenizer exposing (Token, tokenize)
import Lambda exposing (Term(..), Var, Lambda, toText, reduce)

type alias ParseResult = Result ParseError (Term, List Token)

test_parseVarTerm = describe "Test parsing a VarTerm" <|
    [ test "Test parsing a word" <|
        \_ -> Expect.equal (Ok (VarTerm (Var "word"), []))
                           (parseVarTerm [(Token 0 "word")])
    , test "Test parsing an empty string" <|
        \_ -> 
            Expect.equal (Err (ParseError 0 "Bad variable name ()")) 
                         (parseVarTerm [(Token 0 "")])
    , test "Test parsing an variable containing lambda" <|
        \_ -> 
            Expect.equal (Err (ParseError 0 "Bad variable name (λasdf)")) 
                         (parseVarTerm [(Token 0 "λasdf")])
    ]


test_parseLambdaTerm = describe "Test parsing a LambdaTerm." <|
    [ test "Test parsing a lambda using parseSimpleTerm" <|
        \_ -> Expect.equal (Ok (LambdaTerm (Lambda (Var "x") (VarTerm (Var "x"))), []))
                         (parseLambdaTerm [(Token 0 "\\")
                                          ,(Token 1 "x")
                                          ,(Token 2 ".")
                                          ,(Token 3 "x")
                                          ])
   ]

test_self_eq str = 
    case parseTerm <| tokenize str of
        Ok (result, tokens) -> test str <| \_ -> Expect.equal str (toText result)
        Err err -> test str <| \_ -> Expect.equal err.message str

test_testParser = describe "End-to-end tests of the parser." <|
    List.map test_self_eq [
        "x", 
        "(a)(b)",
        "((a)(b))(c)",
        "(a)((b)(c))",
        "λx.x",
        "λx.λy.x",
        "(λx.x)(a)",
        "(λx.λy.x)(a)"
        ]

test_reduce text_in text_out = 
    case parseTerm <| tokenize text_in of
        Ok (result, tokens) -> test text_in <| \_ -> Expect.equal text_out (toText <| reduce result)
        Err err -> test text_in <| \_ -> Expect.equal err.message text_out

test_end_to_end_reduce = describe "End-to-end tests of the parser and reducer." <|
    [ test_reduce "x" "x"
    , test_reduce "λx.x" "λx.x"
    , test_reduce "(λx.x)a" "a"
    , test_reduce "(λx.λy.x)a" "λy.a"
    , test_reduce "((λx.λy.x)a)b" "a"
    ]

