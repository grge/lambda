module TestParser exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Parser exposing (parseVarTerm, parseSimpleTerm, parseTerm, ParseError, ParseResult)
import Tokenizer exposing (Token, tokenize)
import Lambda exposing (Term(..), Var, Lambda, toText)

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


test_parseSimpleTerm = describe "Test parsing a 'simple' Term." <|
    [ test "Test parsing a word using parseSimpleTerm" <|
        \_ -> Expect.equal (Ok (VarTerm (Var "word"), []))
                         (parseSimpleTerm [(Token 0 "word")])
    , test "Test parsing a lambda using parseSimpleTerm" <|
        \_ -> Expect.equal (Ok (LambdaTerm (Lambda (Var "x") (VarTerm (Var "x"))), []))
                         (parseSimpleTerm [(Token 0 "\\")
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
        "λx.x",
        "λx.λy.x",
        "(λx.λy.x) b"
        ]

