module TestParser exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Parser exposing (parseVarTerm, ParseError, ParseResult)
import Tokenizer exposing (Token)
import Lambda exposing (Term(VarTerm), Var)

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


-- test_parseSimpleTerm = describe "Test parsing a 'simple' Term." <|
--     [ test "Test parsing a word" <|
--         \_ -> Expect.equal (Ok (VarTerm "word"))
--                          (parseVarTerm (Token 0 "word") [])
--     , test "Test parsing an empty string" <|
--         \_ -> 
--             Expect.equal (Err (ParseError 0 "Bad variable name ()")) 
--                          (parseVarTerm (Token 0 "") [])
--     , test "Test parsing an variable containing lambda" <|
--         \_ -> 
--             Expect.equal (Err (ParseError 0 "Bad variable name (λasdf)")) 
--                          (parseVarTerm (Token 0 "λasdf") [])
--     ]
