module TestParser exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Parser exposing (parseVarTerm, parseLambdaTerm, parseTerm, parseAll, ParseError)
import Tokenizer exposing (Token, tokenize)
import Lambda exposing (Term(..), Var, Lambda, toText, reduce, reduceWithin)

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
    case parseAll <| tokenize str of
        Ok result -> test str <| \_ -> Expect.equal str (toText result)
        Err err -> test str <| \_ -> Expect.equal err.message str

test_testParser = describe "End-to-end tests of the parser." <|
    List.map test_self_eq [
        "x",
        "a b",
        "a b c",
        "a (b c)",
        "λx.x",
        "λx.λy.x",
        "λx.x y",
        "(λx.x) a",
        "(λx.λy.x) a",
        "f (λx.x)"
        ]

test_rejects str = test str <| \_ ->
    case parseAll <| tokenize str of
        Ok result -> Expect.fail (toText result)
        Err _ -> Expect.pass

test_parse_errors = describe "Input that should not parse." <|
    List.map test_rejects ["x)", "(x", "()", "\\x", "\\.x", "."]

test_reduce text_in text_out =
    case parseAll <| tokenize text_in of
        Ok result -> test text_in <| \_ -> Expect.equal (Ok text_out) (Result.map toText <| reduce result)
        Err err -> test text_in <| \_ -> Expect.equal err.message text_out

test_end_to_end_reduce = describe "End-to-end tests of the parser and reducer." <|
    [ test_reduce "x" "x"
    , test_reduce "λx.x" "λx.x"
    , test_reduce "(λx.x)a" "a"
    , test_reduce "(λx.λy.x)a" "λy.a"
    , test_reduce "((λx.λy.x)a)b" "a"
    , test_reduce "(λx.λy.x) a b" "a"
    , test_reduce "(x y) z" "x y z"
    , test_reduce "x ((λy.y) z)" "x z"
    , test_reduce "(λx.λy.x) y" "λy'.y"
    , test_reduce "(λx.λy.x y) y'" "λy.y' y"
    , test_reduce "(λx.λy.λy'.x) y" "λy'.λy''.y"
    , test_reduce "(λx.y) ((λx.x x) (λx.x x))" "y"
    , test_reduce "(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f x)" "λf.λx.f (f (f x))"
    , test_reduce "(λm.λn.λf.m (n f)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))" "λf.λx.f (f (f (f (f (f x)))))"
    ]

gives_up str = test str <| \_ ->
    case parseAll <| tokenize str of
        Ok term ->
            case reduceWithin 200 term of
                Err _ -> Expect.pass
                Ok normal -> Expect.fail (toText normal)
        Err err ->
            Expect.fail err.message

test_gives_up = describe "Terms with no normal form stop instead of hanging." <|
    List.map gives_up
        [ "(λx.x x) (λx.x x)"
        , "(λx.x x x) (λx.x x x)"
        ]
