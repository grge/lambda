module TestTokenizer exposing (test_tokenize)

import Test exposing (..)
import Expect exposing (Expectation)
import Tokenizer exposing (tokenize, Token)

type alias TestCase =
    {description: String, text: String, tokens: List String}

makeTest : TestCase -> Test
makeTest testCase =
    test testCase.description <|
        \_ ->
            Expect.equal testCase.tokens <| 
                tokensToStrings <| 
                    tokenize testCase.text

tokensToStrings : List Token -> List String
tokensToStrings tokens =
    List.map .string tokens

tokenizerTestCases : List TestCase
tokenizerTestCases = 
    [ TestCase "Simple var"
         "x" ["x"]
    , TestCase "Simple lambda with greek letter"
         "λvar1.var2" ["λ", "var1", ".", "var2"]
    , TestCase "Test tokenize nested lambda with greek letter"
         "λx.λy.y" ["λ", "x", ".", "λ", "y", ".", "y"]
    , TestCase "Test tokenize simple lambda with backslash"
         "\\var1.var2" ["\\", "var1", ".", "var2"] 
    , TestCase "Test tokenize quotes in var name" 
         "var''ia'ble''" ["var''ia'ble''"] 
    , TestCase "Test simple application" 
         "x y" ["x", "y"]
    , TestCase "Test parens" 
        "(x)" ["(", "x", ")"] 
    , TestCase "Test numerical vars"
        "0101" ["0101"]
    , TestCase "Test control chars are whitespace"
        "!@~$@*#x&*@`y%^+=" ["x", "y"]
    ]

test_tokenize : Test
test_tokenize = describe "Test the string tokenizer" <|
    List.map makeTest tokenizerTestCases

