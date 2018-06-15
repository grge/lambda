module TestLambda exposing (..)

import Lambda exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


test_toText : Test
test_toText = describe "Tests for the toText function"
    [ test "Simple one character VarTerm" <|
        \_ -> 
            Expect.equal (toText <| VarTerm (Var "x")) "x" 
    , test "Long, multi character VarTerm" <|
        \_ -> 
            Expect.equal (toText <| VarTerm (Var "longer")) "longer" 
    ]


test_alphaConvert : Test
test_alphaConvert = describe "Test alphaConvert function"
    [ test "Convert plain simple lambda" <|
        \_ ->
            let
                arg = Lambda (Var "x") (VarTerm (Var "x"))
                new_var = Var "y"
                out = Lambda (Var "y") (VarTerm (Var "y"))
            in 
                Expect.equal (alphaConvert arg new_var) out
    ]
        
