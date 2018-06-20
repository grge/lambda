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

test_substitute : Test
test_substitute = describe "Test the substitute function"
    [ test "Substitute into matching VarTerm" <|
        \_ ->
            Expect.equal (VarTerm (Var "x"))
                         (substitute (Var "y") (VarTerm (Var "x")) (VarTerm (Var "y")))
    , test "Substitute into non-matching VarTerm" <|
        \_ ->
            Expect.equal (VarTerm (Var "x"))
                         (substitute (Var "y") (VarTerm (Var "z")) (VarTerm (Var "x")))
    , test "Substitute into LambdaTerm with no match" <|
        \_ ->
            Expect.equal (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y"))))
                         (substitute (Var "z")
                                     (VarTerm (Var "a"))
                                     (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y"))))
                         )
    , test "Substitute into LambdaTerm with bind match" <|
        \_ ->
            Expect.equal (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y"))))
                         (substitute (Var "x")
                                     (VarTerm (Var "y"))
                                     (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y"))))
                         )
    , test "Substitute into LambdaTerm with body match" <|
        \_ ->
            Expect.equal (LambdaTerm (Lambda (Var "x") (VarTerm (Var "a"))))
                         (substitute (Var "y")
                                     (VarTerm (Var "a"))
                                     (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y"))))
                         )
    , test "Substitute into deep LambdaTerm (capture avoiding)" <|
        \_ ->
            Expect.equal (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y"))))
                         (substitute (Var "z")
Success! Compiled 1 module.
Successfully generated /dev/null
Success! Compiled 1 module.
Successfully generated /home/george/code/lambda/elm-stuff/generated-code/elm-community/elm-test/elmTestOutput.js

elm-test 0.18.12
----------------

Running 34 tests. To reproduce these results, run: elm-test --fuzz 100 --seed 26013454

↓ TestLambda
↓ Test the substitute function
✗ Substitute into deep LambdaTerm (capture avoiding)

                                   ▼            ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼                  ▼     ▼▼
    LambdaTerm { bind = { name = "x'" }, body = LambdaTerm { bind = { name = "y" }, body = VarTerm { name = "x" } } }
    ╷
    │ Expect.equal
    ╵
    LambdaTerm { bind = { name = "x" }, body = VarTerm { name = "y" } }
                                                                 ▲     



TEST RUN FAILED

Duration: 180 ms
Passed:   33
Failed:   1

                                     (LambdaTerm { bind = Var "x"
                                                 , body = (LambdaTerm { bind = Var "y"
                                                                      , body = VarTerm (Var "z")})})
                         )
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
        

test_getFreshVar : Test
test_getFreshVar = describe "test getFreshVar function" 
    [ test "Test against same Var" <|
       \_ -> Expect.equal (getFreshVar (Var "x") (VarTerm (Var "x"))) (Var "x'")
    , test "Test against different Var" <|
       \_ -> Expect.equal (getFreshVar (Var "x") (VarTerm (Var "y"))) (Var "x")
    , test "Test against lambda containing same Var" <|
       \_ -> Expect.equal (getFreshVar (Var "x") (LambdaTerm {
                               bind = Var "y",
                               body = VarTerm (Var "x")}))
                          (Var "x'")
    , test "Test against lambda containing different Var" <|
       \_ -> Expect.equal (getFreshVar (Var "x")
                                       (LambdaTerm {
                                           bind = Var "y",
                                           body = VarTerm (Var "z")}))
                           (Var "x")
    ] 
