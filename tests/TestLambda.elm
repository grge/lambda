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
            Expect.equal (LambdaTerm (Lambda (Var "y") (VarTerm (Var "z"))))
                         (substitute (Var "x")
                                     (VarTerm (Var "a"))
                                     (LambdaTerm (Lambda (Var "y") (VarTerm (Var "z")))))
    , test "Substitute into LambdaTerm with bind match" <|
        \_ ->
            Expect.equal (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y"))))
                         (substitute (Var "x")
                                     (VarTerm (Var "y"))
                                     (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y")))))
    , test "Substitute into LambdaTerm with body match" <|
        \_ ->
            Expect.equal (LambdaTerm (Lambda (Var "x") (VarTerm (Var "a"))))
                         (substitute (Var "y")
                                     (VarTerm (Var "a"))
                                     (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y")))))
    , test "Substitute into LambdaTerm with body match and avoid capturing" <|
        \_ ->
            Expect.equal (LambdaTerm (Lambda (Var "x'") (VarTerm (Var "x"))))
                         (substitute (Var "y")
                                     (VarTerm (Var "x"))
                                     (LambdaTerm (Lambda (Var "x") (VarTerm (Var "y")))))
    , test "Substitute into deep LambdaTerm" <|
        \_ ->
            Expect.equal (LambdaTerm { bind = Var "x"
                                     , body = (LambdaTerm { bind = Var "y"
                                                          , body = VarTerm (Var "a")})})
                         (substitute (Var "z")
                                     (VarTerm (Var "a"))
                                     (LambdaTerm { bind = Var "x"
                                                 , body = (LambdaTerm { bind = Var "y"
                                                                      , body = VarTerm (Var "z")})}))
    , test "Substitute into deep LambdaTerm with matching bind" <|
        \_ ->
            Expect.equal (LambdaTerm { bind = Var "x"
                                     , body = (LambdaTerm { bind = Var "y"
                                                          , body = VarTerm (Var "z")})})
                         (substitute (Var "y")
                                     (VarTerm (Var "z"))
                                     (LambdaTerm { bind = Var "x"
                                                 , body = (LambdaTerm { bind = Var "y"
                                                                      , body = VarTerm (Var "z")})}))
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
        
test_betaReduce : Test
test_betaReduce = describe "Test the betaReduce function"
    [ test "Test simple application betaReduce" <|
        \_ ->
            Expect.equal (VarTerm (Var "y"))
                         (betaReduce 
                            { lambda = LambdaTerm
                                            { bind = Var "x"
                                            , body = VarTerm (Var "x")}
                            , argument = VarTerm (Var "y")})
    , test "Test nested lambda" <|
        \_ ->
            Expect.equal (LambdaTerm { bind = Var "y", body = VarTerm (Var "a")})
                         (betaReduce 
                            { lambda = LambdaTerm
                                            { bind = Var "x"
                                            , body = LambdaTerm { bind = Var "y"
                                                                , body = VarTerm (Var "x")}}
                            , argument = VarTerm (Var "a")})
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
