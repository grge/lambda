module App exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Parser exposing (parseAll)
import Tokenizer exposing (tokenize)
import Lambda exposing (toText, reduce)

main = Browser.sandbox { init = init, view = view, update = update }

type alias Model = { input : String, output : String }

init : Model
init = { input = "", output = "" }


type Msg = Change String

evaluate : String -> String
evaluate source =
    case tokenize source of
        [] ->
            ""
        tokens ->
            case parseAll tokens of
                Ok term ->
                    case reduce term of
                        Ok normal -> toText normal
                        Err partial -> "Gave up: this may have no normal form. Reached:\n" ++ toText partial
                Err {message} ->
                    message

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | input = newContent, output = evaluate newContent }

view : Model -> Html Msg
view model =
    div []
      [ h1 [] [text "λ-CALCULUS"]
      , div [id "about"]
          [ p []
            [text "λ-calculus interpreter written in Elm — type a backslash \\ for λ"]
          ]
      , div [class "labeled-box"]
          [ div [class "label"] [text "INPUT:"]
          , input [id "input", placeholder "(\\x.\\y.x) a b", onInput Change, value model.input] []
          ]
      , div [id "parse-error-box"] []
      , div [class "labeled-box"]
          [ div [class "label"] [text "OUTPUT:"]
          , pre [id "output"] [text model.output]
          ]
      ]
