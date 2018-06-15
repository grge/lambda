module App exposing (..)

import Json.Decode
import Regex exposing (regex, replace, HowMany(All))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode)

import Parser exposing (parseTerm)
import Tokenizer exposing (tokenize)
import Lambda exposing (toText, reduce)

main = Html.beginnerProgram { model = model, view = view, update = update }

type alias Model = { input : String, output : String }

model : Model
model = { input = "", output = "" }


type Msg = Change String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            let 
                result = parseTerm <| tokenize newContent
            in
                case result of
                    Ok (term, _) -> { model | output = toText <| reduce term }
                    Err {position, message} -> { model | output = message } 

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    on "keypress" (Json.Decode.map tagger keyCode)

view : Model -> Html Msg
view model =
    div []
      [ h1 [] [text "λ-CALCULUS"]
      , div [id "about"] 
          [ p [] 
            [text "λ-calculus interpreter written in javascript — backslash \\ inserts a lambda"]
          ]
      , div [class "labeled-box"] 
          [ div [class "label"] [text "INPUT:"]
          , input [id "input", placeholder "\\x.x", onInput Change] [text model.input]
          ]
      , div [id "parse-error-box"] []
      , div [class "labeled-box"] 
          [ div [class "label"] [text "OUTPUT:"]
          , pre [id "output"] [text model.output]
          ]
      ]
