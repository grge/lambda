module Tokenizer exposing (tokenize, Token)

import Regex

type alias Token = {index : Int, string : String}

makeToken : Regex.Match -> Token
makeToken match = Token match.index match.match

tokenize : String -> List Token
tokenize str =
    let
        rx = Regex.fromString "[.\\\\\\λ\\(\\)]|((\\w|\\')+)" |> Maybe.withDefault Regex.never
    in
        List.map makeToken <| Regex.find rx str
