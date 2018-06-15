module Tokenizer exposing (tokenize, Token)

import Regex

type alias Token = {index : Int, string : String}

makeToken : Regex.Match -> Token
makeToken match = Token match.index match.match

tokenize : String -> List Token
tokenize str =
    let rx = Regex.regex "[.\\\\\\λ\\(\\)]|((\\w|\\')+)" in
        List.map makeToken <| Regex.find Regex.All rx str
