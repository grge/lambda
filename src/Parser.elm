module Parser exposing (..)

import Lambda exposing (Term(VarTerm, ApplicationTerm, LambdaTerm), Application, Lambda, Var)
import Tokenizer exposing (Token)

import Regex

type alias ParseError = { position : Int, message : String }

type alias ParseResult = Result ParseError (Term, List Token)

parseErr ix text = Err <| ParseError ix text
parseOk term rest = Ok (term, rest)

parseVarTerm : List Token -> ParseResult
parseVarTerm tokens =
    case tokens of
        token :: rest ->
            let 
                rx = Regex.regex "^(\\w|\\')+$"
            in
                if Regex.contains rx token.string then
                    parseOk (VarTerm (Var token.string)) rest
                else
                    parseErr token.index ("Bad variable name (" ++ token.string ++ ")")
        [] ->
            parseErr 0 "Couldn't parse VarTerm from no tokens"

parseSimpleTerm : List Token -> ParseResult
parseSimpleTerm tokens =
    case tokens of
        token :: rest ->
            case token.string of
                "λ" -> parseLambdaTerm tokens
                "\\" -> parseLambdaTerm tokens
                "(" -> 
                    case rest of
                        [] -> parseErr token.index "Unexpected end after ("
                        anything -> parseTerm anything
                "." -> parseErr token.index "Unexpected '.'"
                ")" -> parseErr token.index "Unexpected ')'"
                _ -> parseVarTerm tokens
        [] ->
            parseErr 0 "Couldn't parse simple Term from no tokens"


parseLambdaTerm : List Token -> ParseResult
parseLambdaTerm tokens =
    case tokens of
        lambda :: bind :: dot :: body ->
            if (lambda.string == "λ" || lambda.string == "\\") && dot.string == "." then
                case parseVarTerm [bind] of
                    Ok (bindTerm, _) ->
                       case parseTerm body of
                           Ok (bodyTerm, rest) ->
                               parseOk (LambdaTerm (Lambda (Var bind.string) bodyTerm)) rest
                           Err err ->
                               Err err
                    Err err ->
                        Err err
            else
                parseErr lambda.index "LambdaTerm does not begin with 'λ' or '\\'"

        lambda :: _ ->
            parseErr lambda.index "LambdaTerm is not of form 'λx.y'"
        [] ->
            parseErr 0 "Can't parse LambdaTerm out of no tokens"
        

completeApplication : Term -> List Token -> ParseResult
completeApplication term tokens =
    case parseTerm tokens of
        Ok (argument, rest)->
            parseOk (ApplicationTerm (Application term argument)) rest
        Err err ->
            Err err


parseTerm : List Token -> ParseResult
parseTerm tokens =
    case parseSimpleTerm tokens of
        Ok (term, rest) ->
            case rest of
                car :: cdr ->
                    if car.string == ")" then
                        parseOk term cdr
                    else
                        completeApplication term rest
                [] ->
                    parseOk term []
        Err err ->
            Err err

