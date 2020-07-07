module Lexer where

import Data.List (genericLength)
import Data.Char

import Token

data State = State
    { currentLine :: Integer
    , currentColumn :: Integer
    , referenceIndent :: Maybe(Integer)
    , indentLevel :: Integer
    , tokens :: [Token]
    }

lex :: String -> [Token]
lex input =
    let initialState = State 1 1 Nothing 0 []
    in reverse . tokens $ lexerStep initialState input

lexerStep :: State -> String -> State
lexerStep finalState "" = finalState
lexerStep state input@(char:rest) =
    let result =
            if char == '=' then
                parseEquals state rest
            else if isLower char then
                parseValue state input
            else if isUpper char then
                parseDataType state input
            else if isDigit char then
                parseInteger state input
            else
                (state, "")
        (newState, newInput) = removeWhitespace result
    in lexerStep newState newInput

peek :: String -> String
peek "" = ""
peek (char:_) = [char]

parseEquals :: State -> String -> (State, String)
parseEquals state input =
    if peek input == "=" then
        let token = Token Equality (getPosition state)
        in
            ( state { currentColumn = currentColumn state + 2, tokens = token : tokens state }
            , tail input
            )
    else
        let token = Token Equals (getPosition state)
        in
            ( state { currentColumn = currentColumn state + 1, tokens = token : tokens state }
            , input
            )

parseValue state input =
    let (word, rest) = span isAlphaNum input
        tokenType = matchKeyword word
        token = Token tokenType (getPosition state)
    in
        ( state { currentColumn = currentColumn state + genericLength word, tokens = token : tokens state }
        , rest
        )

parseDataType state input =
    let (word, rest) = span isAlphaNum input
        token = Token (DataType word) (getPosition state)
    in
        ( state { currentColumn = currentColumn state + genericLength word, tokens = token : tokens state }
        , rest
        )

parseInteger state input =
    let (integer, rest) = span isDigit input
        token = Token (Integer $ read integer) (getPosition state)
    in
        ( state { currentColumn = currentColumn state + genericLength integer, tokens = token : tokens state }
        , rest
        )

removeWhitespace (state, input) =
    let (whitespace, rest) = span (== ' ') input
    in
        ( state { currentColumn = currentColumn state + genericLength whitespace }
        , rest
        )

getPosition :: State -> TokenPosition
getPosition state = TokenPosition (currentLine state) (currentColumn state)