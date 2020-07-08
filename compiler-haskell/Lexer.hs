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
            if char == '(' then
                parseChar state rest LeftParen
            else if char == ')' then
                if peek rest == "!" then
                    parseDoubleChar state rest PartialRightParen
                else
                    parseChar state rest RightParen
            else if char == '[' then
                parseChar state rest LeftBracket
            else if char == ']' then
                parseChar state rest RightBracket
            else if char == '<' then
                if peek rest == "|" then
                    parseDoubleChar state rest ReversePipe
                else if peek rest == "!" then
                    parseDoubleChar state rest ReversePartialPipe
                else
                    parseChar state rest LeftAngle
            else if char == '>' then
                parseChar state rest RightAngle
            else if char == '=' then
                if peek rest == "=" then
                    parseDoubleChar state rest Equality
                else
                    parseChar state rest Equals
            else if char == '|' then
                if peek rest == ">" then
                    parseDoubleChar state rest Pipe
                else if peek rest == "|" then
                    parseDoubleChar state rest Or
                else
                    parseChar state rest Union
            else if char == ',' then
                parseChar state rest Comma
            else if char == '.' then
                parseChar state rest Dot
            else if char == '/' && peek rest == "=" then
                parseDoubleChar state rest Inequality
            else if char == '+' then
                parseChar state rest Plus
            else if char == '-' then
                parseChar state rest Minus
            else if char == '%' then
                parseChar state rest Modulo
            else if char == '!' && peek rest == ">" then
                parseDoubleChar state rest PartialPipe
            else if char == '&' && peek rest == "&" then
                parseDoubleChar state rest And
            else if char == '"' then
                parseString state rest
            else if char == '\'' then
                parseCharLiteral state rest
            else if isDigit char then
                parseInteger state input
            else if isUpper char then
                parseDataType state input
            else if isLower char then
                parseValue state input
            else if char == '\n' then
                parseChar state rest Newline
            else
                (state, "")
        (newState, newInput) = removeWhitespace result
    in lexerStep newState newInput

peek :: String -> String
peek "" = ""
peek (char:_) = [char]

parseChar state input tokenType =
    let token = Token tokenType (getPosition state)
    in
        ( state { currentColumn = currentColumn state + 1, tokens = token : tokens state }
        , input
        )

parseDoubleChar state input tokenType =
    let token = Token tokenType (getPosition state)
    in
        ( state { currentColumn = currentColumn state + 2, tokens = token : tokens state }
        , tail input
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

parseString state input =
    let (string, rest) = break (== '"') input
        token = Token (String string) (getPosition state)
    in
        ( state { currentColumn = currentColumn state + genericLength string + 2, tokens = token : tokens state }
        , tail rest
        )

parseCharLiteral state input =
    (state, input)

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