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
    let pc = parseChar state rest
        pdc = parseDoubleChar state rest
        result =
            if char == '(' then
                pc LeftParen
            else if char == ')' then
                if peek rest == "!" then
                    pdc PartialRightParen
                else
                    pc RightParen
            else if char == '[' then
                pc LeftBracket
            else if char == ']' then
                pc RightBracket
            else if char == '<' then
                if peek rest == "|" then
                    pdc ReversePipe
                else if peek rest == "!" then
                    pdc ReversePartialPipe
                else
                    pc LeftAngle
            else if char == '>' then
                pc RightAngle
            else if char == '=' then
                if peek rest == "=" then
                    pdc Equality
                else
                    pc Equals
            else if char == '|' then
                if peek rest == ">" then
                    pdc Pipe
                else if peek rest == "|" then
                    pdc Or
                else
                    pc Union
            else if char == ',' then
                pc Comma
            else if char == '.' then
                pc Dot
            else if char == '/' && peek rest == "=" then
                pdc Inequality
            else if char == '+' then
                pc Plus
            else if char == '-' then
                pc Minus
            else if char == '%' then
                pc Modulo
            else if char == '!' && peek rest == ">" then
                pdc PartialPipe
            else if char == '&' && peek rest == "&" then
                pdc And
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
                pc Newline
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
    if peek input == "" || peek (tail input) /= "'" then
        (state, "")
    else
        let (char:rest) = input
            token = Token (Char char) (getPosition state)
        in
            ( state { currentColumn = currentColumn state + 3, tokens = token : tokens state }
            , tail rest
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
