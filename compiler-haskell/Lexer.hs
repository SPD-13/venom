module Lexer where

import Data.List (genericLength)
import Data.Char

import Token
import Operator

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
            case char of
                '(' -> pc LeftParen
                ')' -> if peek rest == "!" then pdc PartialRightParen else pc RightParen
                '[' -> pc LeftBracket
                ']' -> pc RightBracket
                '<' ->
                    if peek rest == "|" then
                        pdc $ Operator ReversePipe
                    else if peek rest == "!" then
                        pdc $ Operator ReversePartialPipe
                    else
                        pc LeftAngle
                '>' -> pc RightAngle
                '=' -> if peek rest == "=" then pdc $ Operator Equality else pc Equals
                '|' ->
                    if peek rest == ">" then
                        pdc $ Operator Pipe
                    else if peek rest == "|" then
                        pdc $ Operator Or
                    else
                        pc Union
                ',' -> pc Comma
                '.' -> pc Dot
                '/' -> if peek rest == "=" then pdc $ Operator Inequality else (state, "")
                '+' -> pc $ Operator Plus
                '-' -> pc $ Operator Minus
                '%' -> pc $ Operator Modulo
                '!' -> if peek rest == ">" then pdc $ Operator PartialPipe else (state, "")
                '&' -> if peek rest == "&" then pdc $ Operator And else (state, "")
                '"' -> parseString state rest
                '\'' -> parseCharLiteral state rest
                '\n' -> pc Newline
                _ ->
                     if isDigit char then
                        parseInteger state input
                    else if isUpper char then
                        parseDataType state input
                    else if isLower char then
                        parseValue state input
                    else
                        (state, "")
        (newState, newInput) = removeWhitespace result
    in lexerStep newState newInput

peek :: String -> String
peek = take 1

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
