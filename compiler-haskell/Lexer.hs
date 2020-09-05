module Lexer where

import Data.List (genericLength)
import Data.Char

import Error
import Operator
import Position
import Token

data State = State
    { currentLine :: Integer
    , currentColumn :: Integer
    , tokens :: [Token]
    , errors :: [Error]
    }

lex :: String -> Either [Error] [Token]
lex input =
    let initialState = State 1 1 [] []
        finalState = lexerStep initialState input
    in if null (errors finalState) then
        Right $ reverse $ tokens finalState
    else
        Left $ reverse $ errors finalState

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
                '/' ->
                    if peek rest == "=" then
                        pdc $ Operator Inequality
                    else
                        (state { errors = Error "Unexpected character '/'" (Error.Position (getPosition state)) : errors state }, "")
                '+' -> pc $ Operator Plus
                '-' -> pc $ Operator Minus
                '*' -> pc $ Operator Times
                '%' -> pc $ Operator Modulo
                '!' -> if peek rest == ">" then pdc $ Operator PartialPipe else (state, "")
                '&' -> if peek rest == "&" then pdc $ Operator And else (state, "")
                '"' -> parseString state rest
                '\'' -> parseCharLiteral state rest
                '$' ->
                    if peek rest == "[" then
                        removeBlockComment state $ tail rest
                    else if peek rest == "]" then
                        (state, "")
                    else
                        removeComment state rest
                _ ->
                     if isDigit char then 
                        parseInteger state input
                    else if isUpper char then
                        parseDataType state input
                    else if isLower char then
                        parseIdentifier state input
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

parseIdentifier state input =
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

removeBlockComment state input =
    let newState = state { currentColumn = currentColumn state + 2 }
    in removeBlockComment' 1 newState input

removeBlockComment' :: Integer -> State -> String -> (State, String)
removeBlockComment' level state input =
    let (skipped, rest) = span (not . (`elem` ['$', '\n'])) input
        newRest = drop 1 rest
    in
        if peek rest == "\n" then
            removeBlockComment' level (newLine state) newRest
        else if peek rest == "$" then
            let newState = state { currentColumn = currentColumn state + genericLength skipped + 2 }
                newInput = drop 1 newRest
            in
                if peek newRest == "[" then
                    removeBlockComment' (level + 1) newState newInput
                else if peek newRest == "]" then
                    if level == 1 then
                        (newState, newInput)
                    else
                        removeBlockComment' (level - 1) newState newInput
                else
                    removeBlockComment' level (state { currentColumn = currentColumn state + genericLength skipped + 1 }) newRest
        else
            (state, rest)

removeComment :: State -> String -> (State, String)
removeComment state input =
    let rest = dropWhile (/= '\n') input
    in
        ( newLine state
        , drop 1 rest
        )

removeWhitespace (state, input) =
    let (whitespace, rest) = span (`elem` [' ', '\t']) input
    in
        if peek rest == "\n" then
            removeWhitespace
                ( newLine state
                , tail rest
                )
        else
            ( state { currentColumn = currentColumn state + genericLength whitespace }
            , rest
            )

getPosition :: State -> Position
getPosition state = Position.Position (currentLine state) (currentColumn state)

newLine :: State -> State
newLine state = state { currentLine = currentLine state + 1, currentColumn = 1 }
