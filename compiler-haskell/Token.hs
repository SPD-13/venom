module Token where

data Token = Token
    { tokenType :: TokenType
    , pos :: TokenPosition
    } deriving Show

data TokenType
    -- Keywords
    = If -- if
    | Then -- then
    | Else -- else
    | Is -- is
    | Let -- let
    | In -- in
    -- Characters
    | LeftParen -- (
    | RightParen -- )
    | PartialRightParen -- )!
    | LeftBracket -- [
    | RightBracket -- ]
    | LeftAngle -- <
    | RightAngle -- >
    | Equals -- =
    | Union -- |
    | Comma -- ,
    | Dot -- .
    -- Operators
    | Equality -- ==
    | Inequality -- /=
    | Plus -- +
    | Minus -- -
    | Modulo -- %
    | Pipe -- |>
    | ReversePipe -- <|
    | PartialPipe -- !>
    | ReversePartialPipe -- <!
    | Or -- ||
    | And -- &&
    -- Literals
    | String String -- "Hello"
    | Char Char -- 'a'
    | Integer Integer -- 55
    -- Symbols
    | DataType String -- DataType
    | Value String -- value
    -- Layout
    | Newline
    | Indent
    | Dedent
    deriving Show

data TokenPosition = TokenPosition
    { line :: Integer
    , column :: Integer
    } deriving Show

matchKeyword :: String -> TokenType
matchKeyword text = case text of
    "if" -> If
    "then" -> Then
    "else" -> Else
    "is" -> Is
    "let" -> Let
    "in" -> In
    _ -> Value text
