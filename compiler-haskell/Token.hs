module Token where

data Token = Token
    { tokenType :: TokenType
    , pos :: TokenPosition
    } deriving Show

data TokenType
    -- Keywords
    = If
    | Then
    | Else
    -- Characters
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftAngle
    | RightAngle
    | Equals
    | PartialApplication
    | Union
    -- Operators
    | Equality
    | Inequality
    | Modulo
    | Pipe
    | ReversePipe
    | PartialPipe
    | ReversePartialPipe
    | Or
    | And
    -- Symbols
    | DataType String
    | Value String
    -- Literals
    | String String
    | Char Char
    | Integer Integer
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
    _ -> Value text
