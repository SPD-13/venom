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
    | Equals
    -- Operators
    | Equality
    | Inequality
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

matchKeyword :: String -> Maybe(TokenType)
matchKeyword text = case text of
    "if" -> Just If
    "then" -> Just Then
    "else" -> Just Else
    _ -> Nothing
