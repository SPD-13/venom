module Token where

import AST (ExpressionType(..))
import Operator
import Position

data Token = Token
    { tokenType :: TokenType
    , position :: Position
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
    | Colon -- :
    -- Operators
    | Operator Operator
    -- Literals
    | String String -- "Hello"
    | Char Char -- 'a'
    | Integer Integer -- 55
    -- Symbols
    | DataType String -- DataType
    | Identifier String -- value
    deriving (Show, Eq)

matchKeyword :: String -> TokenType
matchKeyword text = case text of
    "if" -> If
    "then" -> Then
    "else" -> Else
    "is" -> Is
    "let" -> Let
    "in" -> In
    _ -> Identifier text
