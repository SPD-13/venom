module Position where

data Position = Position
    { line :: Integer
    , column :: Integer
    } deriving (Eq, Show)
