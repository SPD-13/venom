{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

main :: IO ()
main =
    parseTest (satisfy (== 'a') :: Parser Char) "b"
