module Aoc.Parse
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
  , Parser
  , manyLines
  , untilEol
  , lineChar
  ) where

import           Data.Void

import qualified Data.Text                  as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (binary, decimal, float,
                                             hexadecimal, octal, scientific,
                                             signed)

type Parser = Parsec Void T.Text

manyLines :: Parser a -> Parser [a]
manyLines p = endBy (try p) newline

untilEol :: Parser T.Text
untilEol = takeWhileP Nothing (/= '\n')

lineChar :: Parser Char
lineChar = satisfy (/= '\n')
