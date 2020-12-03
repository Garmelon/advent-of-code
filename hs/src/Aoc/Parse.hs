module Aoc.Parse
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
  , Parser
  , manyLines
  , word
  , untilEol
  ) where

import           Data.Void
import           Data.Char

import qualified Data.Text                  as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (binary, decimal, float,
                                             hexadecimal, octal, scientific,
                                             signed)

type Parser = Parsec Void T.Text

manyLines :: Parser a -> Parser [a]
manyLines p = sepEndBy (try p) newline

word :: Parser T.Text
word = takeWhileP Nothing (not . isSeparator)

untilEol :: Parser T.Text
untilEol = takeWhileP Nothing (/= '\n')
