module Aoc.Parse
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
  , Parser
  , manyLines
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
manyLines p = sepBy p newline <* optional newline <* eof
