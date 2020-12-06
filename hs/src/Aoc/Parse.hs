module Aoc.Parse
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
  , Parser
  , manyLines
  , oneSpace
  , untilSpace
  , untilEol
  , lineChar
  , digit
  ) where

import           Data.Char
import           Data.Void

import qualified Data.Text                  as T
import           Text.Megaparsec            hiding (InvalidPosException, Pos,
                                             PosState, SourcePos, State,
                                             defaultTabWidth, initialPos, mkPos,
                                             pos1, sourcePosPretty, unPos)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (binary, decimal, float,
                                             hexadecimal, octal, scientific,
                                             signed)

type Parser = Parsec Void T.Text

manyLines :: Parser a -> Parser [a]
manyLines p = endBy (try p) newline

oneSpace :: Parser Char
oneSpace = label "whitespace character" $ satisfy isSpace

untilSpace :: Parser T.Text
untilSpace = takeWhileP (Just "non-whitespace character") (not . isSpace)

untilEol :: Parser T.Text
untilEol = takeWhileP (Just "non-newline character") (/= '\n')

lineChar :: Parser Char
lineChar = label "non-newline character" $ satisfy (/= '\n')

digit :: Num a => Parser a
digit = foldr1 (<|>)
  [ 0 <$ char '0'
  , 1 <$ char '1'
  , 2 <$ char '2'
  , 3 <$ char '3'
  , 4 <$ char '4'
  , 5 <$ char '5'
  , 6 <$ char '6'
  , 7 <$ char '7'
  , 8 <$ char '8'
  , 9 <$ char '9'
  ]
