{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D18
  ( day
  ) where

import           Control.Monad
import           Data.Char

import           Control.Monad.Combinators.Expr
import qualified Data.Text                      as T
import qualified Text.Megaparsec.Char.Lexer     as L

import           Aoc.Day
import           Aoc.Parse

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void $ lineWhile isSpace)

symbol :: T.Text -> Parser T.Text
symbol = L.symbol (void $ lineWhile isSpace)

parser :: [[Operator Parser Int]] -> Parser [Int]
parser table = manyLines expr
  where
    parens = between (symbol "(") (symbol ")")
    term = lexeme L.decimal <|> parens expr
    expr = makeExprParser term table

table1 :: [[Operator Parser Int]]
table1 = [[InfixL ((+) <$ symbol "+"), InfixL ((*) <$ symbol "*")]]

table2 :: [[Operator Parser Int]]
table2 = [[InfixL ((+) <$ symbol "+")], [InfixL ((*) <$ symbol "*")]]

solver :: FilePath -> T.Text -> IO ()
solver path text = do
  putStrLn ">> Part 1"
  parseAndSolve path text (parser table1) $ print . sum

  putStrLn ""
  putStrLn ">> Part 2"
  parseAndSolve path text (parser table2) $ print . sum

day :: Day
day = dayText solver
