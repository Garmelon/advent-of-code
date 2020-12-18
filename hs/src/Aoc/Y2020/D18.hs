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

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Lit l)   = l
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void $ lineWhile isSpace)

symbol :: T.Text -> Parser T.Text
symbol = L.symbol (void $ lineWhile isSpace)

parser :: [[Operator Parser Expr]] -> Parser [Expr]
parser table = manyLines expr
  where
    parens = between (symbol "(") (symbol ")")
    term = (Lit <$> lexeme L.decimal) <|> parens expr
    expr = makeExprParser term table

table1 :: [[Operator Parser Expr]]
table1 = [[InfixL (Add <$ symbol "+"), InfixL (Mul <$ symbol "*")]]

table2 :: [[Operator Parser Expr]]
table2 = [[InfixL (Add <$ symbol "+")], [InfixL (Mul <$ symbol "*")]]

solver :: FilePath -> T.Text -> IO ()
solver path text = do
  putStrLn ">> Part 1"
  parseAndSolve path text (parser table1) $ print . sum . map eval

  putStrLn ""
  putStrLn ">> Part 2"
  parseAndSolve path text (parser table2) $ print . sum . map eval

day :: Day
day = dayText solver
