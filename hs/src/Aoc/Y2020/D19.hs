{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D19
  ( day
  ) where

import           Control.Monad
import           Data.Foldable

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Text.Megaparsec.Char.Lexer as L

import           Aoc.Day
import           Aoc.Parse

data Rule = Leaf Char | Branch [[Int]]
  deriving (Show)

type Rules = Map.Map Int Rule

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void lineSpace)

symbol :: T.Text -> Parser T.Text
symbol = L.symbol (void lineSpace)

parser :: Parser (Rules, [String])
parser = do
  rules <- Map.fromList <$> many (rule <* newline)
  void newline
  msgs <- map T.unpack <$> manyLines line
  pure (rules, msgs)
  where
    leaf = Leaf <$> (char '"' *> lineChar <* char '"')
    branch = Branch <$> some (lexeme decimal) `sepBy1` symbol "|"
    rule = do
      name <- decimal <* symbol ":"
      content <- leaf <|> branch
      pure (name, content)

type SolveM = StateT String []

consume :: Char -> SolveM ()
consume c = get >>= \case
  (x:xs) | x == c -> put xs
  _               -> empty

apply :: Rules -> Int -> SolveM ()
apply rules ruleId = case rules Map.! ruleId of
  Leaf c         -> consume c
  Branch options -> traverse_ (apply rules) =<< lift options

applyFully :: Rules -> Int -> SolveM ()
applyFully rules ruleId = do
  apply rules ruleId
  guard . null =<< get

isValid :: Rules -> String -> Bool
isValid rules msg = not $ null $ runStateT (applyFully rules 0) msg

solver :: (Rules, [String]) -> IO ()
solver (rules, msgs) = do
  putStrLn ">> Part 1"
  print $ length $ filter (isValid rules) msgs

day :: Day
day = dayParse parser solver
