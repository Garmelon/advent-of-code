{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D16
  ( day
  ) where

import           Control.Monad

import qualified Data.Map      as Map
import qualified Data.Text     as T

import           Aoc.Day
import           Aoc.Parse

data Input = Input
  { iFields        :: Map.Map T.Text [(Int, Int)]
  , iMyTicket      :: [Int]
  , iNearbyTickets :: [[Int]]
  } deriving (Show)

parser :: Parser Input
parser = do
  fields <- Map.fromList <$> many (fieldLine <* newline)
  void $ string "\nyour ticket:\n"
  myTicket <- ticket <* newline
  void $ string "\nnearby tickets:\n"
  nearbyTickets <- many (ticket <* newline)
  pure $ Input fields myTicket nearbyTickets
  where
    fieldLine = do
      name <- takeWhileP Nothing $ \c -> (c /= ':') && (c /= '\n')
      void $ string ": "
      ranges <- sepBy ((,) <$> (decimal <* string "-") <*> decimal) (string " or ")
      pure (name, ranges)
    ticket = decimal `sepBy` string ","

valid :: Int -> [(Int, Int)] -> Bool
valid n = any (\(a, b) -> a <= n && n <= b)

findValid :: Map.Map T.Text [(Int, Int)] -> Int -> [T.Text]
findValid fields n = map fst $ filter (valid n . snd) $ Map.toList fields

anyValid :: Map.Map T.Text [(Int, Int)] -> Int -> Bool
anyValid fields = not . null . findValid fields

solver :: Input -> IO ()
solver i = do
  putStrLn ">> Part 1"
  print $ sum $ filter (not . anyValid (iFields i)) $ concat $ iNearbyTickets i

day :: Day
day = dayParse parser solver
