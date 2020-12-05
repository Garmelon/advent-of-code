module Aoc.Y2020.D05
  ( day
  ) where

import           Control.Monad

import qualified Data.Set      as Set

import           Aoc.Day
import           Aoc.Parse

-- Ordered from least to most significant
fromBin :: [Bool] -> Int
fromBin bs = sum $ zipWith (*) (iterate (*2) 1) (map fromEnum bs)

parser :: Parser [Int]
parser = manyLines $ do
  fbs <- count 7 $ (False <$ char 'F') <|> (True <$ char 'B')
  lrs <- count 3 $ (False <$ char 'L') <|> (True <$ char 'R')
  pure $ fromBin $ reverse $ fbs ++ lrs

allSeats :: [Int]
allSeats = [0..1023]

mySeats :: Set.Set Int -> [Int]
mySeats taken = do
  sid <- allSeats
  guard $ (sid - 1) `Set.member` taken
  guard $ not $ sid `Set.member` taken
  guard $ (sid + 1) `Set.member` taken
  pure sid

solver :: [Int] -> IO ()
solver ps = do
  putStrLn ">> Part 1"
  print $ maximum ps

  putStrLn ""
  putStrLn ">> Part 2"
  print $ mySeats $ Set.fromList ps

day :: Day
day = dayParse "2020_05" parser solver
