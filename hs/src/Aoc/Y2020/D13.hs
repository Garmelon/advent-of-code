module Aoc.Y2020.D13
  ( day
  ) where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe

import           Aoc.Day
import           Aoc.Parse

parser :: Parser (Int, [Int])
parser = do
  earliest <- decimal
  void newline
  buses <- sepBy ((Just <$> decimal) <|> (Nothing <$ char 'x')) (char ',')
  void newline
  pure (earliest, catMaybes buses)

solver :: (Int, [Int]) -> IO ()
solver (earliest, buses) = do
  putStrLn ">> Part 1"
  let busTimes = [(bus, earliest - mod earliest bus + bus) | bus <- buses]
      (nextBus, nextBusTime) = minimumBy (compare `on` snd) busTimes
  print $ nextBus * (nextBusTime - earliest)

day :: Day
day = dayParse parser solver
