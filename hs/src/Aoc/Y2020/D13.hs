{-# LANGUAGE NumericUnderscores #-}

module Aoc.Y2020.D13
  ( day
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord

import           Aoc.Day
import           Aoc.Parse

parser :: Parser (Integer, [Maybe Integer])
parser = do
  earliest <- decimal
  void newline
  buses <- sepBy ((Just <$> decimal) <|> (Nothing <$ char 'x')) (char ',')
  void newline
  pure (earliest, buses)

data Bus = Bus
  { bId    :: Integer
  , bDelta :: Integer
  } deriving (Show)

departsAt :: Bus -> Integer -> Bool
departsAt bus time = (time + bDelta bus) `mod` bId bus == 0

includeBus :: Bus -> (Integer, Integer) -> (Integer, Integer)
includeBus bus (time, step) =
  let time' = fromJust $ find (bus `departsAt`) $ iterate (+ step) time
      step' = step * bId bus
  in  (time', step')

earliestTimestamp :: [Maybe Integer] -> Integer
earliestTimestamp buses =
  let busDeltas = sortOn (Down . bId) [Bus bus delta | (Just bus, delta) <- zip buses [0..]]
  in  fst $ foldl' (flip includeBus) (0, 1) busDeltas

solver :: (Integer, [Maybe Integer]) -> IO ()
solver (earliest, buses) = do
  putStrLn ">> Part 1"
  let busTimes = [(bus, earliest - mod earliest bus + bus) | Just bus <- buses]
      (nextBus, nextBusTime) = minimumBy (compare `on` snd) busTimes
  print $ nextBus * (nextBusTime - earliest)

  putStrLn ""
  putStrLn ">> Part 2"
  print $ earliestTimestamp buses

day :: Day
day = dayParse parser solver
