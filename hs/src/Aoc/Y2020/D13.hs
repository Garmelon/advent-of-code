{-# LANGUAGE NumericUnderscores #-}

module Aoc.Y2020.D13
  ( day
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.Function

import           Aoc.Day
import           Aoc.Parse

data Bus = Bus
  { bId    :: Integer
  , bDelta :: Integer
  } deriving (Show)

parser :: Parser (Integer, [Bus])
parser = do
  earliest <- decimal
  void newline
  buses <- sepBy ((Just <$> decimal) <|> (Nothing <$ char 'x')) (char ',')
  void newline
  pure (earliest, [Bus bid delta | (Just bid, delta) <- zip buses [0..]])

waitTime :: Integer -> Bus -> Integer
waitTime time bus = time `mod` bId bus

departsAt :: Bus -> Integer -> Bool
departsAt bus time = (time + bDelta bus) `mod` bId bus == 0

-- See https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
earliestTimestamp :: [Bus] -> Integer -> Integer -> Integer
earliestTimestamp [] time _ = time
earliestTimestamp (b:bs) time step
  | b `departsAt` time = earliestTimestamp bs     time          (step * bId b)
  | otherwise          = earliestTimestamp (b:bs) (time + step) step

solver :: (Integer, [Bus]) -> IO ()
solver (earliest, buses) = do
  putStrLn ">> Part 1"
  let nextBus = minimumBy (compare `on` waitTime earliest) buses
  print $ bId nextBus * waitTime earliest nextBus

  putStrLn ""
  putStrLn ">> Part 2"
  print $ earliestTimestamp buses 0 1

day :: Day
day = dayParse parser solver
