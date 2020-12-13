{-# LANGUAGE NumericUnderscores #-}

module Aoc.Y2020.D13
  ( day
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.Function
import           Data.Maybe

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

departsAt :: Bus -> Integer -> Bool
departsAt bus time = (time + bDelta bus) `mod` bId bus == 0

earliestTimestamp :: [Bus] -> Integer -> Integer -> Integer
earliestTimestamp []     start _    = start
earliestTimestamp (b:bs) start step = earliestTimestamp bs time (lcm step $ bId b)
  where
    time = fromJust $ find (b `departsAt`) $ iterate (+ step) start

solver :: (Integer, [Bus]) -> IO ()
solver (earliest, buses) = do
  putStrLn ">> Part 1"
  let busTimes = [(bid, earliest `mod` bid) | Bus bid _ <- buses]
      (nextBus, waitTime) = minimumBy (compare `on` snd) busTimes
  print $ nextBus * waitTime

  putStrLn ""
  putStrLn ">> Part 2"
  print $ earliestTimestamp buses 0 1

day :: Day
day = dayParse parser solver
