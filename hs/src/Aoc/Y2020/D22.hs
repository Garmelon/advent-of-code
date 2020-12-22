{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D22
  ( day
  ) where

import           Control.Monad
import           Data.Foldable

import qualified Data.Sequence as Seq

import           Aoc.Day
import           Aoc.Parse

parser :: Parser (Seq.Seq Int, Seq.Seq Int)
parser = do
  void $ string "Player 1:\n"
  p1 <- many (decimal <* newline)
  void $ string "\nPlayer 2:\n"
  p2 <- many (decimal <* newline)
  pure (Seq.fromList p1, Seq.fromList p2)

step :: (Seq.Seq Int, Seq.Seq Int) -> Either (Seq.Seq Int) (Seq.Seq Int, Seq.Seq Int)
step (Seq.Empty, crab) = Left crab
step (self, Seq.Empty) = Left self
step (s Seq.:<| self, c Seq.:<| crab)
  | s >= c    = Right (self Seq.|> s Seq.|> c, crab)
  | otherwise = Right (self, crab Seq.|> c Seq.|> s)

untilLeft :: (a -> Either b a) -> a -> b
untilLeft f a = case f a of
  Left b   -> b
  Right a2 -> untilLeft f a2

solver :: (Seq.Seq Int, Seq.Seq Int) -> IO ()
solver (self, crab) = do
  putStrLn ">> Part 1"
  let winner = untilLeft step (self, crab)
  print $ sum $ zipWith (*) [1..] $ toList $ Seq.reverse winner

day :: Day
day = dayParse parser solver
