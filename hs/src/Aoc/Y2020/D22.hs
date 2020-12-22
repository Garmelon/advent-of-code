{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D22
  ( day
  ) where

import           Control.Monad
import           Data.Foldable

import qualified Data.Sequence as Seq
import qualified Data.Set      as Set

import           Aoc.Day
import           Aoc.Parse

type Hand = Seq.Seq Int

parser :: Parser (Hand, Hand)
parser = do
  void $ string "Player 1:\n"
  p1 <- many (decimal <* newline)
  void $ string "\nPlayer 2:\n"
  p2 <- many (decimal <* newline)
  pure (Seq.fromList p1, Seq.fromList p2)

combat :: (Hand, Hand) -> Hand
combat (Seq.Empty, crab) = crab
combat (self, Seq.Empty) = self
combat (s Seq.:<| self, c Seq.:<| crab)
  | s >= c    = combat (self Seq.|> s Seq.|> c, crab)
  | otherwise = combat (self, crab Seq.|> c Seq.|> s)

recursiveCombat :: Set.Set (Hand, Hand) -> (Hand, Hand) -> Either Hand Hand
recursiveCombat _ (Seq.Empty, crab) = Right crab
recursiveCombat _ (self, Seq.Empty) = Left self
recursiveCombat previously now@(s Seq.:<| self, c Seq.:<| crab)
  | now `Set.member` previously = Left self
  | otherwise = recursiveCombat (Set.insert now previously) $ case winner of
      Left  _ -> (self Seq.|> s Seq.|> c, crab)
      Right _ -> (self, crab Seq.|> c Seq.|> s)
  where
    sLen = Seq.length self
    cLen = Seq.length crab
    winner = if
      | s <= sLen && c <= cLen -> recursiveCombat Set.empty (Seq.take s self, Seq.take c crab)
      | s >= c                 -> Left self
      | otherwise              -> Right crab


score :: Hand -> Int
score = sum . zipWith (*) [1..] . toList . Seq.reverse

solver :: (Hand, Hand) -> IO ()
solver (self, crab) = do
  putStrLn ">> Part 1"
  print $ score $ combat (self, crab)

  putStrLn ""
  putStrLn ">> Part 2"
  print $ score $ either id id $ recursiveCombat Set.empty (self, crab)

day :: Day
day = dayParse parser solver
