module Aoc.Y2020.D06
  ( day
  ) where

import qualified Data.Set  as Set

import           Aoc.Day
import           Aoc.Parse

type Group = [String]

parser :: Parser [Group]
parser = sepBy (endBy1 (some lowerChar) newline) newline

solver :: [Group] -> IO ()
solver groups = do
  putStrLn ">> Part 1"
  print $ sum $ map (Set.size . foldr1 Set.union . map Set.fromList) groups

  putStrLn ""
  putStrLn ">> Part 2"
  print $ sum $ map (Set.size . foldr1 Set.intersection . map Set.fromList) groups

day :: Day
day = dayParse parser solver
