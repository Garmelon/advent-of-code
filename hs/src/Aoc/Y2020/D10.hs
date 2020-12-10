module Aoc.Y2020.D10
  ( day
  ) where

import           Data.List

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [Integer]
parser = manyLines decimal

possibilities :: Integer -> Integer -> Integer -> [Integer] -> Integer
possibilities _ _ c []     = c
possibilities a b c (1:xs) = possibilities b c (a + b + c) [x - 1 | x <- xs]
possibilities _ b c (2:xs) = possibilities c 0 (b + c)     [x - 2 | x <- xs]
possibilities _ _ c (3:xs) = possibilities 0 0 c           [x - 3 | x <- xs]
possibilities _ _ _ (_:_)  = 0

solver :: [Integer] -> IO ()
solver nums = do
  let snums = sort nums ++ [maximum nums + 3]

  putStrLn ">> Part 1"
  let diffs = zipWith (-) snums (0 : snums)
  print $ length (filter (==1) diffs) * length (filter (==3) diffs)

  putStrLn ""
  putStrLn ">> Part 2"
  print $ possibilities 0 0 1 snums

day :: Day
day = dayParse parser solver
