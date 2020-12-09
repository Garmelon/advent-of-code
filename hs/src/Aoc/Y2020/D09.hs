module Aoc.Y2020.D09
  ( day
  ) where

import           Data.List

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [Int]
parser = manyLines decimal

splitAndGroup :: Int -> [Int] -> [([Int], Int)]
splitAndGroup width xs = case splitAt width xs of
  (_, [])   -> []
  (as, b:_) -> (as, b) : splitAndGroup width (drop 1 xs)

isValid :: [Int] -> Int -> Bool
isValid nums n = elem n $ (+) <$> nums <*> nums

continuousSubsequences :: [a] -> [[a]]
continuousSubsequences = filter (not . null) . concatMap tails . inits

findRanges :: Int -> [Int] -> [[Int]]
findRanges target = filter ((== target) . sum) . continuousSubsequences

solver :: [Int] -> IO ()
solver nums = do
  putStrLn ">> Part 1"
  let (_, invalidN) = head $ dropWhile (uncurry isValid) $ splitAndGroup 25 nums
  print invalidN

  putStrLn ""
  putStrLn ">> Part 2"
  let weakness = head $ findRanges invalidN nums
  print $ minimum weakness + maximum weakness

day :: Day
day = dayParse parser solver
