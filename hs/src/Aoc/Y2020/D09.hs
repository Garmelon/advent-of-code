module Aoc.Y2020.D09 (day) where

import           Aoc.Day
import           Aoc.Parse
import           Data.List

splitAndGroup :: Int -> [a] -> [([a], a)]
splitAndGroup width xs = case splitAt width xs of
  (_, [])   -> []
  (as, b:_) -> (as, b) : splitAndGroup width (drop 1 xs)

isValid :: [Int] -> Int -> Bool
isValid nums n = elem n $ (+) <$> nums <*> nums

findRanges :: Int -> [Int] -> [[Int]]
findRanges target = filter ((== target) . sum) . concatMap tails . inits

day :: Day
day = dayParse (manyLines decimal) $ \nums -> do
  let (_, invalidN) = head $ dropWhile (uncurry isValid) $ splitAndGroup 25 nums
  putStrLn $ ">> Part 1: " ++ show invalidN
  let weakness = head $ findRanges invalidN nums
  putStrLn $ ">> Part 2: " ++ show (minimum weakness + maximum weakness)
