module Aoc.Y2020.D15
  ( day
  ) where

import           Data.List

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [Int]
parser = (decimal `sepBy` char ',') <* newline

step :: [Int] -> [Int]
step []     = error "list must not be empty"
step (x:xs) = case elemIndex x xs of
  Nothing -> 0 : x : xs
  Just i  -> (i + 1) : x : xs

stepUntil :: Int -> [Int] -> [Int]
stepUntil amount nums = iterate step nums !! (amount - length nums)

solver :: [Int] -> IO ()
solver nums = do
  putStrLn ">> Part 1"
  print $ head $ stepUntil 2020 $ reverse nums

day :: Day
day = dayParse parser solver
