module Aoc.Y2019.D01
  ( day
  ) where

import           Aoc.Day
import           Aoc.Parse

fuel :: Integer -> Integer
fuel n = (n `div` 3) - 2

iteratedFuel :: Integer -> Integer
iteratedFuel = sum . takeWhile (> 0) . tail . iterate fuel

parser :: Parser [Integer]
parser = manyLines decimal

solver :: [Integer] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  putStr "Total fuel: "
  print $ sum $ map fuel values

  putStrLn ""
  putStrLn ">> Part 2"
  putStr "Total fuel (iterated): "
  print $ sum $ map iteratedFuel values

day :: Day
day = dayParse parser solver
