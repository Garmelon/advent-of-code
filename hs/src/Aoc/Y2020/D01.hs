module Aoc.Y2020.D01
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse

findPair :: [Integer] -> (Integer, Integer)
findPair l = head $ do
  a <- l
  b <- l
  guard $ a + b == 2020
  pure (a, b)

findTriple :: [Integer] -> (Integer, Integer, Integer)
findTriple l = head $ do
  a <- l
  b <- l
  c <- l
  guard $ a + b + c == 2020
  pure (a, b, c)

parser :: Parser [Integer]
parser = manyLines decimal

solver :: [Integer] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  let (x1, x2) = findPair values
  putStrLn $ show x1 ++ " * " ++ show x2 ++ " = " ++ show (x1 * x2)

  let (y1, y2, y3) = findTriple values
  putStrLn ">> Part 2"
  putStrLn $ show y1 ++ " * " ++ show y2 ++ " * " ++ show y3 ++ " = " ++ show (y1 * y2 * y3)

day :: Day
day = dayParse "2020_01" parser solver
