module Aoc.Y2020.D03
  ( day
  ) where

import           Data.List

import           Aoc.Day
import           Aoc.Parse

type Forest = [[Bool]]
type Slope = [Maybe Int]

parser :: Parser Forest
parser = manyLines $ many $ do
  c <- lineChar
  pure $ c == '#'

slope :: Int -> Int -> Slope
slope dx dy = intercalate (replicate (dy - 1) Nothing) [[Just x] | x <- [0,dx..]]

onSlope :: Forest -> Slope -> Int
onSlope trees s = length $ filter id [row !! x | (row, Just x) <- zip trees s]

solver :: Forest -> IO ()
solver trees = do
  let infTrees = map cycle trees

  putStrLn ">> Part 1"
  let treesHit = length $ filter id $ zipWith (!!) infTrees [0,3..]
  putStrLn $ "Trees hit for slope 3-1: " ++ show treesHit

  putStrLn ""
  putStrLn ">> Part 2"
  let oneOne   = onSlope infTrees $ slope 1 1
      threeOne = onSlope infTrees $ slope 3 1
      fiveOne  = onSlope infTrees $ slope 5 1
      sevenOne = onSlope infTrees $ slope 7 1
      oneTwo   = onSlope infTrees $ slope 1 2
  putStrLn $ "right 1, down 1: " ++ show oneOne
  putStrLn $ "right 3, down 1: " ++ show threeOne
  putStrLn $ "right 5, down 1: " ++ show fiveOne
  putStrLn $ "right 7, down 1: " ++ show sevenOne
  putStrLn $ "right 1, down 2: " ++ show oneTwo
  putStrLn $ "Product: " ++ show (oneOne * threeOne * fiveOne * sevenOne * oneTwo)

day :: Day
day = dayParse parser solver
