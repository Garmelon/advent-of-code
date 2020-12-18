{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D17
  ( day
  ) where

import qualified Data.Set  as Set

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [[Bool]]
parser = manyLines $ many $ (False <$ char '.') <|> (True <$ char '#')

type Pos = [Int]
type World = Set.Set Pos

newWorld :: Int -> [[Bool]] -> World
newWorld dims slice = Set.fromList $ do
  (y, row) <- zip [0..] slice
  (x, True) <- zip [0..] row
  pure $ [x, y] ++ replicate (dims - 2) 0

vicinity :: Pos -> [Pos]
vicinity [] = [[]]
vicinity (x:xs) = do
  x2 <- [x - 1, x, x + 1]
  xs2 <- vicinity xs
  pure $ x2 : xs2

neighbours :: Pos -> [Pos]
neighbours p = [p2 | p2 <- vicinity p, p2 /= p]

interesting :: World -> Set.Set Pos
interesting w = Set.fromList $ vicinity =<< Set.toList w

alive :: Bool -> Int -> Bool
alive True  2 = True
alive True  3 = True
alive False 3 = True
alive _     _ = False

step :: World -> World
step w = Set.filter go $ interesting w
  where
    go p = alive (p `Set.member` w) (length $ filter (`Set.member` w) $ neighbours p)

steps :: World -> World
steps = foldr (.) id $ replicate 6 step

solver :: [[Bool]] -> IO ()
solver slice = do
  putStrLn ">> Part 1"
  print $ Set.size $ steps $ newWorld 3 slice

  putStrLn ""
  putStrLn ">> Part 2"
  print $ Set.size $ steps $ newWorld 4 slice

day :: Day
day = dayParse parser solver
