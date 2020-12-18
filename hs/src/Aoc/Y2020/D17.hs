{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Aoc.Y2020.D17
  ( day
  ) where

import qualified Data.Set  as Set

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [[Bool]]
parser = manyLines $ many $ (False <$ char '.') <|> (True <$ char '#')

type Pos = (Int, Int, Int)
type World = Set.Set Pos

newWorld :: [[Bool]] -> World
newWorld
  = Set.fromList
  . map fst
  . filter snd
  . concat
  . zipWith (\y -> zipWith (\x -> ((x, y, 0),)) [0..]) [0..]

vicinity :: Pos -> [Pos]
vicinity (x, y, z) = (,,) <$> [x - 1, x, x + 1] <*> [y - 1, y, y + 1] <*> [z - 1, z, z + 1]

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
  let world = newWorld slice
  print $ Set.size $ steps world

day :: Day
day = dayParse parser solver
