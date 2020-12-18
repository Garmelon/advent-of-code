{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

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
vicinity = foldr (\x -> ((:) <$> [x - 1, x, x + 1] <*>)) (pure [])

neighbours :: Pos -> [Pos]
neighbours p = [p2 | p2 <- vicinity p, p2 /= p]

interesting :: World -> Set.Set Pos
interesting w = Set.fromList $ vicinity =<< Set.toList w

alive :: Bool -> Int -> Bool
alive True 2 = True
alive _    3 = True
alive _    _ = False

step :: World -> World
step w = flip Set.filter (interesting w) $ \p ->
  alive (p `Set.member` w) (length $ filter (`Set.member` w) $ neighbours p)

steps :: Int -> World -> World
steps n = foldr (.) id $ replicate n step

solver :: [[Bool]] -> IO ()
solver slice = do
  putStrLn ">> Part 1"
  print $ Set.size $ steps 6 $ newWorld 3 slice

  putStrLn ""
  putStrLn ">> Part 2"
  print $ Set.size $ steps 6 $ newWorld 4 slice

day :: Day
day = dayParse parser solver
