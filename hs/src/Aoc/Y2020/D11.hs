{-# LANGUAGE TupleSections #-}

module Aoc.Y2020.D11
  ( day
  ) where

import           Data.Maybe

import qualified Data.Map.Strict   as Map

import           Aoc.Day
import           Aoc.Parse

data Seat = Empty | Occupied
  deriving (Show, Eq)

isOccupied :: Seat -> Bool
isOccupied Occupied = True
isOccupied Empty    = False

isEmpty :: Seat -> Bool
isEmpty = not . isOccupied

type Field = Map.Map (Int, Int) Seat

parser :: Parser Field
parser = do
  ls <- concat . zipWith (\y -> zipWith (curry (y,)) [0..]) [0..]  <$> pLines
  pure $ Map.fromList [((x, y), s) | (y, (x, Just s)) <- ls]
  where
    pSeat = (Nothing <$ char '.') <|> (Just Empty <$ char 'L')
    pLines = manyLines $ many pSeat

step :: Map.Map (Int, Int) Seat -> Map.Map (Int, Int) Seat
step field = Map.fromList $ map (\(xy, s) -> (xy, stepSeat field xy s)) $ Map.toList field

stepSeat :: Map.Map (Int, Int) Seat -> (Int, Int) -> Seat -> Seat
stepSeat field (x, y) s
  | s == Empty    && occupied == 0 = Occupied
  | s == Occupied && occupied >= 4 = Empty
  | otherwise                      = s
  where
    adjacent = mapMaybe (field Map.!?) $ filter (/= (x, y)) $ (,) <$> [x-1,x,x+1] <*> [y-1,y,y+1]
    occupied = length $ filter isOccupied adjacent

iterateUntilSettled :: (Eq a) => (a -> a) -> a -> a
iterateUntilSettled f a
  | a == a'   = a
  | otherwise = iterateUntilSettled f a'
  where
    a' = f a

solver :: Field -> IO ()
solver field = do
  putStrLn ">> Part 1"
  let field' = iterateUntilSettled step field
  print $ length $ filter isOccupied $ Map.elems field'

day :: Day
day = dayParse parser solver
