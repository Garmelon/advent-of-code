{-# LANGUAGE TupleSections #-}

module Aoc.Y2020.D11
  ( day
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.Maybe

import qualified Data.Map.Strict as Map

import           Aoc.Day
import           Aoc.Parse

data Seat = Empty | Occupied
  deriving (Show, Eq)

isOccupied :: Seat -> Bool
isOccupied Occupied = True
isOccupied Empty    = False

data Field = Field
  { fMap  :: Map.Map (Int, Int) Seat
  , fSize :: (Int, Int)
  } deriving (Show, Eq)

parser :: Parser Field
parser = do
  ls <- manyLines $ many pSeat
  let dims = (length $ head ls, length ls)
      lsWithCoords = concat $ zipWith (\y -> zipWith (\x -> ((x, y),)) [0..]) [0..] ls
      m = Map.fromList [(xy, s) | (xy, Just s) <- lsWithCoords]
  pure $ Field m dims
  where
    pSeat = (Nothing <$ char '.') <|> (Just Empty <$ char 'L')

step :: (Field -> (Int, Int) -> Seat -> Seat) -> Field -> Field
step f field = field { fMap = m' }
  where
    m' = Map.fromList $ map (\(xy, s) -> (xy, f field xy s)) $ Map.toList $ fMap field

stepSeatP1 :: Field -> (Int, Int) -> Seat -> Seat
stepSeatP1 field (x, y) s
  | s == Empty    && occupied == 0 = Occupied
  | s == Occupied && occupied >= 4 = Empty
  | otherwise                      = s
  where
    adjacent = mapMaybe (fMap field Map.!?) $ filter (/= (x, y)) $ (,) <$> [x-1,x,x+1] <*> [y-1,y,y+1]
    occupied = length $ filter isOccupied adjacent

potAdjacent :: Field -> (Int, Int) -> [[(Int, Int)]]
potAdjacent field xy = map towards [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]
  where
    (mx, my) = fSize field
    towards (dx, dy)
      = takeWhile (\(x, y) -> 0 <= x && x < mx && 0 <= y && y < my)
      $ drop 1
      $ iterate (\(x, y) -> (x + dx, y + dy)) xy

firstJust :: [Maybe a] -> Maybe a
firstJust []             = Nothing
firstJust (Just a  : _)  = Just a
firstJust (Nothing : as) = firstJust as

stepSeatP2 :: Field -> (Int, Int) -> Seat -> Seat
stepSeatP2 field xy s
  | s == Empty    && occupied == 0 = Occupied
  | s == Occupied && occupied >= 5 = Empty
  | otherwise                      = s
  where
    adjacent = mapMaybe (firstJust . map (fMap field Map.!?)) $ potAdjacent field xy
    occupied = length $ filter isOccupied adjacent

iterateUntilSettled :: (Eq a) => (a -> a) -> a -> a
iterateUntilSettled f a
  | a == a'   = a
  | otherwise = iterateUntilSettled f a'
  where
    a' = f a

printField :: Field -> IO ()
printField field = do
  let (mx, my) = fSize field
  for_ [0..my-1] $ \y -> do
    for_ [0..mx-1] $ \x ->
      printSeat $ fMap field Map.!? (x, y)
    putStrLn ""
  putStrLn ""
  where
    printSeat Nothing         = putStr "."
    printSeat (Just Empty)    = putStr "L"
    printSeat (Just Occupied) = putStr "#"

printIterationsUntilSettled :: (Field -> Field) -> Field -> IO ()
printIterationsUntilSettled f a = do
  printField a
  let a' = f a
  unless (a == a') $ printIterationsUntilSettled f a'

solver :: Field -> IO ()
solver field = do
  putStrLn ">> Part 1"
  printIterationsUntilSettled (step stepSeatP1) field
  let field1 = iterateUntilSettled (step stepSeatP1) field
  print $ length $ filter isOccupied $ Map.elems $ fMap field1

  putStrLn ""
  putStrLn ">> Part 2"
  printIterationsUntilSettled (step stepSeatP2) field
  let field2 = iterateUntilSettled (step stepSeatP2) field
  print $ length $ filter isOccupied $ Map.elems $ fMap field2

day :: Day
day = dayParse parser solver
