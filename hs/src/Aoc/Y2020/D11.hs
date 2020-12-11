module Aoc.Y2020.D11
  ( day
  ) where

import           Data.Maybe

import qualified Data.Map.Strict as Map

import           Aoc.Day
import           Aoc.Parse

type Pos = (Int, Int)

data Field = Field
  { fMap  :: Map.Map Pos Bool
  , fSize :: Pos
  } deriving (Show, Eq)

parser :: Parser Field
parser = do
  ls <- manyLines $ many $ (Nothing <$ char '.') <|> (Just False <$ char 'L')
  let dims = (length $ head ls, length ls)
      lsWithCoords = concat $ zipWith (\y -> zipWith (\x -> (,) (x, y)) [0..]) [0..] ls
      m = Map.fromList [(pos, s) | (pos, Just s) <- lsWithCoords]
  pure $ Field m dims

step :: (Field -> Pos -> Bool -> Bool) -> Field -> Field
step f field = field { fMap = Map.mapWithKey (f field) $ fMap field }

add :: Pos -> Pos -> Pos
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

directions :: [Pos]
directions = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]

countOccupied :: [Bool] -> Int
countOccupied = length . filter id

stepSeatP1 :: Field -> Pos -> Bool -> Bool
stepSeatP1 field pos s
  | not s && occupied == 0 = True
  | s     && occupied >= 4 = False
  | otherwise              = s
  where
    occupied = countOccupied $ mapMaybe ((fMap field Map.!?) . add pos) directions

potentialAdjacent :: Field -> Pos -> [[Pos]]
potentialAdjacent field pos = map towards directions
  where
    (mx, my) = fSize field
    towards delta
      = takeWhile (\(x, y) -> 0 <= x && x < mx && 0 <= y && y < my)
      $ drop 1
      $ iterate (add delta) pos

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (<|>) Nothing

stepSeatP2 :: Field -> Pos -> Bool -> Bool
stepSeatP2 field pos s
  | not s && occupied == 0 = True
  | s     && occupied >= 5 = False
  | otherwise              = s
  where
    occupied
      = countOccupied
      $ mapMaybe (firstJust . map (fMap field Map.!?))
      $ potentialAdjacent field pos

iterateUntilSettled :: (Eq a) => (a -> a) -> a -> a
iterateUntilSettled f a
  | a == a'   = a
  | otherwise = iterateUntilSettled f a'
  where
    a' = f a

solver :: Field -> IO ()
solver field = do
  putStrLn ">> Part 1"
  print $ countOccupied $ Map.elems $ fMap $ iterateUntilSettled (step stepSeatP1) field

  putStrLn ""
  putStrLn ">> Part 2"
  print $ countOccupied $ Map.elems $ fMap $ iterateUntilSettled (step stepSeatP2) field

day :: Day
day = dayParse parser solver
