{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D20
  ( day
  ) where

import           Data.List
import           Data.Maybe
import           Control.Monad

import qualified Data.Map  as Map
import qualified Data.Set  as Set

import           Aoc.Day
import           Aoc.Parse

newtype Tile = Tile [[Bool]] -- List of rows
  deriving (Show)

tLeft :: Tile -> [Bool]
tLeft (Tile l) = map head l

tRight :: Tile -> [Bool]
tRight (Tile l) = map last l

tTop :: Tile -> [Bool]
tTop (Tile l) = head l

tBottom :: Tile -> [Bool]
tBottom (Tile l) = last l

tTranspose :: Tile -> Tile
tTranspose (Tile l) = Tile $ transpose l

tFlipV :: Tile -> Tile
tFlipV (Tile l) = Tile $ reverse l

tFlipH :: Tile -> Tile
tFlipH (Tile l) = Tile $ map reverse l

tTurnCw :: Tile -> Tile
tTurnCw = tFlipH . tTranspose

tTurnCcw :: Tile -> Tile
tTurnCcw = tFlipV . tTranspose

tRotations :: Tile -> [Tile]
tRotations = take 4 . iterate tTurnCw

tVariations :: Tile -> [Tile]
tVariations t = tRotations t ++ tRotations (tFlipH t)

parser :: Parser (Map.Map Int Tile)
parser = Map.fromList <$> (tile `sepBy` newline)
  where
    field = (False <$ string ".") <|> (True <$ "#")
    row = sequenceA (replicate 10 field) <* newline
    tile = do
      tid <- string "Tile " *> decimal <* string ":" <* newline
      rows <- sequenceA $ replicate 10 row
      pure (tid, Tile rows)

deduplicate :: (Ord a) => [a] -> [a]
deduplicate = Set.toList . Set.fromList

type Pos = (Int, Int)

vicinity :: Pos -> [Pos]
vicinity (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

freeAdjacents :: Map.Map Pos a -> [Pos]
freeAdjacents m =
  let adjacents = deduplicate $ concatMap vicinity $ Map.keys m
      taken = Map.keysSet m
  in  filter (not . (`Set.member` taken)) adjacents

unplaced :: Map.Map Int Tile -> Map.Map Pos (Int, Tile) -> [Int]
unplaced tiles placed = Set.toList $ Map.keysSet tiles Set.\\ Set.fromList (map fst $ Map.elems placed)

valid :: Map.Map Pos (Int, Tile) -> Pos -> Tile -> Bool
valid placed (x, y) tile = validLeft && validRight && validTop && validBottom
  where
    validAt pos f g = maybe True (\a -> f (snd a) == g tile) $ placed Map.!? pos
    validLeft   = validAt (x - 1, y) tRight  tLeft
    validRight  = validAt (x + 1, y) tLeft   tRight
    validTop    = validAt (x, y - 1) tBottom tTop
    validBottom = validAt (x, y + 1) tTop    tBottom

place :: Map.Map Int Tile -> Map.Map Pos (Int, Tile) -> Maybe (Map.Map Pos (Int, Tile))
place tiles placed = listToMaybe $ do
  tid <- unplaced tiles placed
  let tile = tiles Map.! tid
  pos <- freeAdjacents placed
  var <- tVariations tile
  guard $ valid placed pos var
  pure $ Map.insert pos (tid, var) placed

whileJust :: (a -> Maybe a) -> a -> a
whileJust f a = maybe a (whileJust f) $ f a

placeAll :: Map.Map Int Tile -> Map.Map Pos (Int, Tile)
placeAll tiles = whileJust (place tiles) $ Map.singleton (0, 0) $ head $ Map.assocs tiles

corners :: [Pos] -> [Pos]
corners positions =
  let minX = minimum $ map fst positions
      maxX = maximum $ map fst positions
      minY = minimum $ map snd positions
      maxY = maximum $ map snd positions
  in  (,) <$> [minX, maxX] <*> [minY, maxY]

solver :: Map.Map Int Tile -> IO ()
solver tiles = do
  putStrLn ">> Part 1"
  let placed = placeAll tiles
      cornerIds = map (fst . (placed Map.!)) $ corners $ Map.keys placed
  print $ product cornerIds

day :: Day
day = dayParse parser solver
