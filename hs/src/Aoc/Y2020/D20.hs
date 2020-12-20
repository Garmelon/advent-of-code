{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D20
  ( day
  ) where

import           Control.Monad
import           Data.Bool
import           Data.List
import           Data.Maybe

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import           Aoc.Day
import           Aoc.Parse

newtype Tile = Tile { unTile :: [[Bool]] } -- List of rows
  deriving (Show)

tLeft :: Tile -> [Bool]
tLeft (Tile l) = map head l

tRight :: Tile -> [Bool]
tRight (Tile l) = map last l

tTop :: Tile -> [Bool]
tTop (Tile l) = head l

tBottom :: Tile -> [Bool]
tBottom (Tile l) = last l

tFlipV :: Tile -> Tile
tFlipV = Tile . reverse . unTile

tTurnCw :: Tile -> Tile
tTurnCw = Tile . transpose . reverse . unTile

tRotations :: Tile -> [Tile]
tRotations = take 4 . iterate tTurnCw

tVariations :: Tile -> [Tile]
tVariations t = tRotations t ++ tRotations (tFlipV t)

tShrink :: Tile -> Tile
tShrink (Tile l) = Tile $ tail $ init $ map (tail . init) l

tJoinV :: Tile -> Tile -> Tile
tJoinV (Tile t) (Tile b) = Tile $ t ++ b

tJoinH :: Tile -> Tile -> Tile
tJoinH (Tile l) (Tile r) = Tile $ zipWith (++) l r

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

extent :: [Pos] -> (Int, Int, Int, Int)
extent positions =
  ( minimum $ map fst positions
  , maximum $ map fst positions
  , minimum $ map snd positions
  , maximum $ map snd positions
  )

corners :: [Pos] -> [Pos]
corners positions = (,) <$> [minX, maxX] <*> [minY, maxY]
  where
    (minX, maxX, minY, maxY) = extent positions

layout :: Map.Map Pos (Int, Tile) -> [[Tile]]
layout placed = map (\y -> map (\x -> snd $ placed Map.! (x, y)) [minX..maxX]) [minY..maxY]
  where
    (minX, maxX, minY, maxY) = extent $ Map.keys placed

isMonster :: [[Bool]] -> Bool
isMonster l = case take 3 $ map (take 20) l of
  [[_   ,_   ,_,_,_   ,_   ,_   ,_   ,_,_,_   ,_   ,_   ,_   ,_,_,_   ,_   ,True,_   ],
   [True,_   ,_,_,_   ,True,True,_   ,_,_,_   ,True,True,_   ,_,_,_   ,True,True,True],
   [_   ,True,_,_,True,_   ,_   ,True,_,_,True,_   ,_   ,True,_,_,True,_   ,_   ,_   ]] -> True
  _ -> False

monsters :: [[Bool]] -> Int
monsters l = length $ filter isMonster $ tails l >>= transpose . map tails

solver :: Map.Map Int Tile -> IO ()
solver tiles = do
  let placed = placeAll tiles

  putStrLn ">> Part 1"
  let cornerIds = map (fst . (placed Map.!)) $ corners $ Map.keys placed
  print $ product cornerIds

  putStrLn ""
  putStrLn ">> Part 2"
  let bigTile = foldr1 tJoinV $ map (foldr1 tJoinH . map tShrink) $ layout placed
      monstersFound = maximum $ map (monsters . unTile) $ tVariations bigTile
      hashes = length $ filter id $ concat $ unTile bigTile
  print $ hashes - monstersFound * 15

  -- Pretty-printing
  putStrLn ""
  putStr $ unlines $ map (map $ bool '.' '#') $ unTile bigTile

day :: Day
day = dayParse parser solver
