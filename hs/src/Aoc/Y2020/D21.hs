{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D21
  ( day
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.List
import           Data.Maybe

import qualified Data.Set       as Set
import qualified Data.Text      as T
import qualified Data.Text.IO   as T

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [(Set.Set T.Text, Set.Set T.Text)]
parser = manyLines $ do
  ingredients <- Set.fromList <$> many (lineWhile isAlpha <* string " ")
  void $ string "(contains "
  allergens <- Set.fromList <$> (lineWhile isAlpha `sepBy` string ", ")
  void $ string ")"
  pure (ingredients, allergens)

data State = State
  { sAllergens :: [(T.Text, [T.Text])]
  , sKnown     :: [(T.Text, T.Text)]
  } deriving (Show)

newState :: [(T.Text, Set.Set T.Text)] -> State
newState allergens = State (map (second Set.toList) allergens) []

step :: State -> Maybe State
step s = listToMaybe $ do
  (allergen, [food]) <- sAllergens s
  let allergens = filter (not . null . snd) $ map (second (delete food)) $ sAllergens s
      known = (food, allergen) : sKnown s
  pure $ State allergens known

whileJust :: (a -> Maybe a) -> a -> a
whileJust f a = maybe a (whileJust f) $ f a

solver :: [(Set.Set T.Text, Set.Set T.Text)] -> IO ()
solver foods = do
  putStrLn ">> Part 1"
  let allFoods = Set.unions $ map fst foods
      allAllergens = Set.toList $ Set.unions $ map snd foods
      foodsByAllergen = map (\a -> (a, foldr1 Set.intersection $ map fst $ filter (Set.member a . snd) foods)) allAllergens
      foodsWithAllergen = Set.unions $ map snd foodsByAllergen
      foodsWithoutAllergen = allFoods Set.\\ foodsWithAllergen
  print $ sum $ map (Set.size . Set.intersection foodsWithoutAllergen . fst) foods

  putStrLn ""
  putStrLn ">> Part 2"
  let known = sKnown $ whileJust step $ newState foodsByAllergen
  T.putStrLn $ T.intercalate "," $ map fst $ sortOn snd known

day :: Day
day = dayParse parser solver
