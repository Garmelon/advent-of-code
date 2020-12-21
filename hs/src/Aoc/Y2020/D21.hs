{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D21
  ( day
  ) where

import           Control.Monad
import           Data.Char

import qualified Data.Set      as Set
import qualified Data.Text     as T

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [(Set.Set T.Text, Set.Set T.Text)]
parser = manyLines $ do
  ingredients <- Set.fromList <$> many (lineWhile isAlpha <* string " ")
  void $ string "(contains "
  allergens <- Set.fromList <$> (lineWhile isAlpha `sepBy` string ", ")
  void $ string ")"
  pure (ingredients, allergens)

solver :: [(Set.Set T.Text, Set.Set T.Text)] -> IO ()
solver foods = do
  putStrLn ">> Part 1"
  let allFoods = Set.unions $ map fst foods
      allAllergens = Set.toList $ Set.unions $ map snd foods
      foodsByAllergen = map (\a -> (a, foldr1 Set.intersection $ map fst $ filter (Set.member a . snd) foods)) allAllergens
      foodsWithAllergen = Set.unions $ map snd foodsByAllergen
      foodsWithoutAllergen = allFoods Set.\\ foodsWithAllergen
  print $ sum $ map (Set.size . Set.intersection foodsWithoutAllergen . fst) foods

day :: Day
day = dayParse parser solver
