{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D16
  ( day
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.List

import qualified Data.Map       as Map
import qualified Data.Set       as Set
import qualified Data.Text      as T

import           Aoc.Day
import           Aoc.Parse

data Input = Input
  { iFields        :: Map.Map T.Text [(Int, Int)]
  , iMyTicket      :: [Int]
  , iNearbyTickets :: [[Int]]
  } deriving (Show)

parser :: Parser Input
parser = do
  fields <- Map.fromList <$> many (fieldLine <* newline)
  void $ string "\nyour ticket:\n"
  myTicket <- ticket <* newline
  void $ string "\nnearby tickets:\n"
  nearbyTickets <- many (ticket <* newline)
  pure $ Input fields myTicket nearbyTickets
  where
    fieldLine = do
      name <- takeWhileP Nothing $ \c -> (c /= ':') && (c /= '\n')
      void $ string ": "
      ranges <- sepBy ((,) <$> (decimal <* string "-") <*> decimal) (string " or ")
      pure (name, ranges)
    ticket = decimal `sepBy` string ","

valid :: Int -> [(Int, Int)] -> Bool
valid n = any (\(a, b) -> a <= n && n <= b)

findValid :: Map.Map T.Text [(Int, Int)] -> Int -> Set.Set T.Text
findValid fields n = Set.fromList $ map fst $ filter (valid n . snd) $ Map.toList fields

anyValid :: Map.Map T.Text [(Int, Int)] -> Int -> Bool
anyValid fields = not . Set.null . findValid fields

anyOf :: [a] -> [(a, [a])]
anyOf []     = []
anyOf (a:as) = (a, as) : map (second (a:)) (anyOf as)

findFields :: [(a, Set.Set T.Text)] -> [[(a, T.Text)]]
findFields [] = pure []
findFields variants = do
  ((index, names), rest) <- anyOf variants
  case Set.toList names of
    [name] -> do
      let rest' = map (second $ Set.delete name) rest
      ((index, name) :) <$> findFields rest'
    _ -> []

solver :: Input -> IO ()
solver input = do
  let Input fields ownTicket nearbyTickets = input

  putStrLn ">> Part 1"
  print $ sum $ filter (not . anyValid fields) $ concat nearbyTickets

  putStrLn ""
  putStrLn ">> Part 2"
  let validTickets = filter (all (anyValid fields)) nearbyTickets
      possibleFields = map (foldr1 Set.intersection) $ transpose $ map (map (findValid fields)) validTickets
      actualFields = Map.fromList $ head $ findFields $ zip [(0::Int)..] possibleFields
      namedOwnValues = zipWith (\i v -> (actualFields Map.! i, v)) [0..] ownTicket
      relevantOwnValues = map snd $ filter (T.isPrefixOf "departure" . fst) namedOwnValues
  print $ product relevantOwnValues

day :: Day
day = dayParse parser solver
