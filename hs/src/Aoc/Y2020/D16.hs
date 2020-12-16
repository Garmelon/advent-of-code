{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D16
  ( day
  ) where

import           Data.Bifunctor
import           Data.List

import qualified Data.Text      as T

import           Aoc.Day
import           Aoc.Parse

data Input = Input [(T.Text, Int -> Bool)] [Int] [[Int]]

parser :: Parser Input
parser = Input
  <$> many (field <* newline)
  <*> (string "\nyour ticket:\n" *> ticket)
  <*> (string "\nnearby tickets:\n" *> many ticket)
  where
    bound = around (string "-") decimal decimal
    field = do
      name <- lineUntil (==':') <* string ": "
      ((a, b), (c, d)) <- around (string " or ") bound bound
      pure (name, \n -> (a <= n && n <= b) || (c <= n && n <= d))
    ticket = (decimal `sepBy` string ",") <* newline

anyValid :: [(T.Text, Int -> Bool)] -> Int -> Bool
anyValid fields n = any (($ n) . snd) fields

anyOf :: [a] -> [(a, [a])]
anyOf []     = []
anyOf (a:as) = (a, as) : map (second (a:)) (anyOf as)

findFields :: [(a, [T.Text])] -> [[(a, T.Text)]]
findFields [] = pure []
findFields variants = do
  ((i, [name]), rest) <- anyOf variants
  rest2 <- findFields $ map (second $ delete name) rest
  pure $ (i, name) : rest2

solver :: Input -> IO ()
solver (Input fields ownTicket nearbyTickets) = do
  putStrLn ">> Part 1"
  print $ sum $ filter (not . anyValid fields) $ concat nearbyTickets

  putStrLn ""
  putStrLn ">> Part 2"
  let validTickets = filter (all (anyValid fields)) nearbyTickets
      possibleNames = map (\ns -> map fst $ filter (\(_, p) -> all p ns) fields) $ transpose validTickets
      actualNames = map snd $ sortOn fst $ head $ findFields $ zip [(0::Int)..] possibleNames
      relevantValues = map snd $ filter (T.isPrefixOf "departure" . fst) $ zip actualNames ownTicket
  print $ product relevantValues

day :: Day
day = dayParse parser solver
