{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D07
  ( day
  ) where

import Control.Monad
import Data.Maybe

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as T

import           Aoc.Day
import           Aoc.Parse

type BagName = T.Text

data BagCount = BagCount
  { bcName :: BagName
  , bcNum :: Int
  } deriving (Show)

type BagMap = Map.Map BagName [BagCount]

pBagName :: Parser T.Text
pBagName = do
  first <- word
  void $ char ' '
  second <- word
  void $ char ' '
  void $ string "bags" <|> string "bag"
  pure $ first <> " " <> second

pBagCount :: Parser BagCount
pBagCount = do
  n <- decimal
  void $ char ' '
  name <- pBagName
  pure $ BagCount name n

parser :: Parser BagMap
parser = fmap Map.fromList $ manyLines $ do
  name <- pBagName
  void $ string " contain "
  bags <- ([] <$ string "no other bags") <|> (pBagCount `sepBy1` string ", ")
  void $ char '.'
  pure (name, bags)

children :: BagMap -> BagName -> Set.Set BagName
children m b =
  let ch = map bcName $ fromMaybe [] $ m Map.!? b
  in  foldr Set.union (Set.fromList ch) $ map (children m) ch

countChildren :: BagMap -> BagName -> Int
countChildren m b =
  let ch = fromMaybe [] $ m Map.!? b
  in  sum $ map (\(BagCount name num) -> num + num * countChildren m name) ch

myBag :: BagName
myBag = "shiny gold"

solver :: BagMap -> IO ()
solver bags = do
  putStrLn ">> Part 1"
  print $ length $ filter (Set.member myBag) $ map (children bags) $ Set.toList $ Map.keysSet bags

  putStrLn ""
  putStrLn ">> Part 2"
  print $ countChildren bags myBag

day :: Day
day = dayParse parser solver
