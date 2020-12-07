{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D07
  ( day
  ) where

import           Control.Monad
import           Data.Maybe

import           Control.Monad.Trans.State
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Data.Text                 as T

import           Aoc.Day
import           Aoc.Parse

type BagName = T.Text

data BagCount = BagCount
  { bcName :: BagName
  , bcNum  :: Int
  } deriving (Show)

type BagMap = Map.Map BagName [BagCount]

pBagName :: Parser T.Text
pBagName = do
  name <- mconcat <$> sequenceA [word, string " ", word]
  void $ string " bags" <|> string " bag"
  pure name

pBagCount :: Parser BagCount
pBagCount = flip BagCount <$> (decimal <* char ' ') <*> pBagName

parser :: Parser BagMap
parser = fmap Map.fromList $ manyLines $ do
  name <- pBagName
  void $ string " contain "
  bags <- ([] <$ string "no other bags") <|> (pBagCount `sepBy1` string ", ")
  void $ char '.'
  pure (name, bags)

children :: BagMap -> BagName -> State (Map.Map BagName (Set.Set BagName)) (Set.Set BagName)
children m b = do
  sm <- get
  case sm Map.!? b of
    Just result -> pure result
    Nothing -> do
      let ch = map bcName $ fromMaybe [] $ m Map.!? b
      result <- Set.unions . (Set.fromList ch :) <$> traverse (children m) ch
      put $ Map.insert b result sm
      pure result

countChildren :: BagMap -> BagName -> Int
countChildren m b =
  let ch = fromMaybe [] $ m Map.!? b
  in  sum $ map (\(BagCount name num) -> num + num * countChildren m name) ch

myBag :: BagName
myBag = "shiny gold"

solver :: BagMap -> IO ()
solver bags = do
  putStrLn ">> Part 1"
  let childrenPerBag = flip evalState Map.empty $ traverse (children bags) $ Map.keys bags
  print $ length $ filter (Set.member myBag) childrenPerBag

  putStrLn ""
  putStrLn ">> Part 2"
  print $ countChildren bags myBag

day :: Day
day = dayParse parser solver
