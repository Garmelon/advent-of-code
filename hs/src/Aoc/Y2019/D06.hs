{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2019.D06
  ( day
  ) where

import           Data.Maybe

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T

import           Aoc.Day
import           Aoc.Parse

data Tree = Tree T.Text [Tree]
  deriving (Show)

buildTree :: Map.Map T.Text [T.Text] -> T.Text -> Tree
buildTree m e = Tree e $ map (buildTree m) $ Map.findWithDefault [] e m

orbits :: Tree -> Int
orbits = helper 0
  where
    helper depth (Tree _ ts) = (depth +) $ sum $ map (helper $ depth + 1) ts

pathTo :: T.Text -> Tree -> Maybe [T.Text]
pathTo target (Tree name ts)
  | name == target = Just [name]
  | otherwise = case mapMaybe (pathTo target) ts of
      (x:_) -> Just $ name : x
      []    -> Nothing

differentParts :: (Eq a) => [a] -> [a] -> ([a], [a])
differentParts (a:as) (b:bs) | a == b = differentParts as bs
differentParts as bs = (as, bs)

parser :: Parser [(T.Text, T.Text)]
parser = manyLines $ (,) <$> (word <* char ')') <*> word

solver :: [(T.Text, T.Text)] -> IO ()
solver pairs = do
  let orbitMap = Map.fromListWith (++) $ map (\(a, b) -> (a, [b])) pairs
      roots = Set.fromList (map fst pairs) Set.\\ Set.fromList (concat $ Map.elems orbitMap)
      [tree] = map (buildTree orbitMap) $ Set.toList roots

  putStrLn ">> Part 1"
  print $ orbits tree

  putStrLn ""
  putStrLn ">> Part 2"
  let l = do
        p1 <- pathTo "YOU" tree
        p2 <- pathTo "SAN" tree
        let (p1', p2') = differentParts p1 p2
        -- Between orbiting bodies, not SAN and YOU themselves, hence the -2
        pure $ length p1' + length p2' - 2
  print l

day :: Day
day = dayParse parser solver
