{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2019.D03
  ( day
  ) where

import           Data.List

import qualified Data.Map.Strict as M

import           Aoc.Day
import           Aoc.Parse

data Pos = Pos Int Int
  deriving (Show, Eq, Ord)

zeroPos :: Pos
zeroPos = Pos 0 0

addPos :: Pos -> Pos -> Pos
addPos (Pos x1 y1) (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)

manhattan :: Pos -> Int
manhattan (Pos x y) = abs x + abs y

data Dir = U | D | L | R
  deriving (Show)

asDelta :: Dir -> Pos
asDelta U = Pos   0   1
asDelta D = Pos   0 (-1)
asDelta L = Pos (-1)  0
asDelta R = Pos   1   0

data Step = Step Dir Int
  deriving (Show)

step :: Step -> Pos -> [Pos]
step (Step dir n) = take n . drop 1 . iterate (addPos delta)
  where delta = asDelta dir

steps :: Pos -> [Step] -> [Pos]
steps pos = concat . drop 1 . scanl' (flip step . last) [pos]

costs :: [Pos] -> M.Map Pos Int
costs = M.fromList . reverse . flip zip [1..]

intersections :: [Step] -> [Step] -> [(Pos, Int)]
intersections s1 s2 =
  let costs1 = costs $ steps zeroPos s1
      costs2 = costs $ steps zeroPos s2
  in  M.toList $ M.intersectionWith (+) costs1 costs2

parser :: Parser ([Step], [Step])
parser = do
  s1 <- (pStep `sepBy` char ',') <* newline
  s2 <- (pStep `sepBy` char ',') <* newline
  pure (s1, s2)
  where
    pStep = Step <$> pDir <*> decimal
    pDir = (U <$ char 'U') <|> (D <$ char 'D') <|> (L <$ char 'L') <|> (R <$ char 'R')

solver :: ([Step], [Step]) -> IO ()
solver (steps1, steps2) = do
  let ixs = intersections steps1 steps2

  putStrLn ">> Part 1"
  print $ manhattan $ fst $ head $ sortOn (manhattan . fst) ixs

  putStrLn ""
  putStrLn ">> Part 2"
  print $ snd $ head $ sortOn snd ixs

day :: Day
day = dayParse parser solver
