{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2019.A03
  ( solve201903
  ) where

import           Data.List
import           Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Read (readMaybe)

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

-- Reading input

readDir :: Char -> Maybe Dir
readDir 'U' = Just U
readDir 'D' = Just D
readDir 'L' = Just L
readDir 'R' = Just R
readDir _ = Nothing

readStep :: String -> Maybe Step
readStep [] = Nothing
readStep (d:n) = Step <$> readDir d <*> readMaybe n

readSteps :: T.Text -> Maybe [Step]
readSteps = traverse (readStep . T.unpack) . T.splitOn ","

solve201903 :: FilePath -> IO ()
solve201903 f = do
  text <- T.readFile f
  let [steps1, steps2] = mapMaybe readSteps $ T.lines text
      ixs = intersections steps1 steps2

  putStrLn ">> Part 1"
  print $ manhattan $ fst $ head $ sortOn (manhattan . fst) ixs

  putStrLn ">> Part 2"
  print $ snd $ head $ sortOn snd ixs
