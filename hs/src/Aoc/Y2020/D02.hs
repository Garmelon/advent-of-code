module Aoc.Y2020.D02
  ( day
  ) where

import qualified Data.Text as T

import           Aoc.Day
import           Aoc.Parse

data Line = Line
  { lMin  :: Int
  , lMax  :: Int
  , lChar :: Char
  , lPw   :: T.Text
  }

parser :: Parser [Line]
parser = manyLines $ Line
  <$> (decimal <* char '-')
  <*> (decimal <* space)
  <*> (anySingle <* char ':' <* space)
  <*> line

validCount :: Line -> Bool
validCount l = n >= lMin l && n <= lMax l
  where
    n = T.count (T.singleton $  lChar l) $ lPw l

validPositions :: Line -> Bool
validPositions l = (left == lChar l) /= (right == lChar l)
  where
    left = T.index (lPw l) (lMin l - 1)
    right = T.index (lPw l) (lMax l - 1)

solver :: [Line] -> IO ()
solver ls = do
  putStrLn ">> Part 1"
  print $ length $ filter validCount ls

  putStrLn ""
  putStrLn ">> Part 2"
  print $ length $ filter validPositions ls

day :: Day
day = dayParse parser solver
