module Aoc.Y2020.D05
  ( day
  ) where

import           Control.Monad
import qualified Data.Set      as Set

import           Aoc.Day
import           Aoc.Parse

data Pass = Pass
  { fb :: [Bool]
  , lr :: [Bool]
  } deriving (Show, Eq, Ord)

parser :: Parser [Pass]
parser = manyLines $ do
  fbs <- sequenceA $ replicate 7 $ (False <$ char 'F') <|> (True <$ char 'B')
  lrs <- sequenceA $ replicate 3 $ (False <$ char 'L') <|> (True <$ char 'R')
  pure Pass { fb = fbs, lr = lrs }

calcNum :: [Bool] -> Int
calcNum bs = sum $ zipWith (*) (iterate (*2) 1) (map fromEnum $ reverse bs)

seatId :: Pass -> Int
seatId p = 8 * calcNum (fb p) + calcNum (lr p)

cols :: [[Bool]]
cols = sequenceA $ replicate 3 [False, True]

rows :: [[Bool]]
rows = sequenceA $ replicate 7 [False, True]

mySeats :: Set.Set Pass -> [Pass]
mySeats taken = do
  col <- cols
  (prevRow, row, nextRow) <- zip3 rows (drop 1 rows) (drop 2 rows)
  let prevPass = Pass prevRow col
      pass     = Pass row col
      nextPass = Pass nextRow col
  guard $ prevPass `Set.member` taken
  guard $ not $ pass `Set.member` taken
  guard $ nextPass `Set.member` taken
  pure pass

solver :: [Pass] -> IO ()
solver ps = do
  putStrLn ">> Part 1"
  print $ maximum $ map seatId ps

  putStrLn ""
  putStrLn ">> Part 2"
  let taken = Set.fromList ps
  print $ map seatId $ mySeats taken

day :: Day
day = dayParse "2020_05" parser solver
