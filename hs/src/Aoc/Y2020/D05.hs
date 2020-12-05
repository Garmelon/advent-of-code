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
  fbs <- count 7 $ (False <$ char 'F') <|> (True <$ char 'B')
  lrs <- count 3 $ (False <$ char 'L') <|> (True <$ char 'R')
  pure Pass { fb = fbs, lr = lrs }

calcNum :: [Bool] -> Int
calcNum bs = sum $ zipWith (*) (iterate (*2) 1) (map fromEnum $ reverse bs)

seatId :: Pass -> Int
seatId p = 8 * calcNum (fb p) + calcNum (lr p)

allSeats :: [Pass]
allSeats = Pass
  <$> sequenceA (replicate 7 [False, True])
  <*> sequenceA (replicate 3 [False, True])

mySeats :: Set.Set Int -> [Int]
mySeats taken = do
  sid <- seatId <$> allSeats
  guard $ (sid - 1) `Set.member` taken
  guard $ not $ sid `Set.member` taken
  guard $ (sid + 1) `Set.member` taken
  pure sid

solver :: [Pass] -> IO ()
solver ps = do
  putStrLn ">> Part 1"
  print $ maximum $ map seatId ps

  putStrLn ""
  putStrLn ">> Part 2"
  let taken = Set.fromList $ map seatId ps
  print $ mySeats taken

day :: Day
day = dayParse "2020_05" parser solver
