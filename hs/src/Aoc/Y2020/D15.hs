{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Strict             #-}

module Aoc.Y2020.D15
  ( day
  ) where

import qualified Data.Map  as Map

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [Int]
parser = (decimal `sepBy` char ',') <* newline

data State = State
  { sIdx      :: Int
  , sNum      :: Int
  , sLastSeen :: Map.Map Int Int
  } deriving (Show)

-- nums must not be empty
newState :: [Int] -> State
newState nums = State
  { sIdx = length nums - 1
  , sNum = last nums
  , sLastSeen = Map.fromList $ zip (init nums) [0..]
  }

step :: State -> State
step s =
  let newNum = case sLastSeen s Map.!? sNum s of
        Nothing  -> 0
        Just idx -> sIdx s - idx
  in  State
      { sIdx = sIdx s + 1
      , sNum = newNum
      , sLastSeen = Map.insert (sNum s) (sIdx s) $ sLastSeen s
      }

replicateF :: Int -> (a -> a) -> a -> a
replicateF n f a
  | n <= 0    = a
  | otherwise = replicateF (n - 1) f (f a)

stepUntil :: Int -> State -> IO State
stepUntil amount s = stepUntilIo (amount - sIdx s - 1) s

-- | A Helper for stepUntil that prints how many steps are left in regular
-- intervals. Don't call it directly.
stepUntilIo :: Int -> State -> IO State
stepUntilIo amount s | amount <= 0 = pure s
stepUntilIo amount s = do
  putStrLn $ show amount ++ " left"
  let width = min amount 100_000
  stepUntilIo (amount - width) $ replicateF width step s

solver :: [Int] -> IO ()
solver nums = do
  let s = newState nums

  putStrLn ">> Part 1"
  p1 <- sNum <$> stepUntil 2020 s
  print p1

  putStrLn ""
  putStrLn ">> Part 2"
  p2 <- sNum <$> stepUntil 30_000_000 s
  print p2

  putStrLn ""
  putStrLn ">> Summary"
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

day :: Day
day = dayParse parser solver
