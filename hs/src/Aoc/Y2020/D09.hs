{-# LANGUAGE RecordWildCards #-}

module Aoc.Y2020.D09
  ( day
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.Maybe

import qualified Data.Sequence as Seq

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [Int]
parser = manyLines decimal

splitAndGroup :: Int -> [Int] -> [([Int], Int)]
splitAndGroup width
  = map (\l -> (take width l, l !! width))
  . filter ((> width) . length)
  . tails

isValid :: [Int] -> Int -> Bool
isValid nums n = not $ null $ do
  a <- nums
  b <- nums
  guard $ a /= b
  guard $ a + b == n
  pure ()

data State = State
  { sTarget :: Int
  , sSeq    :: Seq.Seq Int
  , sSum    :: Int
  , sRest   :: [Int]
  } deriving (Show)

newState :: Int -> [Int] -> State
newState target = State target Seq.empty 0

step :: State -> Either (Maybe [Int]) State
step s@State{..} = case compare sSum sTarget of
  EQ -> Left $ Just $ toList sSeq
  GT -> case Seq.viewl sSeq of
    Seq.EmptyL  -> Left Nothing -- Should only happen if sTarget is negative
    l Seq.:< ls -> Right s{ sSeq = ls, sSum = sSum - l }
  LT -> case sRest of
    []     -> Left Nothing -- Can happen if no sequence of correct sum is found
    (r:rs) -> Right s{ sSeq = sSeq Seq.|> r, sSum = sSum + r, sRest = rs }

untilLeft :: (b -> Either a b) -> b -> a
untilLeft f a = either id (untilLeft f) $ f a

findRange :: Int -> [Int] -> Maybe [Int]
findRange target nums = untilLeft step $ newState target nums

solver :: [Int] -> IO ()
solver nums = do
  putStrLn ">> Part 1"
  let (_, invalidN) = head $ dropWhile (uncurry isValid) $ splitAndGroup 25 nums
  print invalidN

  putStrLn ""
  putStrLn ">> Part 2"
  let weakness = fromJust $ findRange invalidN nums
  print $ minimum weakness + maximum weakness

day :: Day
day = dayParse parser solver
