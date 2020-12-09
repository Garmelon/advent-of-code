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

-- Only works if all numbers are positive.
findRange :: Int -> [Int] -> Maybe [Int]
findRange target = helper 0 Seq.empty
  where
    helper :: Int -> Seq.Seq Int -> [Int] -> Maybe [Int]
    helper sectionSum section rest = case compare sectionSum target of
      EQ -> Just $ toList section
      GT -> case Seq.viewl section of
        Seq.EmptyL  -> Nothing -- Should only happen if sTarget is negative
        l Seq.:< ls -> helper (sectionSum - l) ls rest
      LT -> case rest of
        []     -> Nothing -- Can happen if no sequence of correct sum is found
        (r:rs) -> helper (sectionSum + r) (section Seq.|> r) rs

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
