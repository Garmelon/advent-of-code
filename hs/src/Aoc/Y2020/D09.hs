module Aoc.Y2020.D09
  ( day
  ) where

import           Control.Monad
import           Data.List

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

-- Fast enough to give me a result, but not linear time. I couldn't figure out a
-- way to implement the linear time algorithm that didn't look like a mess.
-- Maybe I'll have another go for the cleanup.
findRange :: Int -> [Int] -> [Int]
findRange n
  = head
  . filter ((== n) . sum)
  . map (head . dropWhile ((< n) . sum) . inits)
  . tails

solver :: [Int] -> IO ()
solver nums = do
  putStrLn ">> Part 1"
  let (_, invalidN) = head $ dropWhile (uncurry isValid) $ splitAndGroup 25 nums
  print invalidN

  putStrLn ""
  putStrLn ">> Part 2"
  let weakness = findRange invalidN nums
  print $ minimum weakness + maximum weakness

day :: Day
day = dayParse parser solver
