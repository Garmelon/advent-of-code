module Aoc.Y2020.D23
  ( day
  ) where

import           Data.Sequence as Seq

import           Aoc.Day
import           Aoc.Parse

parser :: Parser (Seq Int)
parser = Seq.fromList <$> many digit <* newline

solver :: Seq Int -> IO ()
solver circle = do
  putStrLn ">> Part 1"
  print circle

day :: Day
day = dayParse parser solver
