module Aoc.Y2019.A01
  ( solve201901
  ) where

fuel :: Integer -> Integer
fuel n = (n `div` 3) - 2

iteratedFuel :: Integer -> Integer
iteratedFuel = sum . takeWhile (> 0) . tail . iterate fuel

solve201901 :: FilePath -> IO ()
solve201901 f = do
  values <- (map read . lines) <$> readFile f

  putStr "Total fuel: "
  print $ sum $ map fuel values

  putStr "Total fuel (iterated): "
  print $ sum $ map iteratedFuel values
