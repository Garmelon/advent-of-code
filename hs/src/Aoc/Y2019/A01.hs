module Aoc.Y2019.A01
  ( solve201901
  ) where

fuel :: Integer -> Integer
fuel n = (n `div` 3) - 2

iteratedFuel :: Integer -> Integer
iteratedFuel = sum . takeWhile (> 0) . tail . iterate fuel

solve201901 :: FilePath -> IO ()
solve201901 f = do
  values <- map read . lines <$> readFile f

  putStrLn ">> Part 1"
  putStr "Total fuel: "
  print $ sum $ map fuel values

  putStrLn ">> Part 2"
  putStr "Total fuel (iterated): "
  print $ sum $ map iteratedFuel values
