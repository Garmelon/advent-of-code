{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D14
  ( day
  ) where

import           Control.Monad
import           Data.List

import qualified Data.Map      as Map

import           Aoc.Day
import           Aoc.Parse

data Instr
  = Mask [Maybe Bool]
  | Set Int Int

parser :: Parser [Instr]
parser = manyLines (pMask <|> pSet)
  where
    pMask = do
      void $ string "mask = "
      bits <- sequenceA $ replicate 36 $ (Nothing <$ char 'X') <|> (Just False <$ char '0') <|> (Just True <$ char '1')
      pure $ Mask $ reverse bits -- Most significant first
    pSet = do
      void $ string "mem["
      addr <- decimal
      void $ string "] = "
      Set addr <$> decimal

digits :: Int -> [Bool]
digits = map odd . iterate (`div` 2)

undigits :: [Bool] -> Int
undigits = sum . map fst . filter snd . zip (iterate (*2) 1)

mask1 :: [Maybe Bool] -> Int -> Int
mask1 m = undigits . zipWith maskBit m . digits
  where
    maskBit Nothing  = id
    maskBit (Just a) = const a

mask2 :: [Maybe Bool] -> Int -> [Int]
mask2 m = map undigits . sequenceA . zipWith maskBit m . digits
  where
    maskBit (Just False) b = [b]
    maskBit (Just True)  _ = [True]
    maskBit Nothing      _ = [False, True]

data Mem = Mem
  { mMask :: [Maybe Bool]
  , mMem  :: Map.Map Int Int
  } deriving (Show)

newMem :: Mem
newMem = Mem{mMask = replicate 36 Nothing, mMem = Map.empty}

setMask :: [Maybe Bool] -> Mem -> Mem
setMask m mem = mem{mMask = m}

setMem :: Int -> Int -> Mem -> Mem
setMem addr val mem = mem{mMem = Map.insert addr val $ mMem mem}

doInstr1 :: Instr -> Mem -> Mem
doInstr1 (Mask m)       mem = setMask m mem
doInstr1 (Set addr val) mem = setMem addr (mask1 (mMask mem) val) mem

doInstr2 :: Instr -> Mem -> Mem
doInstr2 (Mask m)       mem = setMask m mem
doInstr2 (Set addr val) mem = foldl' (\m a -> setMem a val m) mem $ mask2 (mMask mem) addr

solver :: [Instr] -> IO ()
solver instrs = do
  putStrLn ">> Part 1"
  let mem1 = foldl' (flip doInstr1) newMem instrs
  print $ sum $ mMem mem1

  putStrLn ""
  putStrLn ">> Part 2"
  let mem2 = foldl' (flip doInstr2) newMem instrs
  print $ sum $ mMem mem2

day :: Day
day = dayParse parser solver
