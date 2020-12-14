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

mask :: [Maybe Bool] -> Int -> [Bool]
mask m n = zipWith maskBit m $ digits n
  where
    maskBit Nothing  = id
    maskBit (Just a) = const a

data Mem = Mem
  { mMask :: [Maybe Bool]
  , mMem  :: Map.Map Int [Bool]
  } deriving (Show)

newMem :: Mem
newMem = Mem{mMask = replicate 36 Nothing, mMem = Map.empty}

setMask :: [Maybe Bool] -> Mem -> Mem
setMask m mem = mem{mMask = m}

setMem :: Int -> Int -> Mem -> Mem
setMem addr val mem = mem{mMem = Map.insert addr masked $ mMem mem}
  where
    masked = mask (mMask mem) val

doInstr :: Instr -> Mem -> Mem
doInstr (Mask m)       = setMask m
doInstr (Set addr val) = setMem addr val

value :: [Bool] -> Int
value = sum . map fst . filter snd . zip (iterate (*2) 1)

solver :: [Instr] -> IO ()
solver instrs = do
  putStrLn ">> Part 1"
  let mem = foldl' (flip doInstr) newMem instrs
  print $ sum $ map value $ Map.elems $ mMem mem

day :: Day
day = dayParse parser solver
