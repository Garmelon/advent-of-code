{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D08
  ( day
  ) where

import qualified Data.Map  as Map
import qualified Data.Set  as Set

import           Aoc.Day
import           Aoc.Parse

data Opcode = Acc | Jmp | Nop
  deriving (Show, Eq, Ord)

data Instr = Instr Opcode Int
  deriving (Show, Eq, Ord)

parser :: Parser [Instr]
parser = manyLines $ Instr <$> (pOpcode <* char ' ') <*> signed (pure ()) decimal
  where
    pOpcode =  (Acc <$ string "acc") <|> (Jmp <$ string "jmp") <|> (Nop <$ string "nop")

data State = State
  { sInstrs :: Map.Map Int Instr
  , sIdx    :: Int
  , sAcc    :: Int
  } deriving (Show)

newState :: [Instr] -> State
newState instrs = State (Map.fromList $ zip [0..] instrs) 0 0

incIdx :: Int -> State -> State
incIdx delta s = s { sIdx = delta + sIdx s }

incAcc :: Int -> State -> State
incAcc delta s = s { sAcc = delta + sAcc s }

step :: State -> Maybe State
step s = do
  (Instr op val) <- sInstrs s Map.!? sIdx s
  pure $ case op of
    Acc -> incIdx 1 $ incAcc val s
    Jmp -> incIdx val s
    Nop -> incIdx 1 s

run :: State -> [State]
run s = s : case step s of
  Nothing -> []
  Just s' -> run s'

untilRepeatingIdx :: [State] -> [State]
untilRepeatingIdx = helper Set.empty
  where
    helper _ [] = []
    helper idxs (s:ss)
      | i `Set.member` idxs = []
      | otherwise           = s : helper (Set.insert i idxs) ss
      where i = sIdx s

solver :: [Instr] -> IO ()
solver instrs = do
  let s = newState instrs

  putStrLn ">> Part 1"
  print $ sAcc $ last $ untilRepeatingIdx $ run s

  putStrLn ""
  putStrLn ">> Part 2"

day :: Day
day = dayParse parser solver
