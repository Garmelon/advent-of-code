{-# LANGUAGE OverloadedStrings #-}

-- So this is the famous intcode I've been hearing so much about (i. e. somebody
-- mentioned it once somewhere). This is just a quick and relatively dirty
-- implementation. I plan on copy-pasting and improving this code whenever a new
-- day requires an intcode machine, instead of maintaining a single global
-- intcode machine implementation.

module Aoc.Y2019.A02
  ( solve201902
  ) where

import Control.Monad
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Memory = Memory { unmemory :: M.Map Int Int }

instance Show Memory where
  show mem = "Memory " <> show (memToList mem)

newMem :: [Int] -> Memory
newMem = Memory . M.fromList . zip [0..]

memToList :: Memory -> [Int]
memToList = map snd . M.toList . unmemory

readMem :: Int -> Memory-> Maybe Int
readMem addr (Memory mem) = mem M.!? addr

writeMem :: Int -> Int -> Memory -> Memory
writeMem addr val = Memory . M.insert addr val . unmemory

data State = State
  { stateMem :: Memory
  , stateIdx :: Int
  } deriving (Show)

newState :: Memory -> State
newState mem = State mem 0

increaseIdx :: State -> State
increaseIdx s = s{stateIdx = stateIdx s + 4}

data StepError
  = Exited
  | CouldNotRead Int -- addr
  | UnknownOpcode Int Int -- addr, opcode
  deriving (Show)

readAt :: State -> Int -> Either StepError Int
readAt s i = case readMem i $ stateMem s of
  Nothing -> Left $ CouldNotRead i
  Just v -> Right v

writeAt :: Int -> Int -> State -> State
writeAt addr val s = s{stateMem = writeMem addr val $ stateMem s}

step :: State -> Either StepError State
step s = do
  let idx = stateIdx s
  opcode <- readAt s idx
  case opcode of
    1 -> increaseIdx <$> opcodeWith (+) s
    2 -> increaseIdx <$> opcodeWith (*) s
    99 -> Left Exited
    _ -> Left $ UnknownOpcode idx opcode

opcodeWith :: (Int -> Int -> Int) -> State -> Either StepError State
opcodeWith f s = do
  let idx = stateIdx s
  addr1 <- readAt s $ idx + 1
  addr2 <- readAt s $ idx + 2
  val1 <- readAt s addr1
  val2 <- readAt s addr2
  target <- readAt s $ idx + 3
  pure $ writeAt target (f val1 val2) s

run :: State -> (State, StepError)
run s = case step s of
  Left e -> (s, e)
  Right s' -> run s'

patch :: Int -> Int -> Memory -> Memory
patch noun verb = writeMem 2 verb . writeMem 1 noun

solve201902 :: FilePath -> IO ()
solve201902 f = do
  values <- map (read . T.unpack) . T.splitOn "," <$> T.readFile f
  let mem = newMem values

  putStrLn ">> Part 1"
  let (s1, _) = run $ newState $ patch 12 2 mem
  putStrLn $ "Value at position 0: " <> show (readMem 0 $ stateMem s1)

  putStrLn ">> Part 2"
  let attempts = [(noun, verb) | noun <- [0..99], verb <- [0..99]]
  for_ attempts $ \(noun, verb) -> do
    let (s2, _) = run $ newState $ patch noun verb mem
        Just result = readMem 0 $ stateMem s2
    when (result == 19690720) $
      putStrLn $ "100 * noun + verb = " <> show (100 * noun + verb)
