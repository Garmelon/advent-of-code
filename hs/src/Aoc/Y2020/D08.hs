{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D08
  ( day
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Data.Maybe

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

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

data Machine = Machine
  { mInstrs  :: Map.Map Int Instr
  , mHaltIdx :: Int
  , mVisited :: Set.Set Int
  , mIdx     :: Int
  , mAcc     :: Int
  } deriving (Show)

newMachine :: [Instr] -> Machine
newMachine instrs = Machine
  { mInstrs = Map.fromList $ zip [0..] instrs
  , mHaltIdx = length instrs
  , mVisited = Set.empty
  , mIdx = 0
  , mAcc = 0
  }

data StepError
  = InvalidIdx Int
  | DuplicatedIdx Int
  | Halted
  deriving (Show, Eq)

type RunM = ExceptT StepError (State Machine)

visit :: Int -> RunM ()
visit idx = lift $ modify $ \m -> m { mVisited = Set.insert idx $ mVisited m }

incIdx :: Int -> RunM ()
incIdx delta = lift $ modify $ \m -> m { mIdx = delta + mIdx m }

incAcc :: Int -> RunM ()
incAcc delta = lift $ modify $ \m -> m { mAcc = delta + mAcc m}

getInstr :: RunM Instr
getInstr = do
  m <- lift get
  let idx = mIdx m
  when (idx == mHaltIdx m) $ throwE Halted
  when (idx `Set.member` mVisited m) $ throwE $ DuplicatedIdx idx
  case mInstrs m Map.!? idx of
    Nothing    -> throwE $ InvalidIdx idx
    Just instr -> visit idx $> instr

step :: RunM ()
step = do
  (Instr op val) <- getInstr
  case op of
    Acc -> incAcc val *> incIdx 1
    Jmp -> incIdx val
    Nop -> incIdx 1

run :: Machine -> (Machine, StepError)
run m = case runState (runExceptT (forever step)) m of
  (Left e, m') -> (m', e)
  (Right _, _) -> error "infinite loop was not infinite"

variations :: Machine -> [Machine]
variations m = do
  let instrs = mInstrs m
      maxIdx = maximum $ Map.keys instrs
  idx <- [0 .. maxIdx - 1]
  (Instr op val) <- maybeToList $ instrs Map.!? idx
  newOp <- case op of
    Acc -> []
    Jmp -> pure Nop
    Nop -> pure Jmp
  let newInstrs = Map.insert idx (Instr newOp val) instrs
  pure m { mInstrs = newInstrs}

solver :: [Instr] -> IO ()
solver instrs = do
  let m = newMachine instrs

  putStrLn ">> Part 1"
  let (m1, e1) = run m
  print e1
  print $ mAcc m1

  putStrLn ""
  putStrLn ">> Part 2"
  let m2 = head [m' | (m', Halted) <- run <$> variations m]
  print Halted
  print $ mAcc m2

day :: Day
day = dayParse parser solver
