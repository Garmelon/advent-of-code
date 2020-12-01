{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aoc.Y2019.A05
  ( solve201905
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Basic types

-- Typesafe addresses and values so we don't confuse the two unless we want to.

newtype Addr = Addr Integer
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype Value = Value Integer
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

-- Memory

newtype Memory = Memory { unmemory :: M.Map Addr Value }

instance Show Memory where
  show mem = "Memory " <> show (memToList mem)

newMem :: [Integer] -> Memory
newMem = Memory . M.fromList . map (bimap Addr Value) . zip [0..]

memToList :: Memory -> [Integer]
memToList (Memory m) =
  let maxAddr = fromMaybe 0 $ S.lookupMax $ M.keysSet m
  in  map (toInteger . fromMaybe 0 . (m M.!?)) [0..maxAddr]

readMem :: Addr -> Memory -> Maybe Value
readMem addr (Memory mem) = mem M.!? addr

writeMem :: Addr -> Value -> Memory -> Memory
writeMem addr val = Memory . M.insert addr val . unmemory

-- State

data State = State
  { stateMem :: Memory
  , stateIdx :: Addr
  , stateInput :: [Integer]
  } deriving (Show)

newState :: Memory -> State
newState mem = State mem 0 []

data StepError
  = Halted
  | CouldNotRead Addr
  | UnknownOpcode Integer
  deriving (Show)

readAt :: State -> Addr -> Either StepError Value
readAt s i = case readMem i $ stateMem s of
  Nothing -> Left $ CouldNotRead i
  Just v -> Right v

writeAt :: Addr -> Value -> State -> State
writeAt addr val s = s{stateMem = writeMem addr val $ stateMem s}

-- Opcode

data ParamMode = PositionMode | ImmediateMode
  deriving (Show, Eq)

digits :: Integer -> [Integer]
digits i = (i `mod` 10) : digits (i `div` 10)

pmFromDigit :: Integer -> ParamMode
pmFromDigit 0 = PositionMode
pmFromDigit 1 = ImmediateMode
pmFromDigit _ = undefined

paramModes :: Integer -> [ParamMode]
paramModes i = map pmFromDigit $ drop 2 $ (digits i ++ repeat 0)

data Operand = Direct Value | Indirect Addr
  deriving (Show)

data Opcode
  = OpAdd Operand Operand Addr
  | OpMul Operand Operand Addr
  | OpInput Addr
  | OpOutput Operand Addr
  | OpHalt
  deriving (Show)

opWidth :: Opcode -> Addr
opWidth (OpAdd    _ _ _) = 4
opWidth (OpMul    _ _ _) = 4
opWidth (OpInput  _)     = 2
opWidth (OpOutput _ _)   = 3
opWidth  OpHalt          = 1

parseOpcode :: State -> Either StepError Opcode
parseOpcode s = do
  let idx = stateIdx s
  undefined

solve201905 :: FilePath -> IO ()
solve201905 f = do
  stuff <- readFile f

  putStrLn ">> Part 1"

  putStrLn ">> Part 2"
