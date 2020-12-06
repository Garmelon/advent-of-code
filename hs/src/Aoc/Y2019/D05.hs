{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Aoc.Y2019.D05
  ( day
  ) where

import           Data.Bifunctor
import           Data.Maybe
import           Text.Read       (readMaybe)

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import           Aoc.Day
import           Aoc.Parse

-------------------
-- General types --
-------------------

newtype Addr = Addr Integer
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype Value = Value Integer
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

valToAddr :: Value -> Addr
valToAddr = fromIntegral

offsetAddr :: Addr -> Int -> Addr
offsetAddr a i = a + fromIntegral i

data StepError
  = Halted State
  | CouldNotRead Addr
  | UnknownOpcode Addr Integer
  | InvalidInput T.Text
  deriving (Show)

displayError :: StepError -> T.Text
displayError (Halted s)          = "Halted at " <> T.pack (show $ stateIdx s)
displayError (CouldNotRead a)    = "Could not read value at " <> T.pack (show a)
displayError (UnknownOpcode a i) = "Unknown opcode " <> T.pack (show i) <> " at " <> T.pack (show a)
displayError (InvalidInput t)    = "Invalid input: " <> t

type StepM = Either StepError

------------
-- Memory --
------------

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

-----------
-- State --
-----------

data State = State
  { stateMem :: Memory
  , stateIdx :: Addr
  } deriving (Show)

newState :: Memory -> State
newState mem = State mem 0

readAt :: State -> Addr -> StepM Value
readAt s i = case readMem i $ stateMem s of
  Nothing -> Left $ CouldNotRead i
  Just v  -> Right v

writeAt :: State -> Addr -> Value -> State
writeAt s addr val = s{stateMem = writeMem addr val $ stateMem s}

-------------
-- Opcodes --
-------------

data ParamMode = PositionMode | ImmediateMode
  deriving (Show, Eq)

-- | Infinite list of param modes based on the digits of a number. The default
-- mode is 'PositionMode'.
paramModes :: Integer -> [ParamMode]
paramModes = map pmFromDigit . digits
  where
    digits i = (i `mod` 10) : digits (i `div` 10)
    pmFromDigit 0 = PositionMode
    pmFromDigit _ = ImmediateMode

data Operand = Direct Value | Indirect Addr
  deriving (Show)

pmToOp :: ParamMode -> Value -> Operand
pmToOp PositionMode  = Indirect . valToAddr
pmToOp ImmediateMode = Direct

data Opcode
  = OpAdd Operand Operand Addr      -- 1
  | OpMul Operand Operand Addr      -- 2
  | OpInput Addr                    -- 3
  | OpOutput Operand                -- 4
  | OpJumpIfTrue Operand Operand    -- 5
  | OpJumpIfFalse Operand Operand   -- 6
  | OpLessThan Operand Operand Addr -- 7
  | OpEquals Operand Operand Addr   -- 8
  | OpHalt                          -- 99
  deriving (Show)

--------------
-- Stepping --
--------------

-- Parsing opcodes

getOp :: State -> Addr -> [ParamMode] -> Int -> StepM Operand
getOp s a pms i = do
  let pm = pms !! i
  value <- readAt s $ offsetAddr a $ 1 + i
  pure $ pmToOp pm value

getAddr :: State -> Addr -> Int -> StepM Addr
getAddr s a i = valToAddr <$> readAt s (offsetAddr a $ 1 + i)

parseOpcode :: State -> StepM Opcode
parseOpcode s = do
  let a = stateIdx s
  value <- toInteger <$> readAt s a
  let opcode = value `mod` 100
      pms = paramModes $ value `div` 100
      getOp' = getOp s a pms
      getAddr' = getAddr s a
  case opcode of
     1  -> OpAdd         <$> getOp'   0 <*> getOp' 1 <*> getAddr' 2
     2  -> OpMul         <$> getOp'   0 <*> getOp' 1 <*> getAddr' 2
     3  -> OpInput       <$> getAddr' 0
     4  -> OpOutput      <$> getOp'   0
     5  -> OpJumpIfTrue  <$> getOp'   0 <*> getOp' 1
     6  -> OpJumpIfFalse <$> getOp'   0 <*> getOp' 1
     7  -> OpLessThan    <$> getOp'   0 <*> getOp' 1 <*> getAddr' 2
     8  -> OpEquals      <$> getOp'   0 <*> getOp' 1 <*> getAddr' 2
     99 -> pure OpHalt
     _  -> Left $ UnknownOpcode a opcode

-- Executing opcodes

data StepResult
  = NormalStep State
  | InputStep (Integer -> State)
  | OutputStep Integer State

readOp :: State -> Operand -> StepM Value
readOp _ (Direct v)   = pure v
readOp s (Indirect a) = readAt s a

incIdx :: Int -> State -> State
incIdx i s = s{stateIdx = offsetAddr (stateIdx s) i}

setIdx :: Addr -> State -> State
setIdx i s = s{stateIdx = i}

binaryOp :: State -> Operand -> Operand -> Addr -> Int -> (Value -> Value -> Value) -> StepM StepResult
binaryOp s x y r i f = do
  vx <- readOp s x
  vy <- readOp s y
  pure $ NormalStep $ incIdx i $ writeAt s r $ f vx vy

jumpOp :: State -> Operand -> Operand -> Int -> (Value -> Bool) -> StepM StepResult
jumpOp s x t i f = do
  vx <- readOp s x
  NormalStep <$> if f vx
    then flip setIdx s . valToAddr <$> readOp s t
    else pure $ incIdx i s

execOpcode :: State -> Opcode -> StepM StepResult
execOpcode s (OpAdd x y r) = binaryOp s x y r 4 (+)
execOpcode s (OpMul x y r) = binaryOp s x y r 4 (*)
execOpcode s (OpInput r) = pure $ InputStep $ incIdx 2 . writeAt s r . fromInteger
execOpcode s (OpOutput x) = do
  vx <- readOp s x
  pure $ OutputStep (toInteger vx) $ incIdx 2 s
execOpcode s (OpJumpIfTrue x t) = jumpOp s x t 3 (/= 0)
execOpcode s (OpJumpIfFalse x t) = jumpOp s x t 3 (== 0)
execOpcode s (OpLessThan x y r) = binaryOp s x y r 4 $ \a b -> if a < b then 1 else 0
execOpcode s (OpEquals x y r) = binaryOp s x y r 4 $ \a b -> if a == b then 1 else 0
execOpcode s OpHalt = Left $ Halted s

step :: State -> Either StepError StepResult
step s = execOpcode s =<< parseOpcode s

run :: State -> IO StepError
run s = case step s of
  Left e -> pure e
  Right (NormalStep s') -> run s'
  Right (InputStep f) -> do
    putStr "?> "
    t <- T.getLine
    case readMaybe $ T.unpack t of
      Nothing -> pure $ InvalidInput t
      Just i  -> run $ f i
  Right (OutputStep o s') -> do
    putStrLn $ "-> " ++ show o
    run s'

runAndPrintResult :: State -> IO ()
runAndPrintResult s = do
  e <- run s
  T.putStrLn $ displayError e

parser :: Parser Memory
parser = newMem <$> (signed (pure ()) decimal `sepBy` char ',') <* newline

solver :: Memory -> IO ()
solver mem = do
  putStrLn ">> Part 1"
  putStrLn "Input: 1"
  runAndPrintResult $ newState mem

  putStrLn ""
  putStrLn ">> Part 2"
  putStrLn "Input: 5"
  runAndPrintResult $ newState mem

day :: Day
day = dayParse parser solver
