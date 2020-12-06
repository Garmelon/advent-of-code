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
pmToOp PositionMode  = Indirect . fromIntegral
pmToOp ImmediateMode = Direct

data Opcode
  = OpAdd Operand Operand Addr
  | OpMul Operand Operand Addr
  | OpInput Addr
  | OpOutput Operand
  | OpHalt
  deriving (Show)

--------------
-- Stepping --
--------------

-- Parsing opcodes

getOp :: State -> Addr -> [ParamMode] -> Int -> StepM Operand
getOp s a pms i = do
  let pm = pms !! i
  value <- readAt s $ a + 1 + fromIntegral i
  pure $ pmToOp pm value

getAddr :: State -> Addr -> Int -> StepM Addr
getAddr s a i = fromIntegral <$> readAt s (a + 1 + fromIntegral i)

parseOpcode :: State -> StepM Opcode
parseOpcode s = do
  let a = stateIdx s
  value <- toInteger <$> readAt s a
  let opcode = value `mod` 100
      pms = paramModes $ value `div` 100
  case opcode of
     1  -> OpAdd <$> getOp s a pms 0 <*> getOp s a pms 1 <*> getAddr s a 2
     2  -> OpMul <$> getOp s a pms 0 <*> getOp s a pms 1 <*> getAddr s a 2
     3  -> OpInput <$> getAddr s a 0
     4  -> OpOutput <$> getOp s a pms 0
     99 -> pure OpHalt
     _  -> Left $ UnknownOpcode a opcode

-- Executing opcodes

data StepResult
  = NormalStep State
  | InputStep (Integer -> State)
  | OutputStep State Integer

readOp :: State -> Operand -> StepM Value
readOp _ (Direct v)   = pure v
readOp s (Indirect a) = readAt s a

incIdx :: Int -> State -> State
incIdx i s = s{stateIdx = stateIdx s + fromIntegral i}

execOpcode :: State -> Opcode -> StepM StepResult
execOpcode s (OpAdd x y r) = do
  vx <- readOp s x
  vy <- readOp s y
  pure $ NormalStep $ incIdx 4 $ writeAt s r $ vx + vy
execOpcode s (OpMul x y r) = do
  vx <- readOp s x
  vy <- readOp s y
  pure $ NormalStep $ incIdx 4 $ writeAt s r $ vx * vy
execOpcode s (OpInput r) = pure $ InputStep $ incIdx 2 . writeAt s r . fromInteger
execOpcode s (OpOutput x) = do
  vx <- readOp s x
  pure $ OutputStep (incIdx 2 s) $ toInteger vx
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
  Right (OutputStep s' o) -> do
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
  runAndPrintResult $ newState mem

  putStrLn ""
  putStrLn ">> Part 2"

day :: Day
day = dayParse parser solver
