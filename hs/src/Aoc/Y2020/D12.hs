{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D12
  ( day
  ) where

import           Data.Foldable
import           Data.List

import           Aoc.Day
import           Aoc.Parse

data Pos = Pos Int Int
  deriving (Show)

data Dir = North | East | South | West
  deriving (Show)

data Rot = RLeft | RRight | RFlip
  deriving (Show)

data Move
  = MTranslate Dir Int
  | MForward Int
  | MRotate Rot
  deriving (Show)

parser :: Parser [Move]
parser = manyLines
  $   (MTranslate <$> pDir <*> decimal)
  <|> (MForward   <$> (char 'F' *> decimal))
  <|> (MRotate    <$> pRot)
  where
    pDir = foldr1 (<|>) [North <$ char 'N', East <$ char 'E', South <$ char 'S', West <$ char 'W']
    pRot = foldr1 (<|>) [ RLeft  <$ (string "L90"  <|> string "R270")
                        , RRight <$ (string "L270" <|> string "R90")
                        , RFlip  <$ (string "L180" <|> string "R180")
                        ]

add :: Pos -> Pos -> Pos
add (Pos x1 y1) (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)

manhattan :: Pos -> Int
manhattan (Pos x y) = abs x + abs y

dirToPos :: Dir -> Int -> Pos
dirToPos North d = Pos   0 (-d)
dirToPos East  d = Pos   d   0
dirToPos South d = Pos   0   d
dirToPos West  d = Pos (-d)  0

rotate :: Rot -> Dir -> Dir
rotate RRight North = East
rotate RRight East  = South
rotate RRight South = West
rotate RRight West  = North
rotate RLeft  North = West
rotate RLeft  East  = North
rotate RLeft  South = East
rotate RLeft  West  = South
rotate RFlip  North = South
rotate RFlip  East  = West
rotate RFlip  South = North
rotate RFlip  West  = East

data State = State Pos Dir
  deriving (Show)

step :: Move -> State -> State
step (MTranslate dir steps) (State spos sdir) = State (add spos $ dirToPos dir steps) sdir
step (MForward steps)       (State spos sdir) = State (add spos $ dirToPos sdir steps) sdir
step (MRotate rot)          (State spos sdir) = State spos (rotate rot sdir)

run :: [Move] -> State
run moves = foldr (flip (.) . step) id moves $ State (Pos 0 0) East

solver :: [Move] -> IO ()
solver moves = do
  putStrLn ">> Part 1"
  let (State pos1 _) = run moves
  print $ manhattan pos1

day :: Day
day = dayParse parser solver
