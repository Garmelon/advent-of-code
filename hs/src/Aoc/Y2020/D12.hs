{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D12
  ( day
  ) where

import           Data.Foldable

import           Aoc.Day
import           Aoc.Parse

data Pos = Pos Int Int
  deriving (Show)

data Rot = RLeft | RRight | RFlip
  deriving (Show)

data Move
  = MTranslate Pos Int
  | MForward Int
  | MRotate Rot
  deriving (Show)

parser :: Parser [Move]
parser = manyLines
  $   (MTranslate <$> pDir <*> decimal)
  <|> (MForward   <$> (char 'F' *> decimal))
  <|> (MRotate    <$> pRot)
  where
    pDir = foldr1 (<|>) [ Pos   0 (-1) <$ char 'N'
                        , Pos   1   0  <$ char 'E'
                        , Pos   0   1  <$ char 'S'
                        , Pos (-1)  0  <$ char 'W'
                        ]
    pRot = foldr1 (<|>) [ RLeft  <$ (string "L90"  <|> string "R270")
                        , RRight <$ (string "L270" <|> string "R90")
                        , RFlip  <$ (string "L180" <|> string "R180")
                        ]

add :: Pos -> Pos -> Pos
add (Pos x1 y1) (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)

mul :: Int -> Pos -> Pos
mul a (Pos x y) = Pos (a * x) (a * y)

manhattan :: Pos -> Int
manhattan (Pos x y) = abs x + abs y

rotate :: Rot -> Pos -> Pos
rotate RRight (Pos x y) = Pos (-y)   x
rotate RLeft  (Pos x y) = Pos   y  (-x)
rotate RFlip  (Pos x y) = Pos (-x) (-y)

data State1 = State1 Pos Pos -- Ship, direction
  deriving (Show)

step1 :: Move -> State1 -> State1
step1 (MTranslate dir steps) (State1 spos sdir) = State1 (add spos $ mul steps dir) sdir
step1 (MForward steps)       (State1 spos sdir) = State1 (add spos $ mul steps sdir) sdir
step1 (MRotate rot)          (State1 spos sdir) = State1 spos (rotate rot sdir)

data State2 = State2 Pos Pos -- Ship, waypoint
  deriving (Show)

step2 :: Move -> State2 -> State2
step2 (MTranslate dir steps) (State2 spos wp) = State2 spos (add wp $ mul steps dir)
step2 (MForward steps)       (State2 spos wp) = State2 (add spos $ mul steps wp) wp
step2 (MRotate rot)          (State2 spos wp) = State2 spos (rotate rot wp)

solver :: [Move] -> IO ()
solver moves = do
  putStrLn ">> Part 1"
  let initialState1 = State1 (Pos 0 0) (Pos 1 0)
      (State1 pos1 _) = foldl' (flip step1) initialState1 moves
  print $ manhattan pos1

  putStrLn ""
  putStrLn ">> Part 2"
  let initialState2 = State2 (Pos 0 0) (Pos 10 (-1))
      (State2 pos2 _) = foldl' (flip step2) initialState2 moves
  print $ manhattan pos2

day :: Day
day = dayParse parser solver
