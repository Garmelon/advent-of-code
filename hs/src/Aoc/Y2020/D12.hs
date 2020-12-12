{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2020.D12
  ( day
  ) where

import           Data.Foldable

import           Aoc.Day
import           Aoc.Parse

data Pos = Pos Int Int
data Rot = RLeft | RRight | RFlip

data Move
  = MTranslate Pos Int
  | MForward Int
  | MRotate Rot

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

data State = State Pos Pos -- Ship, direction/waypoint

step1 :: Move -> State -> State
step1 (MTranslate dir steps) (State spos sdir) = State (add spos $ mul steps dir) sdir
step1 (MForward steps)       (State spos sdir) = State (add spos $ mul steps sdir) sdir
step1 (MRotate rot)          (State spos sdir) = State spos (rotate rot sdir)

step2 :: Move -> State -> State
step2 (MTranslate dir steps) (State spos wp) = State spos (add wp $ mul steps dir)
step2 (MForward steps)       (State spos wp) = State (add spos $ mul steps wp) wp
step2 (MRotate rot)          (State spos wp) = State spos (rotate rot wp)

solver :: [Move] -> IO ()
solver moves = do
  putStrLn ">> Part 1"
  let initialState1 = State (Pos 0 0) (Pos 1 0)
      (State pos1 _) = foldl' (flip step1) initialState1 moves
  print $ manhattan pos1

  putStrLn ""
  putStrLn ">> Part 2"
  let initialState2 = State (Pos 0 0) (Pos 10 (-1))
      (State pos2 _) = foldl' (flip step2) initialState2 moves
  print $ manhattan pos2

day :: Day
day = dayParse parser solver
