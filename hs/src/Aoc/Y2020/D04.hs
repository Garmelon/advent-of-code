{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Aoc.Y2020.D04
  ( day
  ) where

import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.Void

import qualified Data.Text     as T

import           Aoc.Day
import           Aoc.Parse

data Field a = None | Invalid T.Text | Valid a
  deriving (Show)

isPresent :: Field a -> Bool
isPresent None        = False
isPresent (Invalid _) = True
isPresent (Valid _)   = True

isValid :: Field a -> Bool
isValid None        = False
isValid (Invalid _) = False
isValid (Valid _)   = True

instance Semigroup (Field a) where
  _         <> Valid a   = Valid a
  Valid a   <> _         = Valid a
  _         <> Invalid t = Invalid t
  Invalid t <> _         = Invalid t
  None      <> None      = None

instance Monoid (Field a) where
  mempty = None

data Height = Cm Int | In Int
  deriving (Show)

data Passport = Passport
  { byr :: Field Int
  , iyr :: Field Int
  , eyr :: Field Int
  , hgt :: Field Height
  , hcl :: Field T.Text
  , ecl :: Field T.Text
  , pid :: Field Int
  , cid :: Field Void
  } deriving (Show)

instance Semigroup Passport where
  p1 <> p2 = Passport
    { byr = byr p1 <> byr p2
    , iyr = iyr p1 <> iyr p2
    , eyr = eyr p1 <> eyr p2
    , hgt = hgt p1 <> hgt p2
    , hcl = hcl p1 <> hcl p2
    , ecl = ecl p1 <> ecl p2
    , pid = pid p1 <> pid p2
    , cid = cid p1 <> cid p2
    }

instance Monoid Passport where
  mempty = Passport mempty mempty mempty mempty mempty mempty mempty mempty

mapFieldsExceptCid :: (forall a. Field a -> b) -> Passport -> [b]
mapFieldsExceptCid f p = [f $ byr p, f $ iyr p, f $ eyr p, f $ hgt p, f $ hcl p, f $ ecl p, f $ pid p]

pField :: T.Text -> Parser a -> Parser (Field a)
pField name p = do
  notFollowedBy spaceChar
  void $ string name
  void $ char ':'
  (Valid <$> try p) <|> (Invalid <$> lineUntil isSpace)

nDigits :: Int -> Parser Int
nDigits n = do
  digits <- takeWhileP (Just "digit") isDigit
  guard $ T.length digits == n
  pure $ read $ T.unpack digits

nDigitsBetween :: Int -> Int -> Int -> Parser Int
nDigitsBetween n low high = do
  i <- nDigits n
  guard $ low <= i && i <= high
  pure i

pByr :: Parser Passport
pByr = do
  f <- pField "byr" $ nDigitsBetween 4 1920 2002
  pure mempty{byr = f}

pIyr :: Parser Passport
pIyr = do
  f <- pField "iyr" $ nDigitsBetween 4 2010 2020
  pure mempty{iyr = f}

pEyr :: Parser Passport
pEyr = do
  f <- pField "eyr" $ nDigitsBetween 4 2020 2030
  pure mempty{eyr = f}

pHgt :: Parser Passport
pHgt = do
  f <- pField "hgt" $ do
    i <- decimal
    (string "cm" *> guard (150 <= i && i <= 193) $> Cm i) <|> (string "in" *> guard (59 <= i && i <= 76) $> In i)
  pure mempty{hgt = f}

pHcl :: Parser Passport
pHcl = do
  f <- pField "hcl" $ do
    void $ char '#'
    t <- lineUntil isSpace
    guard $ T.length t == 6 && T.all isHexDigit t
    pure t
  pure mempty{hcl = f}

pEcl :: Parser Passport
pEcl = do
  f <- pField "ecl" $ foldr1 (<|>) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  pure mempty{ecl = f}

pPid :: Parser Passport
pPid = do
  f <- pField "pid" $ nDigits 9
  pure mempty{pid = f}

pCid :: Parser Passport
pCid = do
  f <- pField "cid" $ fail "void"
  pure mempty{cid = f}

parser :: Parser [Passport]
parser = passport `sepBy` newline
  where
    passport = mconcat <$> field `endBy1` spaceChar
    field = pByr <|> pIyr <|> pEyr <|> pHgt <|> pHcl <|> pEcl <|> pPid <|> pCid

hasRequiredKeys :: Passport -> Bool
hasRequiredKeys = and . mapFieldsExceptCid isPresent

hasValidKeys :: Passport -> Bool
hasValidKeys = and . mapFieldsExceptCid isValid

solver :: [Passport] -> IO ()
solver passports = do
  putStrLn ">> Part 1"
  print $ length $ filter hasRequiredKeys passports

  putStrLn ""
  putStrLn ">> Part 2"
  print $ length $ filter hasValidKeys passports

day :: Day
day = dayParse parser solver
