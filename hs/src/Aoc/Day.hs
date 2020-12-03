module Aoc.Day
  ( Day(..)
  , dayPure
  , dayFile
  , dayString
  , dayText
  , dayParse
  ) where

import           Control.Monad

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Text.Megaparsec

import           Aoc.Parse

data Day
  = DayPure String (IO ())
  | DayFile String (FilePath -> IO ())

dayPure :: String -> IO () -> Day
dayPure = DayPure

dayFile :: String -> (FilePath -> IO ()) -> Day
dayFile = DayFile

dayString :: String -> (String -> IO ()) -> Day
dayString name f = dayFile name $ f <=< readFile

dayText :: String -> (T.Text -> IO ()) -> Day
dayText name f = dayFile name $ f <=< T.readFile

dayParse :: String -> Parser a -> (a -> IO ()) -> Day
dayParse name p f = dayFile name $ \path -> do
  text <- T.readFile path
  case parse p path text of
    Right a -> f a
    Left e  -> putStrLn $ errorBundlePretty e
