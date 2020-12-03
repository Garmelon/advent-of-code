module Aoc.Day
  ( Day(..)
  , runDay
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

-- | Helper function for trying out days in ghci.
runDay :: Day -> FilePath -> IO ()
runDay (DayPure _ f) _ = f
runDay (DayFile _ f) p = f p

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
  case parse (p <* eof) path text of
    Right a -> f a
    Left e  -> putStrLn $ errorBundlePretty e
