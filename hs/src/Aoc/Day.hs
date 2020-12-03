module Aoc.Day
  ( Day(..)
  , dayPure
  , dayFile
  , dayString
  , dayText
  , Parser
  , dayParser
  ) where

import           Control.Monad
import           Data.Void

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Text.Megaparsec

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

type Parser = Parsec Void T.Text

dayParser :: String -> Parser a -> (a -> IO ()) -> Day
dayParser name p f = dayFile name $ \path -> do
  text <- T.readFile path
  case parse p path text of
    Right a -> f a
    Left e  -> putStrLn $ errorBundlePretty e
