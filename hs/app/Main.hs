module Main where

import           Control.Monad

import           Options.Applicative

import           Aoc.Day             (Day (..))
import qualified Aoc.Y2019           as Y2019
import qualified Aoc.Y2020           as Y2020

toCommand :: Day -> Mod CommandFields (IO ())
toCommand (DayPure name f) = command name $ info (helper <*> pure f) mempty
toCommand (DayFile name f) = command name $ info (helper <*> p) mempty
  where
    p = f <$> strArgument (metavar "INPUTFILE")

parser :: Parser (IO ())
parser = subparser $ mconcat $ map toCommand $ Y2019.days ++ Y2020.days

opts :: ParserInfo (IO ())
opts = info (helper <*> parser) $ fullDesc <> failureCode 1

main :: IO ()
main = join $ customExecParser (prefs showHelpOnEmpty) opts
