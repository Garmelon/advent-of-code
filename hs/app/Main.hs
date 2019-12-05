module Main where

import Options.Applicative

import Aoc.Y2019.A01

data Settings = Settings
  { function :: FilePath -> IO ()
  , filename :: FilePath
  }

solutions :: Parser (FilePath -> IO ())
solutions = subparser $ mconcat $ map (\(name, func) -> command name (info (pure func) mempty))
  [ ("201901", solve201901)
  ]

parser :: Parser Settings
parser = Settings
  <$> solutions
  <*> strArgument (metavar "INPUTFILE")

opts :: ParserInfo Settings
opts = info (helper <*> parser) $ fullDesc <> failureCode 1

main :: IO ()
main = do
  settings <- execParser opts
  function settings $ filename settings
