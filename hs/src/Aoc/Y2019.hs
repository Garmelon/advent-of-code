module Aoc.Y2019
  ( year
  ) where

import           Aoc.Day
import qualified Aoc.Y2019.D01 as D01
import qualified Aoc.Y2019.D02 as D02
import qualified Aoc.Y2019.D03 as D03

year :: Year
year = Year 2019
  [ ( 1, D01.day)
  , ( 2, D02.day)
  , ( 3, D03.day)
  ]
