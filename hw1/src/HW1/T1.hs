module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay day = case day of
                   Monday    -> Tuesday
                   Tuesday   -> Wednesday
                   Wednesday -> Thursday
                   Thursday  -> Friday
                   Friday    -> Saturday
                   Saturday  -> Sunday
                   Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays n day = afterDays (n - 1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend day = case day of
                 Saturday -> True
                 Sunday   -> True
                 _        -> False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day = daysToParty (nextDay day) + 1
