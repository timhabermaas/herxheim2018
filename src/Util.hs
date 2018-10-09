module Util
    ( gymSleepCount
    , campingSleepCount -- TODO: Rename
    , formatDay
    ) where

import Types
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Calendar (Day)

gymSleepCount :: [Sleepover] -> Int
gymSleepCount = length . filter (\s -> s == GymSleeping)

campingSleepCount :: [Sleepover] -> Int
campingSleepCount = length . filter (== Camping)

formatDay :: Day -> String
formatDay d = formatTime defaultTimeLocale "%d.%m.%Y" d
