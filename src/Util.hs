module Util
    ( fridaySleepCount
    , saturdaySleepCount
    , maxSleepCount
    ) where

import Types

fridaySleepCount :: [Sleepover] -> Int
fridaySleepCount = length . filter (\s -> s == AllNights || s == FridayNight)

saturdaySleepCount :: [Sleepover] -> Int
saturdaySleepCount = length . filter (\s -> s == AllNights || s == SaturdayNight)

maxSleepCount :: [Sleepover] -> Int
maxSleepCount ss = max (fridaySleepCount ss) (saturdaySleepCount ss)
