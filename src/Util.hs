module Util
    ( gymSleepCount
    , campingSleepCount -- TODO: Rename
    ) where

import Types

gymSleepCount :: [Sleepover] -> Int
gymSleepCount = length . filter (\s -> s == GymSleeping)

campingSleepCount :: [Sleepover] -> Int
campingSleepCount = length . filter (== Camping)
