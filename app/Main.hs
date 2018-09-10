module Main where

import Lib
import System.Environment (getEnv)
import Data.Text (pack)

main :: IO ()
main = do
    dbUrl <- getEnv "DATABASE_URL"
    port <- read <$> getEnv "PORT"
    sleepingLimit <- read <$> getEnv "SLEEPING_LIMIT"
    campingLimit <- read <$> getEnv "CAMPING_LIMIT"
    pw <- AdminPassword . pack <$> getEnv "ADMIN_PASSWORD"
    startApp dbUrl port sleepingLimit campingLimit pw
