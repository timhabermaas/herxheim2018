module Main where

import Lib
import System.Environment (getEnv)

main :: IO ()
main = do
    dbUrl <- getEnv "DATABASE_URL"
    startApp dbUrl
