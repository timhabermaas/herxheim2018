{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app, AdminPassword(..))
import Test.Hspec
import Test.Hspec.Wai
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy as T
import Data.Semigroup ((<>))

import qualified Db as Db
import System.Environment (getEnv)

main :: IO ()
main = do
    dbUrl <- getEnv "DATABASE_URL"
    conn <- Db.connect dbUrl
    Db.migrate conn
    hspec $ spec conn (AdminPassword "admin")

spec :: Db.Connection -> AdminPassword -> Spec
spec conn pw = with (return $ app conn pw) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
        it "has some FAQ entries" $ do
            get "/" `shouldRespondWith` (successAndContains "Warum muss ich mich dieses Jahr überhaupt anmelden?")

successAndContains :: T.Text -> ResponseMatcher
successAndContains text =
    200 { matchBody = MatchBody matcher }
  where
    matcher _header body =
        if T.isInfixOf text (TE.decodeUtf8 body)
            then Nothing
            else Just $ show text <> " not found"
