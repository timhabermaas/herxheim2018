{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Db
    ( migrate
    , connect
    , saveRegistration
    , allRegistrations
    , DbParticipant(..)
    , Connection
    ) where

import Control.Monad (void)
import Data.String (IsString(fromString))
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Calendar (Day)

import Types

-- TODO: Do not expose this datatype, but parameterize the id + registeredAt field of the on in Types
-- e.g. Participant () () would come from the form
data DbParticipant = DbParticipant
    { dbParticipantId :: Int
    , dbParticipantName :: T.Text
    , dbParticipantBirthday :: Day
    , dbParticipantStreet :: T.Text
    , dbParticipantPostalCode :: T.Text
    , dbParticipantCity :: T.Text
    , dbParticipantRegisteredAt :: UTCTime
    , dbParticipantSleepovers :: Sleepover
    } deriving (Show)

instance FromField Sleepover where
    fromField f bs = do
        value <- fromField f bs
        case value :: String of
            "both" -> return AllNights
            "fr" -> return FridayNight
            "sa" -> return SaturdayNight
            "none" -> return NoNights
            _ -> fail "sleepover not of expected value"

sleepoversToText :: Sleepover -> T.Text
sleepoversToText FridayNight = "fr"
sleepoversToText SaturdayNight = "sa"
sleepoversToText AllNights = "both"
sleepoversToText NoNights = "none"

instance FromRow DbParticipant where
    fromRow = DbParticipant <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

newtype Connection = Connection PSQL.Connection

saveRegistration :: Connection -> Participant -> IO ()
saveRegistration (Connection conn) Participant{..} = do
    t <- getCurrentTime
    void $ PSQL.execute conn "INSERT INTO participants (name, birthday, street, postalCode, city, registeredAt, sleepovers) VALUES (?, ?, ?, ?, ?, ?, ?)" (participantName, participantBirthday, participantStreet, participantPostalCode, participantCity, t, sleepoversToText participantSleepovers)

allRegistrations :: Connection -> IO [DbParticipant]
allRegistrations (Connection conn) = do
    PSQL.query_ conn "SELECT id, name, birthday, street, postalCode, city, registeredAt, sleepovers FROM participants"

connect :: String -> IO Connection
connect url = Connection <$> PSQL.connectPostgreSQL (BS.pack url)

migrate :: Connection -> IO ()
migrate (Connection conn) =
    void $ PSQL.execute_ conn statement
  where
    statement = fromString $ unlines
        [ "CREATE TABLE IF NOT EXISTS participants ("
        , "id SERIAL PRIMARY KEY,"
        , "name text NOT NULL,"
        , "birthday date NOT NULL,"
        , "street text NOT NULL,"
        , "postalCode text NOT NULL,"
        , "city text NOT NULL,"
        , "sleepovers text NOT NULL,"
        , "registeredAt timestamptz NOT NULL);"
        ]
