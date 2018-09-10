{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Db
    ( migrate
    , connect
    , saveRegistration
    , deleteRegistration
    , allRegistrations
    , DbParticipant(..)
    , DbId(..)
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
    { dbParticipantId :: DbId DbParticipant
    , dbParticipantName :: T.Text
    , dbParticipantBirthday :: Day
    , dbParticipantStreet :: T.Text
    , dbParticipantPostalCode :: T.Text
    , dbParticipantCity :: T.Text
    , dbParticipantCountry :: T.Text
    , dbParticipantRegisteredAt :: UTCTime
    , dbParticipantSleepovers :: Sleepover
    , dbParticipantComment :: Maybe T.Text
    , dbParticipantEmail :: Maybe T.Text
    } deriving (Show)

newtype DbId a = DbId Int deriving (Show)

instance FromField (DbId a) where
    fromField f bs = DbId <$> fromField f bs

instance FromField Sleepover where
    fromField f bs = do
        value <- fromField f bs
        case value :: String of
            -- both, fr and sa are for backwards compability only.
            -- We used to save the specific day on which the participant slept
            -- at the convention.
            "both" -> return GymSleeping
            "fr" -> return GymSleeping
            "sa" -> return GymSleeping
            "none" -> return NoNights
            "n/a" -> return CouldntSelect
            "camping" -> return Camping
            "gym" -> return GymSleeping
            _ -> fail "sleepover not of expected value"

sleepoversToText :: Sleepover -> T.Text
sleepoversToText NoNights = "none"
sleepoversToText CouldntSelect = "n/a"
sleepoversToText Camping = "camping"
sleepoversToText GymSleeping = "gym"

instance FromRow DbParticipant where
    fromRow = DbParticipant <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

newtype Connection = Connection PSQL.Connection

saveRegistration :: Connection -> Participant -> IO ()
saveRegistration (Connection conn) Participant{..} = do
    t <- getCurrentTime
    void $ PSQL.execute conn "INSERT INTO participants (name, birthday, street, postalCode, city, country, registeredAt, sleepovers, comment, email) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" (participantName, participantBirthday, participantStreet, participantPostalCode, participantCity, participantCountry, t, sleepoversToText participantSleepovers, participantComment, participantEmail)

deleteRegistration :: Connection -> DbId Participant -> IO ()
deleteRegistration (Connection conn) (DbId id') = do
    void $ PSQL.execute conn "DELETE FROM participants WHERE id = ?" (PSQL.Only id')


allRegistrations :: Connection -> IO [DbParticipant]
allRegistrations (Connection conn) = do
    PSQL.query_ conn "SELECT id, name, birthday, street, postalCode, city, country, registeredAt, sleepovers, comment, email FROM participants ORDER BY registeredAt DESC"

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
        , "country text NOT NULL,"
        , "registeredAt timestamptz NOT NULL);"
        , "ALTER TABLE participants ADD COLUMN IF NOT EXISTS comment text;"
        , "ALTER TABLE participants ADD COLUMN IF NOT EXISTS email text;"
        ]
