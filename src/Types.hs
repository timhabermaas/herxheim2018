{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( Participant(..)
    , ParticipantLimit(..)
    , Sleepover(..)
    , ParticipantId(..)
    ) where

import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Web.HttpApiData (FromHttpApiData)

data Sleepover = FridayNight | SaturdayNight | AllNights | NoNights deriving (Show, Ord, Eq)

newtype ParticipantId = ParticipantId Int deriving (FromHttpApiData, Show)

data Participant = Participant
    { participantName :: T.Text
    , participantBirthday :: Day
    , participantStreet :: T.Text
    , participantPostalCode :: T.Text
    , participantCity :: T.Text
    , participantSleepovers :: Sleepover
    , participantCountry :: T.Text
    , participantComment :: Maybe T.Text
    , participantEmail :: Maybe T.Text
    } deriving (Show)

newtype ParticipantLimit = ParticipantLimit Int
