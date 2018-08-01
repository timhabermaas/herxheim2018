{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( Participant(..)
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
    } deriving (Show)
