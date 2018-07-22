module Types
    ( Participant(..)
    , Sleepover(..)
    ) where

import qualified Data.Text as T
import Data.Time.Calendar (Day)

data Sleepover = FridayNight | SaturdayNight | AllNights | NoNights deriving (Show, Ord, Eq)

data Participant = Participant
    { participantName :: T.Text
    , participantBirthday :: Day
    , participantStreet :: T.Text
    , participantPostalCode :: T.Text
    , participantCity :: T.Text
    , participantSleepovers :: Sleepover
    } deriving (Show)
