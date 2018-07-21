module Types
    ( Participant(..)
    , Sleepover(..)
    ) where

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Time.Calendar (Day)

data Sleepover = FridayNight | SaturdayNight deriving (Show, Ord, Eq)

data Participant = Participant
    { participantName :: T.Text
    , participantBirthday :: Day
    , participantStreet :: T.Text
    , participantPostalCode :: T.Text
    , participantCity :: T.Text
    , participantSleepovers :: Set.Set Sleepover
    } deriving (Show)
