{-# LANGUAGE OverloadedStrings #-}

module Form
    ( registerForm
    , Participant(..)
    ) where

import Types
import qualified Text.Digestive.Form as DF
import qualified Data.Text as T

registerForm :: (Monad m) => DF.Form T.Text m Participant
registerForm =
    Participant <$> "name" DF..: DF.text Nothing
                <*> "birthday" DF..: DF.dateFormlet "%-d.%-m.%Y" Nothing
                <*> "street" DF..: DF.text Nothing
                <*> "postalCode" DF..: DF.text Nothing
                <*> "city" DF..: DF.text Nothing
                <*> "sleepover" DF..: DF.choice [(FridayNight, "Freitag"), (SaturdayNight, "Samstag"), (AllNights, "Freitag und Samstag"), (NoNights, "Keine Übernachtung")] (Just AllNights)
