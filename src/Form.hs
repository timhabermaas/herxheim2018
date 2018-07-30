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
    Participant <$> "name" DF..: mustBePresent (DF.text Nothing)
                <*> "birthday" DF..: DF.dateFormlet "%-d.%-m.%Y" Nothing
                <*> "street" DF..: mustBePresent (DF.text Nothing)
                <*> "postalCode" DF..: mustBePresent (DF.text Nothing)
                <*> "city" DF..: mustBePresent (DF.text Nothing)
                <*> "sleepover" DF..: DF.choice [(FridayNight, "Freitag"), (SaturdayNight, "Samstag"), (AllNights, "Freitag und Samstag"), (NoNights, "Keine Übernachtung")] (Just AllNights)

mustBePresent :: (Monad m) => DF.Form T.Text m T.Text -> DF.Form T.Text m T.Text
mustBePresent = DF.check "can't be blank" notEmpty
  where
    notEmpty = not . T.null
