{-# LANGUAGE OverloadedStrings #-}

module Form
    ( registerForm
    , Participant(..)
    ) where

import Types
import qualified Text.Digestive.Form as DF
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Monoid ((<>))

registerForm :: (Monad m) => DF.Form T.Text m Participant
registerForm =
    Participant <$> "name" DF..: DF.text Nothing
                <*> "birthday" DF..: DF.dateFormlet "%-d.%-m.%Y" Nothing
                <*> "street" DF..: DF.text Nothing
                <*> "postalCode" DF..: DF.text Nothing
                <*> "city" DF..: DF.text Nothing
                <*> ((<>) <$> sleepover "fridayNight" FridayNight <*> sleepover "saturdayNight" SaturdayNight)
  where
    sleepover name v = (\b -> if b then Set.singleton v else Set.empty) <$> name DF..: DF.bool Nothing
