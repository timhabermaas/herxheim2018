{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module RegistrationsTable
    ( tableAsHtml
    , tableAsDocx
    ) where

import Text.Pandoc
import qualified Text.Blaze.Html5 as H
import qualified Data.Text as T
import qualified Db as Db
import Types
import Data.Monoid ((<>))
import Data.Time.Format (formatTime, defaultTimeLocale)

import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Identity (runIdentity)

tableAsHtml :: [Db.DbParticipant] -> H.Html
tableAsHtml participants =
    runPandocMonadUnsafe $ writeHtml5 def $ table participants

tableAsDocx :: [Db.DbParticipant] -> T.Text
tableAsDocx participants = runPandocMonadUnsafe $ writeOpenDocument def $ table participants

runPandocMonadUnsafe :: PandocPure a -> a
runPandocMonadUnsafe =
    either (error . show) id
  . runIdentity
  . flip evalStateT def
  . flip evalStateT def
  . runExceptT
  . unPandocPure

table :: [Db.DbParticipant] -> Pandoc
table participants = Pandoc nullMeta [table']
  where
    table' =
        Table
          []
          [AlignLeft, AlignLeft, AlignLeft, AlignLeft]
          [0, 0, 0, 0]
          [[plain "Name"], [plain "Adresse"], [plain "Geburtsdatum"], [plain "Übernachtung"]]
          (fmap row participants)
    row p@Db.DbParticipant{..} =
        [ [plain' dbParticipantName]
        , [plain' $ address p]
        , [plain' $ T.pack $ birthday dbParticipantBirthday]
        , [plain' $ sleeping dbParticipantSleepovers]
        ]
    address Db.DbParticipant{..} = dbParticipantStreet <> ", " <> dbParticipantPostalCode <> " " <> dbParticipantCity
    birthday d = formatTime defaultTimeLocale "%d.%m.%Y" d
    sleeping s = case s of
        FridayNight -> "Nur Freitag"
        SaturdayNight -> "Nur Samstag"
        AllNights -> "Samstag und Sonntag"
        NoNights -> "Keine Übernachtung"
    plain s = Plain [Str s]
    plain' s = Plain [Str $ T.unpack s]
