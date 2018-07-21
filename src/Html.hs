{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Html
    ( registerPage
    , successPage
    , registrationListPage
    , Html
    ) where

import qualified Text.Blaze.Html5              as H
import Text.Blaze.Html5 ((!), (!?))
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Text.Digestive.Blaze.Html5    as DH
import qualified Text.Digestive.View           as DV
import qualified Data.Text                     as T
import qualified Data.Set                      as Set
import Data.Monoid ((<>))

import qualified Db as Db

type Html = H.Html

layout :: H.Html -> H.Html
layout inner =
    H.html $ do
        H.head $ do
            H.title "Herxheim Convention 2018"
        H.body $ do
            inner

registrationListPage :: [Db.DbParticipant] -> H.Html
registrationListPage participants =
    H.ul $ mapM_ registrationEntry participants
  where
    registrationEntry Db.DbParticipant{..} = H.li $ H.toHtml $ dbParticipantName <> "(" <> sleepoverNames dbParticipantSleepovers <> ")"
    sleepoverNames ss = T.intercalate ", " $ T.pack . show <$> Set.toList ss

successPage :: H.Html
successPage = layout $ do
    "Danke fuer deine Anmeldung!"

modifiedView :: DV.View T.Text -> DV.View H.Html
modifiedView = fmap H.toHtml

registerPage :: DV.View T.Text -> H.Html
registerPage view = layout $ do
    H.form ! A.action "/register" ! A.method "post" $ do
        H.div $ do
            H.label "Name"
            DH.errorList "name" (modifiedView view)
            DH.inputText "name" view
        H.div $ do
            H.label "Geburtsdatum"
            DH.errorList "birthday" (modifiedView view)
            DH.inputText "birthday" view
        H.div $ do
            H.label "Strasse"
            DH.inputText "street" view
        H.div $ do
            H.label "PLZ"
            DH.inputText "postalCode" view
        H.div $ do
            H.label "Stadt"
            DH.inputText "city" view
        H.div $ do
            H.label "Uebernachtungen"
            myInputCheckbox "fridayNight" view
            myInputCheckbox "saturdayNight" view
        H.div $ do
            H.input ! A.type_ "submit"

-- Own implementation of DH.inputCheckbox because the type is too restrictive:
-- DH's implementation expectes View HTML while View v works just as well.
myInputCheckbox :: T.Text -> DV.View v -> Html
myInputCheckbox ref view = H.input
    !  A.type_ "checkbox"
    !  A.id    (H.toValue ref')
    !  A.name  (H.toValue ref')
    !? (selected, A.checked "checked")
  where
    ref'     = DV.absoluteRef ref view
    selected = DV.fieldInputBool ref view
