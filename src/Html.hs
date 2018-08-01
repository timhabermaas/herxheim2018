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
import Data.Monoid ((<>))
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Db as Db
import Types

type Html = H.Html

layout :: H.Html -> H.Html
layout inner = do
    H.docType
    H.html ! A.lang "de" $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
            H.title "Herxheim Convention 2018"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://cdn.jsdelivr.net/npm/flatpickr/dist/flatpickr.min.css"
            H.script ! A.src "https://cdn.jsdelivr.net/npm/flatpickr" $ ""
        H.body $ do
            H.div ! A.class_ "container" $ do
                inner
            H.script $ "flatpickr(document.getElementById('Registration.birthday'), { dateFormat: 'd.m.Y'})"

registrationListPage :: [Db.DbParticipant] -> H.Html
registrationListPage participants = layout $ do
    row $ do
        col 12 $ do
            H.table ! A.class_ "table" $ do
                H.thead $ do
                    H.tr $ do
                        H.th "Name"
                        H.th "Geburtsdatum"
                        H.th "Adresse"
                        H.th "Land"
                        H.th "Übernachtung" ! A.colspan "2"
                        H.th "Aktionen"
                    H.tr $ do
                        H.th ""
                        H.th ""
                        H.th ""
                        H.th ""
                        H.th ! A.class_ "text-center" $ "Fr -> Sa"
                        H.th ! A.class_ "text-center" $ "Sa -> So"
                        H.th ""
                H.tbody $ mapM_ participantRow participants
                H.tfoot $ do
                    H.tr $ do
                        H.th $ H.toHtml $ length participants
                        H.th ""
                        H.th ""
                        H.th ""
                        H.th ! A.class_ "text-right" $ H.toHtml $ length $ filter (\p -> Db.dbParticipantSleepovers p == AllNights || Db.dbParticipantSleepovers p == FridayNight) participants
                        H.th ! A.class_ "text-right" $ H.toHtml $ length $ filter (\p -> Db.dbParticipantSleepovers p == AllNights || Db.dbParticipantSleepovers p == SaturdayNight) participants
                        H.th ""
    row $ do
        col 12 $ do
            H.a ! A.href "/registrations.csv" $ "Download als .csv"
  where
    participantRow p@Db.DbParticipant{..} =
        H.tr $ do
            H.td $ H.toHtml dbParticipantName
            H.td $ H.toHtml $ birthday dbParticipantBirthday
            H.td $ H.toHtml $ address p
            H.td $ H.toHtml $ dbParticipantCountry
            H.td ! A.class_ "text-center" $ friday dbParticipantSleepovers
            H.td ! A.class_ "text-center" $ saturday dbParticipantSleepovers
            H.td $ do
                H.form ! A.action (H.toValue $ "/registrations/" <> idToText dbParticipantId <> "/delete")  ! A.method "post" $ do
                    H.input ! A.onclick (H.toValue $ "return confirm('Willst du wirklich ' + '" <> dbParticipantName <> "' + ' ausladen?');") ! A.class_ "btn btn-danger" ! A.type_ "submit" ! A.name "delete" ! A.value "Löschen"
    idToText (Db.DbId i) = show i
    birthday d = formatTime defaultTimeLocale "%d.%m.%Y" d
    address Db.DbParticipant{..} = (dbParticipantStreet <> ", " <> dbParticipantPostalCode <> " " <> dbParticipantCity) :: T.Text
    friday FridayNight = "X"
    friday AllNights = "X"
    friday _ = ""
    saturday SaturdayNight = "X"
    saturday AllNights = "X"
    saturday _ = ""

successPage :: H.Html
successPage = layout $ do
    "Danke fuer deine Anmeldung!"

modifiedView :: DV.View T.Text -> DV.View H.Html
modifiedView = fmap H.toHtml

registerPage :: DV.View T.Text -> H.Html
registerPage view = layout $ do
    row $ do
        col 12 $ do
            H.h1 ! A.class_ "mt-3" $ "Herxheim-Anmeldung 2018"
    row $ do
        col 6 $ do
            H.form ! A.action "/register" ! A.method "post" $ do
                H.div ! A.class_ "form-group" $ do
                    label "Name" "name" view
                    DH.inputText "name" view ! A.class_ "form-control"
                    DH.errorList "name" (modifiedView view)
                H.div ! A.class_ "form-group" $ do
                    label "Geburtsdatum" "birthday" view
                    DH.inputText "birthday" view ! A.class_ "form-control"
                    DH.errorList "birthday" (modifiedView view)
                H.div ! A.class_ "form-group" $ do
                    label "Straße" "street" view
                    DH.inputText "street" view ! A.class_ "form-control"
                    DH.errorList "street" (modifiedView view)
                H.div ! A.class_ "row" $ do
                    H.div ! A.class_ "col-4 form-group" $ do
                        label "PLZ" "postalCode" view
                        DH.inputText "postalCode" view ! A.class_ "form-control"
                        DH.errorList "postalCode" (modifiedView view)
                    H.div ! A.class_ "col-8 form-group" $ do
                        label "Stadt" "city" view
                        DH.inputText "city" view ! A.class_ "form-control"
                        DH.errorList "city" (modifiedView view)
                H.div ! A.class_ "form-group" $ do
                    label "Land" "country" view
                    DH.inputSelect "country" (modifiedView view) ! A.class_ "form-control"
                    DH.errorList "country" (modifiedView view)
                H.div ! A.class_ "form-group" $ do
                    H.h4 "Übernachtung"
                    bootstrapRadios "sleepover" (modifiedView view)
                H.div ! A.class_ "form-group" $ do
                    H.input ! A.class_ "btn btn-primary" ! A.type_ "submit" ! A.value "Anmelden"
        col 6 $ do
            H.h2 "FAQs"
            H.h3 "Warum muss ich mich dieses Jahr überhaupt anmelden?"
            H.p "Wir haben dieses Jahr nur eine begrenzte Anzahl an Schlafplätzen zur Verfügung und mit einer Anmeldung sicherst du dir einen Schlafplatz."
            H.h3 "Ich kann leider doch nicht kommen, was nun?"
            H.p $ do
                "Schade! Schreibe dann eine E-Mail an "
                mailLink registrationEmail registrationEmail
                " mit deinem Namen und bitte um Abmeldung."
            H.h3 "Was passiert mit meinen Daten?"
            H.p $ do
                "Die Daten dienen ausschließlich der Anmeldung bei der Herxheim-Convention "
                "und werden nur zu diesem Zweck gespeichert. Du kannst uns jederzeit unter "
                mailLink registrationEmail registrationEmail
                "kontaktieren um deine Daten und somit deine Anmeldung löschen zu lassen. "
                "Bis 30 Tage nach der Convention (11.11.2018) werden alle deine Daten gelöscht."

registrationEmail :: T.Text
registrationEmail = "herxheim.convention@gmail.com"

mailLink :: T.Text -> T.Text -> Html
mailLink text email = H.a ! A.href (H.toValue $ "mailto:" <> email) $ H.toHtml text

label :: T.Text -> T.Text -> DV.View a -> Html
label text name view =
    let ref = H.toValue $ DV.absoluteRef name view
    in
        H.label ! A.for ref $ H.toHtml text

bootstrapRadios :: T.Text -> DV.View Html -> Html
bootstrapRadios ref view =
    let options = DV.fieldInputChoice ref view
        ref' = DV.absoluteRef ref view
        radio (i, c, selected) = do
            let cssId = H.toValue $ ref' <> i
            H.div ! A.class_ "form-check" $ do
                H.input ! A.id cssId ! A.class_ "form-check-input" ! A.type_ "radio" ! A.name (H.toValue ref') ! A.value (H.toValue i) !? (selected, A.checked "selected")
                H.label ! A.class_ "form-check-label" ! A.for cssId $ c
    in
        mapM_ radio options

row :: Html -> Html
row inner = H.div ! A.class_ "row" $ inner

col :: Int -> Html -> Html
col columns inner =
    H.div ! A.class_ (H.toValue $ "col-md-" ++ show columns) $ inner
