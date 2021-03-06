{-# LANGUAGE OverloadedStrings #-}

module Form
    ( registerForm
    , Participant(..)
    , BotStatus(..)
    ) where

import Types
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DT
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorianValid)

data BotStatus = IsBot | IsHuman

registerForm :: (Monad m) => (GymSleepingLimitReached, CampingSleepingLimitReached) -> DF.Form T.Text m (BotStatus, Participant)
registerForm isOverLimit =
    (,) <$> botField
        <*> participant
  where
    -- Preventing bot form submissions by checking for a form field not being filled out.
    botField = (\t -> if T.null t then IsHuman else IsBot) <$> "botField" DF..: DF.text Nothing
    participant = Participant <$> "name" DF..: mustBePresent (DF.text Nothing)
                              <*> "birthday" DF..: birthdayFields
                              <*> "street" DF..: mustBePresent (DF.text Nothing)
                              <*> "postalCode" DF..: mustBePresent (DF.text Nothing)
                              <*> "city" DF..: mustBePresent (DF.text Nothing)
                              <*> optionalSleepover
                              <*> "country" DF..: DF.choice countries (Just "Deutschland")
                              <*> "comment" DF..: optionalText
                              <*> "email" DF..: optionalText

    optionalText =
        (\t -> if T.null t then Nothing else Just t) <$> DF.text Nothing
    optionalSleepover =
        case isOverLimit of
            (GymSleepingLimitReached, CampingSleepingLimitReached) -> pure CouldntSelect
            (EnoughGymSleepingSpots, CampingSleepingLimitReached) -> "sleepover" DF..: DF.choice (filter (\(s, _) -> s /= Camping) allChoices) (Just GymSleeping)
            (GymSleepingLimitReached, EnoughTentSpots) -> "sleepover" DF..: DF.choice (filter (\(s, _) -> s /= GymSleeping) allChoices) (Just Camping)
            (EnoughGymSleepingSpots, EnoughTentSpots) -> "sleepover" DF..: DF.choice allChoices (Just GymSleeping)

    allChoices =
        [ (GymSleeping, "Ich schlafe im Klassenzimmer.")
        , (Camping, "Ich schlafe im Zelt auf dem Schulgelände.")
        , (NoNights, "Ich sorge für meine eigene Übernachtung.")
        ]

birthdayFields :: Monad m => DF.Form T.Text m Day
birthdayFields =
    DF.validate (maybe (DT.Error "kein gültiges Datum") DT.Success)
  $ fromGregorianValid <$> "year" DF..: DF.choice years (Just 1990)
                       <*> "month" DF..: DF.choice months (Just 1)
                       <*> "day" DF..: DF.choice days (Just 1)
  where
    years :: [(Integer, T.Text)]
    years = fmap (\y -> (y, T.pack $ show y)) [1850..2018]

    months :: [(Int, T.Text)]
    months =
        [ (1, "Januar")
        , (2, "Februar")
        , (3, "März")
        , (4, "April")
        , (5, "Mai")
        , (6, "Juni")
        , (7, "Juli")
        , (8, "August")
        , (9, "September")
        , (10, "Oktober")
        , (11, "November")
        , (12, "Dezember")
        ]

    days :: [(Int, T.Text)]
    days = fmap (\y -> (y, T.pack $ show y)) [1..31]


mustBePresent :: (Monad m) => DF.Form T.Text m T.Text -> DF.Form T.Text m T.Text
mustBePresent = DF.check "muss ausgefüllt werden" notEmpty
  where
    notEmpty = not . T.null

-- Source: https://github.com/umpirsky/country-list/blob/ad8f48405cc470d49dd535e2f2392e6572539d24/data/de_DE/country.txt
countries :: [(T.Text, T.Text)]
countries = (\t -> (t, t)) <$>
    [ "Afghanistan"
    , "Ägypten"
    , "Ålandinseln"
    , "Albanien"
    , "Algerien"
    , "Amerikanisch-Samoa"
    , "Amerikanische Jungferninseln"
    , "Amerikanische Überseeinseln"
    , "Andorra"
    , "Angola"
    , "Anguilla"
    , "Antarktis"
    , "Antigua und Barbuda"
    , "Äquatorialguinea"
    , "Argentinien"
    , "Armenien"
    , "Aruba"
    , "Ascension"
    , "Aserbaidschan"
    , "Äthiopien"
    , "Australien"
    , "Bahamas"
    , "Bahrain"
    , "Bangladesch"
    , "Barbados"
    , "Belarus"
    , "Belgien"
    , "Belize"
    , "Benin"
    , "Bermuda"
    , "Bhutan"
    , "Bolivien"
    , "Bonaire, Sint Eustatius und Saba"
    , "Bosnien und Herzegowina"
    , "Botsuana"
    , "Brasilien"
    , "Britische Jungferninseln"
    , "Britisches Territorium im Indischen Ozean"
    , "Brunei Darussalam"
    , "Bulgarien"
    , "Burkina Faso"
    , "Burundi"
    , "Cabo Verde"
    , "Ceuta und Melilla"
    , "Chile"
    , "China"
    , "Cookinseln"
    , "Costa Rica"
    , "Côte d’Ivoire"
    , "Curaçao"
    , "Dänemark"
    , "Deutschland"
    , "Diego Garcia"
    , "Dominica"
    , "Dominikanische Republik"
    , "Dschibuti"
    , "Ecuador"
    , "El Salvador"
    , "Eritrea"
    , "Estland"
    , "Eurozone"
    , "Falklandinseln"
    , "Färöer"
    , "Fidschi"
    , "Finnland"
    , "Frankreich"
    , "Französisch-Guayana"
    , "Französisch-Polynesien"
    , "Französische Süd- und Antarktisgebiete"
    , "Gabun"
    , "Gambia"
    , "Georgien"
    , "Ghana"
    , "Gibraltar"
    , "Grenada"
    , "Griechenland"
    , "Grönland"
    , "Guadeloupe"
    , "Guam"
    , "Guatemala"
    , "Guernsey"
    , "Guinea"
    , "Guinea-Bissau"
    , "Guyana"
    , "Haiti"
    , "Honduras"
    , "Indien"
    , "Indonesien"
    , "Irak"
    , "Iran"
    , "Irland"
    , "Island"
    , "Isle of Man"
    , "Israel"
    , "Italien"
    , "Jamaika"
    , "Japan"
    , "Jemen"
    , "Jersey"
    , "Jordanien"
    , "Kaimaninseln"
    , "Kambodscha"
    , "Kamerun"
    , "Kanada"
    , "Kanarische Inseln"
    , "Kasachstan"
    , "Katar"
    , "Kenia"
    , "Kirgisistan"
    , "Kiribati"
    , "Kokosinseln"
    , "Kolumbien"
    , "Komoren"
    , "Kongo-Brazzaville"
    , "Kongo-Kinshasa"
    , "Kosovo"
    , "Kroatien"
    , "Kuba"
    , "Kuwait"
    , "Laos"
    , "Lesotho"
    , "Lettland"
    , "Libanon"
    , "Liberia"
    , "Libyen"
    , "Liechtenstein"
    , "Litauen"
    , "Luxemburg"
    , "Madagaskar"
    , "Malawi"
    , "Malaysia"
    , "Malediven"
    , "Mali"
    , "Malta"
    , "Marokko"
    , "Marshallinseln"
    , "Martinique"
    , "Mauretanien"
    , "Mauritius"
    , "Mayotte"
    , "Mazedonien"
    , "Mexiko"
    , "Mikronesien"
    , "Monaco"
    , "Mongolei"
    , "Montenegro"
    , "Montserrat"
    , "Mosambik"
    , "Myanmar"
    , "Namibia"
    , "Nauru"
    , "Nepal"
    , "Neukaledonien"
    , "Neuseeland"
    , "Nicaragua"
    , "Niederlande"
    , "Niger"
    , "Nigeria"
    , "Niue"
    , "Nordkorea"
    , "Nördliche Marianen"
    , "Norfolkinsel"
    , "Norwegen"
    , "Oman"
    , "Österreich"
    , "Osttimor"
    , "Pakistan"
    , "Palästinensische Autonomiegebiete"
    , "Palau"
    , "Panama"
    , "Papua-Neuguinea"
    , "Paraguay"
    , "Peru"
    , "Philippinen"
    , "Pitcairninseln"
    , "Polen"
    , "Portugal"
    , "Puerto Rico"
    , "Republik Moldau"
    , "Réunion"
    , "Ruanda"
    , "Rumänien"
    , "Russland"
    , "Salomonen"
    , "Sambia"
    , "Samoa"
    , "San Marino"
    , "São Tomé und Príncipe"
    , "Saudi-Arabien"
    , "Schweden"
    , "Schweiz"
    , "Senegal"
    , "Serbien"
    , "Seychellen"
    , "Sierra Leone"
    , "Simbabwe"
    , "Singapur"
    , "Sint Maarten"
    , "Slowakei"
    , "Slowenien"
    , "Somalia"
    , "Sonderverwaltungsregion Macau"
    , "Sonderverwaltungszone Hongkong"
    , "Spanien"
    , "Spitzbergen"
    , "Sri Lanka"
    , "St. Barthélemy"
    , "St. Helena"
    , "St. Kitts und Nevis"
    , "St. Lucia"
    , "St. Martin"
    , "St. Pierre und Miquelon"
    , "St. Vincent und die Grenadinen"
    , "Südafrika"
    , "Sudan"
    , "Südgeorgien und die Südlichen Sandwichinseln"
    , "Südkorea"
    , "Südsudan"
    , "Suriname"
    , "Swasiland"
    , "Syrien"
    , "Tadschikistan"
    , "Taiwan"
    , "Tansania"
    , "Thailand"
    , "Togo"
    , "Tokelau"
    , "Tonga"
    , "Trinidad und Tobago"
    , "Tristan da Cunha"
    , "Tschad"
    , "Tschechien"
    , "Tunesien"
    , "Türkei"
    , "Turkmenistan"
    , "Turks- und Caicosinseln"
    , "Tuvalu"
    , "Uganda"
    , "Ukraine"
    , "Ungarn"
    , "Uruguay"
    , "Usbekistan"
    , "Vanuatu"
    , "Vatikanstadt"
    , "Venezuela"
    , "Vereinigte Arabische Emirate"
    , "Vereinigte Staaten"
    , "Vereinigtes Königreich"
    , "Vereinte Nationen"
    , "Vietnam"
    , "Wallis und Futuna"
    , "Weihnachtsinsel"
    , "Westsahara"
    , "Zentralafrikanische Republik"
    , "Zypern"
    ]
