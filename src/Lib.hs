{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}


module Lib
    ( startApp
    , app
    , AdminPassword(..)
    , Config(..)
    ) where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze (HTML)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Media ((//), (/:))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Digestive.Form.Encoding as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.View as DF
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import qualified Db as Db
import qualified Html as Page
import qualified Form as Form
import qualified Data.Maybe as M
import Types
import Util

data CSV

instance Accept CSV where
    contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance MimeRender CSV BSL.ByteString where
    mimeRender _ val = val

type API
    = Get '[HTML] Page.Html
 :<|> "register" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Page.Html
 :<|> "success" :> Get '[HTML] Page.Html
 :<|> "admin" :> BasicAuth "foo-realm" () :> Get '[HTML] Page.Html
 :<|> "registrations.csv" :> BasicAuth "foo-realm" () :> Get '[CSV] BSL.ByteString
 :<|> "registrations" :> BasicAuth "foo-realm" () :> Capture "participantId" ParticipantId :> "delete" :> Post '[HTML] Page.Html
 :<|> "registrations" :> BasicAuth "foo-realm" () :> "print" :> Get '[HTML] Page.Html

newtype AdminPassword = AdminPassword T.Text

data Config = Config
    { configDbConnection :: Db.Connection
    , configAdminPassword :: AdminPassword
    , configSleepingLimits :: (GymSleepingLimit, CampingSleepingLimit)
    }

startApp :: String -> Int -> Int -> Int -> AdminPassword -> IO ()
startApp dbUrl port participationLimit campingLimit pw = do
    conn <- Db.connect dbUrl
    Db.migrate conn
    let config = Config { configDbConnection = conn, configAdminPassword = pw, configSleepingLimits = (GymSleepingLimit participationLimit, CampingSleepingLimit campingLimit) }
    run port $ logStdoutDev $ app config

authCheck :: AdminPassword -> BasicAuthCheck ()
authCheck (AdminPassword pw) =
    let check (BasicAuthData username password) =
            if username == "admin" && password == TE.encodeUtf8 pw
            then pure (Authorized ())
            else pure Unauthorized
    in
        BasicAuthCheck check

authServerContext :: AdminPassword -> Context (BasicAuthCheck () ': '[])
authServerContext pw = (authCheck pw) :. EmptyContext

app :: Config -> Application
app Config{..} =
    serveWithContext api (authServerContext configAdminPassword) (server configDbConnection configSleepingLimits)

api :: Proxy API
api = Proxy

server :: Db.Connection -> (GymSleepingLimit, CampingSleepingLimit) -> Server API
server conn limits =
         registerHandler conn limits
    :<|> postRegisterHandler conn limits
    :<|> successHandler
    :<|> registrationsHandler conn limits
    :<|> registrationsCsvHandler conn
    :<|> deleteRegistrationsHandler conn
    :<|> printRegistrationsHandler conn

isOverLimit :: Db.Connection -> (GymSleepingLimit, CampingSleepingLimit) -> IO (GymSleepingLimitReached, CampingSleepingLimitReached)
isOverLimit conn (GymSleepingLimit gymLimit, CampingSleepingLimit campingLimit) = do
    sleepovers <- liftIO $ fmap Db.dbParticipantSleepovers <$> Db.allRegistrations conn
    let gymLimitReached =
            if gymSleepCount sleepovers >= gymLimit then
                GymSleepingLimitReached
            else
                EnoughGymSleepingSpots
    let campingLimitReached =
            if campingSleepCount sleepovers >= campingLimit then
                CampingSleepingLimitReached
            else
                EnoughTentSpots
    pure (gymLimitReached, campingLimitReached)

registerHandler :: Db.Connection -> (GymSleepingLimit, CampingSleepingLimit) -> Handler Page.Html
registerHandler conn limits = do
    overLimit <- liftIO $ isOverLimit conn limits
    view <- DF.getForm "Registration" $ Form.registerForm overLimit
    pure $ Page.registerPage view overLimit

registrationsHandler :: Db.Connection -> (GymSleepingLimit, CampingSleepingLimit) -> () -> Handler Page.Html
registrationsHandler conn limits _ = do
    registrations <- liftIO $ Db.allRegistrations conn
    pure $ Page.registrationListPage registrations limits


-- Using newtype wrapper for Participant because the canonical CSV decoder/encoder for the
-- database row isn't exactly what we want.
newtype CsvParticipant = CsvParticipant Db.DbParticipant

-- The IsString instance of ByteString is not using the source encoding (in this case UTF8),
-- but Char8.pack to convert String to ByteString:
-- `f :: ByteString`, `f "ü"` will result in \252.
-- See https://github.com/haskell/bytestring/issues/140
fixEncoding :: T.Text -> BS.ByteString
fixEncoding = TE.encodeUtf8

instance Csv.ToNamedRecord CsvParticipant where
    toNamedRecord (CsvParticipant Db.DbParticipant{..}) =
        Csv.namedRecord
            [ "Name" Csv..= dbParticipantName
            , "Adresse" Csv..= TE.encodeUtf8 address
            , "Land" Csv..= TE.encodeUtf8 dbParticipantCountry
            , fixEncoding "Übernachtung" Csv..= sleeping dbParticipantSleepovers
            , "Anmerkung" Csv..= (TE.encodeUtf8 <$> dbParticipantComment)
            ]
      where
        address = (dbParticipantStreet <> ", " <> dbParticipantPostalCode <> " " <> dbParticipantCity) :: T.Text
        sleeping s = case s of
            NoNights -> "Keine Übernachtung" :: T.Text
            Camping -> "Zelt"
            GymSleeping -> "Klassenzimmer"
            CouldntSelect -> "Keine Auswahl"

registrationsCsvHandler :: Db.Connection -> () -> Handler BSL.ByteString
registrationsCsvHandler conn _ = do
    registrations <- liftIO $ Db.allRegistrations conn
    let headers = fixEncoding <$> V.fromList [ "Name", "Adresse", "Land", "Übernachtung", "Anmerkung" ]
    pure $ Csv.encodeByName headers $ fmap CsvParticipant registrations

postRegisterHandler :: Db.Connection -> (GymSleepingLimit, CampingSleepingLimit) -> [(T.Text, T.Text)] -> Handler Page.Html
postRegisterHandler conn limits body = do
    overLimit <- liftIO $ isOverLimit conn limits
    r <- DF.postForm "Registration" (Form.registerForm overLimit) $ servantPathEnv body
    case r of
        (view, Nothing) -> do
            liftIO $ print view
            pure $ Page.registerPage view overLimit
        (_, Just (botStatus, registration)) ->
            case botStatus of
                Form.IsBot -> redirectTo "/success"
                Form.IsHuman -> do
                    liftIO $ Db.saveRegistration conn registration
                    redirectTo "/success"

deleteRegistrationsHandler :: Db.Connection -> () -> ParticipantId -> Handler Page.Html
deleteRegistrationsHandler conn _ (ParticipantId participantId) = do
    liftIO $ Db.deleteRegistration conn (Db.DbId participantId)
    redirectTo "/admin"

printRegistrationsHandler :: Db.Connection -> () -> Handler Page.Html
printRegistrationsHandler conn _ = do
    regs <- liftIO $ Db.allRegistrations conn
    pure $ Page.registrationPrintPage regs


successHandler :: Handler Page.Html
successHandler = do
    pure Page.successPage

redirectTo :: BS.ByteString -> Handler a
redirectTo url =
    throwError $ err303 { errHeaders = [("Location", url)] }

servantPathEnv :: (Monad m) => [(T.Text, T.Text)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv body _ = pure env
  where
    lookupParam p = lookup (DF.fromPath p) body
    env path = return (DF.TextInput <$> (M.maybeToList (lookupParam path)))
