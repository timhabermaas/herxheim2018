{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}


module Lib
    ( startApp
    , app
    , AdminPassword(..)
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

data CSV

instance Accept CSV where
    contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance MimeRender CSV BSL.ByteString where
    mimeRender _ val = val

type API
    = Get '[HTML] Page.Html
 :<|> "register" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Page.Html
 :<|> "success" :> Get '[HTML] Page.Html
 :<|> "registrations" :> BasicAuth "foo-realm" () :> Get '[HTML] Page.Html
 :<|> "registrations.csv" :> BasicAuth "foo-realm" () :> Get '[CSV] BSL.ByteString

newtype AdminPassword = AdminPassword T.Text

startApp :: String -> Int -> AdminPassword -> IO ()
startApp dbUrl port pw = do
    conn <- Db.connect dbUrl
    Db.migrate conn
    run port $ logStdoutDev $ app conn pw

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

app :: Db.Connection -> AdminPassword -> Application
app conn pw = serveWithContext api (authServerContext pw) (server conn)

api :: Proxy API
api = Proxy

server :: Db.Connection -> Server API
server conn =
         registerHandler
    :<|> postRegisterHandler conn
    :<|> successHandler
    :<|> registrationsHandler conn
    :<|> registrationsCsvHandler conn

registerHandler :: Handler Page.Html
registerHandler = do
    view <- DF.getForm "Registration" Form.registerForm
    pure $ Page.registerPage view

registrationsHandler :: Db.Connection -> () -> Handler Page.Html
registrationsHandler conn _ = do
    registrations <- liftIO $ Db.allRegistrations conn
    pure $ Page.registrationListPage registrations


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
        Csv.namedRecord [ "Name" Csv..= dbParticipantName, "Adresse" Csv..= TE.encodeUtf8 address, fixEncoding "Übernachtung" Csv..= sleeping dbParticipantSleepovers ]
      where
        address = (dbParticipantStreet <> ", " <> dbParticipantPostalCode <> " " <> dbParticipantCity) :: T.Text
        sleeping s = case s of
            FridayNight -> "Nur Freitag" :: T.Text
            SaturdayNight -> "Nur Samstag"
            AllNights -> "Samstag und Sonntag"
            NoNights -> "Keine Übernachtung"

registrationsCsvHandler :: Db.Connection -> () -> Handler BSL.ByteString
registrationsCsvHandler conn _ = do
    registrations <- liftIO $ Db.allRegistrations conn
    let headers = fixEncoding <$> V.fromList [ "Name", "Adresse", "Übernachtung" ]
    pure $ Csv.encodeByName headers $ fmap CsvParticipant registrations

postRegisterHandler :: Db.Connection -> [(T.Text, T.Text)] -> Handler Page.Html
postRegisterHandler conn body = do
    r <- DF.postForm "Registration" Form.registerForm $ servantPathEnv body
    case r of
        (view, Nothing) -> do
            liftIO $ putStrLn $ show view
            pure $ Page.registerPage view
        (_, Just registration) -> do
            liftIO $ Db.saveRegistration conn registration
            throwError $ err303 { errHeaders = [("Location", "/success")] }

successHandler :: Handler Page.Html
successHandler = do
    pure Page.successPage

servantPathEnv :: (Monad m) => [(T.Text, T.Text)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv body _ = pure env
  where
    lookupParam p = lookup (DF.fromPath p) body
    env path = return (DF.TextInput <$> (M.maybeToList (lookupParam path)))
