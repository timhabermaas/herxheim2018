{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    ) where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze (HTML)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Text.Digestive.Form.Encoding as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.View as DF

import qualified Db as Db
import qualified Html as Page
import qualified Form as Form
import qualified Data.Maybe as M


type API
    = "register" :> Get '[HTML] Page.Html
 :<|> "register" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Page.Html
 :<|> "success" :> Get '[HTML] Page.Html
 :<|> "registrations" :> Get '[HTML] Page.Html


startApp :: String -> Int -> IO ()
startApp dbUrl port = do
    conn <- Db.connect dbUrl
    Db.migrate conn
    run port $ logStdoutDev $ app conn

app :: Db.Connection -> Application
app conn = serve api $ server conn

api :: Proxy API
api = Proxy

server :: Db.Connection -> Server API
server conn = registerHandler :<|> postRegisterHandler conn :<|> successHandler :<|> registrationsHandler conn

registerHandler :: Handler Page.Html
registerHandler = do
    view <- DF.getForm "Registration" Form.registerForm
    pure $ Page.registerPage view

registrationsHandler :: Db.Connection -> Handler Page.Html
registrationsHandler conn = do
    registrations <- liftIO $ Db.allRegistrations conn
    pure $ Page.registrationListPage registrations


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
