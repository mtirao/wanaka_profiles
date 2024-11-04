module Main where

import Db

import Domain
import Views

import ProfilesController

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text.Lazy as TL
import Data.ByteString.Conversion.To

import Data.ByteString.Internal
import Data.ByteString.Lazy.Internal

import Data.Pool(createPool)
import Data.Aeson

import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status

import Network.Wai (Request, pathInfo)
import Network.Wai.Middleware.HttpAuth
import Data.ByteString.Lazy (fromStrict)

-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe DbConfig)
makeDbConfig conf = do
    dbConfname <- C.lookup conf "database.name" :: IO (Maybe String)
    dbConfUser <- C.lookup conf "database.user" :: IO (Maybe String)
    dbConfPassword <- C.lookup conf "database.password" :: IO (Maybe String)
    dbConfHost <- C.lookup conf "database.host" :: IO (Maybe String)
    return $ DbConfig <$> dbConfname
                    <*> dbConfUser
                    <*> dbConfPassword
                    <*> dbConfHost

authSettings :: AuthSettings
authSettings = "My Realm" { authIsProtected = needsAuth }

needsAuth :: Request -> IO Bool
needsAuth req = return $ case pathInfo req of
  "admin":_ -> True   -- all admin pages need authentication
  _         -> False  -- everything else is public

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    let tlsConfig = tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
        config    = setPort 3443 defaultSettings
    case dbConf of
        Nothing -> putStrLn "No database configuration found, terminating..."
        Just conf -> do
            pool <- createPool (newConn conf) close 1 40 10
            scotty 3000 $ do 
                middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
                middleware logStdout
                middleware $ basicAuth (\u p -> validatePassword pool (fromStrict u) (fromStrict p)) authSettings
                -- AUTH
                post   "/api/smartlist/accounts/login" $ userAuthenticate body pool

                -- PROFILES AUTH
                post "/admin/smartlist/profile" $ createProfile pool
                get "/admin/smartlist/profile/:id" $ do   -- Query over ProfileView, which includes Patient information
                                                idd <- param "id" :: ActionM TL.Text
                                                getProfile pool idd
                put "/admin/smartlist/profile/:id" $ do
                                                idd <- param "id" :: ActionM TL.Text
                                                updateProfile pool idd
                
                -- PROFILES API
                post "/api/smartlist/profile" $ createProfile pool
                get "/api/smartlist/profile/:id" $ do   -- Query over ProfileView, which includes Patient information
                                                idd <- param "id" :: ActionM TL.Text
                                                getProfile pool idd
                put "/api/smartlist/profile/:id" $ do
                                                idd <- param "id" :: ActionM TL.Text
                                                updateProfile pool idd