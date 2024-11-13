module Main where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal as TI
import Data.ByteString.Conversion.To
import Data.ByteString.Internal
import Data.ByteString.Lazy.Internal
import Data.Pool(createPool)
import Data.ByteString.Lazy (fromStrict)

import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status
import Network.Wai (Request, pathInfo)
import Network.Wai.Middleware.HttpAuth

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

import Control.Monad.IO.Class

import Auth
import Tenant(getConnection)
import ProfileController


authSettings :: AuthSettings
authSettings = "My Realm" { authIsProtected = needsAuth }

needsAuth :: Request -> IO Bool
needsAuth req = return $ case pathInfo req of
  "admin":_ -> True   -- all admin pages need authentication
  _         -> False  -- everything else is public

main :: IO ()
main = do
    -- let tlsConfig = tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
    --    config    = setPort 3443 defaultSettings
    -- pool <- createPool (getConnection) close 1 40 10
    Right connection <- getConnection
    scotty 3000 $ do 
        middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
        middleware logStdout
        -- middleware $ basicAuth (\u p -> validatePassword pool (fromStrict u) (fromStrict p)) authSettings
        -- AUTH
        post   "/api/wanaka/accounts/login" $ userAuthenticate body connection
        get "/api/wanaka/profile/:id" $ do  
                                        idd <- param "id" :: ActionM TL.Text
                                        getProfile (TI.pack (TL.unpack idd)) connection
        -- PROFILES AUTH
        -- post "/admin/wanaka/profile" $ createProfile pool
        -- get "/admin/wanaka/profile/:id" $ do   -- Query over ProfileView, which includes Patient information
        --                                idd <- param "id" :: ActionM TL.Text
        --                                getProfile pool idd
        -- put "/admin/wanaka/profile/:id" $ do
        --                                idd <- param "id" :: ActionM TL.Text
        --                                updateProfile pool idd
        
        -- PROFILES API
        -- post "/api/wanaka/profile" $ createProfile pool
        -- put "/api/wanaka/profile/:id" $ do
        --                                idd <- param "id" :: ActionM TL.Text
        --                                updateProfile pool idd