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
import Network.Wai 
import Network.Wai.Middleware.HttpAuth

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

import Data.Time
import Data.Time.Clock.POSIX

import Views
import ErrorMessage
import Control.Monad.IO.Class
import Auth
import Tenant
import Connection
import ProfileController
import TenantController
import GroupController
import PermissionsController
import ResourceMapController
import Evaluator
import AuthDTO (tokenExperitionTime)
import Data.Text.Encoding (decodeUtf8)
import Control.Arrow (ArrowApply(app))
import qualified Data.Text.Encoding as T


validateTokenAuthorization :: Middleware
validateTokenAuthorization app req respond = do 
                                    let path = rawPathInfo req
                                    if path == "/api/wanaka/accounts/login" then do
                                        app req respond
                                    else do
                                        let maybeAuthHeader = lookup "Authorization" (requestHeaders req)
                                        print maybeAuthHeader
                                        case maybeAuthHeader of
                                            Just authHeader -> do
                                                case extractBearerAuth authHeader of
                                                    Just auth ->  do 
                                                        Right connection <- getConnection
                                                        result <- liftIO $ findTenantByToken (T.decodeUtf8 auth) connection
                                                        print result
                                                        case result of
                                                            Right [] -> do
                                                                respond $ responseLBS status401 [] "Unathorized"
                                                            Right [a] -> do
                                                                    app req respond
                                                    Nothing -> respond $ responseLBS status401 [] "Unathorized"
                                            Nothing -> respond $ responseLBS status401 [] "Unathorized"

validateAuthorization :: Middleware
validateAuthorization app req respond = do
                                    let path = rawPathInfo req
                                    if path == "/api/wanaka/accounts/login" then do
                                        let maybeAuthHeader = lookup "Authorization" (requestHeaders req)
                                        case maybeAuthHeader of
                                            Just authHeader -> do
                                                case extractBasicAuth authHeader of
                                                    Just _ ->  app req respond
                                                    Nothing -> respond $ responseLBS status401 [] "Unathorized"
                                            Nothing -> respond $ responseLBS status401 [] "Unathorized"
                                    else do 
                                        let maybeAuthHeader = lookup "Authorization" (requestHeaders req)
                                        case maybeAuthHeader of
                                            Nothing -> respond $ responseLBS status401 [] "Unathorized"
                                            Just authHeaader -> do
                                                case extractBearerAuth authHeaader of
                                                    Nothing -> respond $ responseLBS status401 [] "Unathorized"
                                                    Just auth -> do
                                                        let token =  decodeToken (TL.fromStrict $ T.decodeUtf8 auth)
                                                        curTime <- liftIO getPOSIXTime
                                                        print auth
                                                        case token of
                                                            Nothing -> do
                                                                respond $ responseLBS status401 [] "Unathorized"
                                                            Just authToken -> if tokenExperitionTime authToken >= toInt64 curTime then 
                                                                                app req respond
                                                                            else 
                                                                                respond $ responseLBS status401 [] "Unathorized"

main :: IO ()
main = do
    -- let tlsConfig = tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
    --    config    = setPort 3443 defaultSettings
    -- pool <- createPool (getConnection) close 1 40 10
    Right connection <- getConnection
    scotty 3000 $ do 
        middleware validateTokenAuthorization
        middleware validateAuthorization
        middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
        middleware logStdout
        -- AUTH
        post "/api/wanaka/accounts/login" $ userAuthenticate connection
        get "/api/wanaka/accounts/validate" $ validateUserToken connection

        -- TENANT
        post "/api/wanaka/accounts" $ createUser body connection
        delete "/api/wanaka/accounts/:id" $ do  
                                        idd <- param "id" :: ActionM TL.Text
                                        deleteUser (TI.pack (TL.unpack idd)) connection
        patch "/api/wanaka/accounts/:id" $ do  
                                        idd <- param "id" :: ActionM TL.Text
                                        updateUserPassword (TI.pack (TL.unpack idd)) body connection

        -- PROFILE
        get "/api/wanaka/profile/:id" $ do  
                                        idd <- param "id" :: ActionM TL.Text
                                        getProfile (TI.pack (TL.unpack idd)) connection
        post "/api/wanaka/profile" $ createProfile body connection
        delete "/api/wanaka/profile/:id" $ do  
                                        idd <- param "id" :: ActionM TL.Text
                                        deleteUserProfile (TI.pack (TL.unpack idd)) connection
        put "/api/wanaka/profile/:id" $ do  
                                    idd <- param "id" :: ActionM TL.Text 
                                    updateUserProfile (TI.pack (TL.unpack idd)) body connection
        
        -- GROUP
        post "/api/wanaka/group" $ createGroup body connection
        get "/api/wanaka/group/:id" $ do
                                    idd <- param "id" :: ActionM TL.Text
                                    getGroup (TI.pack (TL.unpack idd)) connection
        delete "/api/wanaka/group/:id" $ do
                                    idd <- param "id" :: ActionM TL.Text
                                    deleteUserGroup (TI.pack (TL.unpack idd)) connection
        
        -- USER PERMISSIONS
        post "/api/wanaka/permission" $ createPermissions body connection
        get "/api/wanaka/permission/" $ do
                                    idd <- param "resource" :: ActionM TL.Text
                                    getPermissions (TI.pack (TL.unpack idd)) connection
        delete "/api/wanaka/permission/" $ do
                                    idd <- param "resource" :: ActionM TL.Text
                                    deletePermissions (TI.pack (TL.unpack idd)) connection

        -- RESOURCE MAP
        post "/api/wanaka/map" $ createMap body connection
        get "/api/wanaka/map/" $ do
                                    idd <- param "resource" :: ActionM TL.Text
                                    getMap (TI.pack (TL.unpack idd)) connection
        delete "/api/wanaka/map/" $ do
                                    idd <- param "resource" :: ActionM TL.Text
                                    deleteMap (TI.pack (TL.unpack idd)) connection