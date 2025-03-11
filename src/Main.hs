module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal as TI
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Hasql.Connection as S
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Data.Time.Clock

import ProfileController

-- MIDLEWARES

data DbConfig = DbConfig
    { dbName     :: String
    , dbUser     :: String
    , dbPassword :: String
    , dbHost     :: String
    , dbPort     :: Int
    }

makeDbConfig :: C.Config -> IO (Maybe DbConfig)
makeDbConfig conf = do
    dbConfname <- C.lookup conf "database.name" :: IO (Maybe String)
    dbConfUser <- C.lookup conf "database.user" :: IO (Maybe String)
    dbConfPassword <- C.lookup conf "database.password" :: IO (Maybe String)
    dbConfHost <- C.lookup conf "database.host" :: IO (Maybe String)
    dbConfPort <- C.lookup conf "database.port" :: IO (Maybe Int)
    return $ DbConfig <$> dbConfname
                      <*> dbConfUser
                      <*> dbConfPassword
                      <*> dbConfHost
                      <*> dbConfPort

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    case dbConf of
        Nothing -> putStrLn "Error loading configuration"
        Just conf -> do
            let connSettings = S.settings (encodeUtf8 $ pack $ dbHost conf)
                                        (fromIntegral $ dbPort conf)
                                        (encodeUtf8 $ pack $ dbUser conf)
                                        (encodeUtf8 $ pack $ dbPassword conf)
                                        (encodeUtf8 $ pack $ dbName conf)
            result <- S.acquire connSettings
            case result of
                Left err -> putStrLn $ "Error acquiring connection: " ++ show err
                Right pool -> scotty 3002 $ do
                    middleware logStdoutDev
                    -- PROFILE
                    get "/api/wanaka/profile/:id" $ do
                        idd <- param "id" :: ActionM TL.Text
                        getProfile (TI.pack (TL.unpack idd)) pool
                    post "/api/wanaka/profile" $ createProfile body pool
                    delete "/api/wanaka/profile/:id" $ do
                        idd <- param "id" :: ActionM TL.Text
                        deleteUserProfile (TI.pack (TL.unpack idd)) pool
                    put "/api/wanaka/profile/:id" $ do
                        idd <- param "id" :: ActionM TL.Text
                        updateUserProfile (TI.pack (TL.unpack idd)) body pool