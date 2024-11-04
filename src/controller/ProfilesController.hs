{-# LANGUAGE OverloadedStrings #-}
module ProfilesController where

import Domain ( Profile (userPassword, userId), Login, getUserName, ErrorMessage (ErrorMessage), getPassword, extractPassword, convertToString, LoginResponse(LoginResponse), Token(Token))
import Views ( jsonResponse )
import Profiles ()
import Db ( DbOperation(find, insert, update), findUserByLogin )

import Web.Scotty ( body, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)


import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL 
import qualified Data.Text as T
import qualified Data.ByteString.Internal as BI

import GHC.Int
import GHC.Generics (Generic)

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types.Status

import Data.Aeson

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))

---CREATE
createProfile :: Pool Connection -> ActionT TL.Text IO ()
createProfile pool = do
                        b <- body
                        profile <- return $ (decode b :: Maybe Profile)
                        case profile of
                            Nothing -> status status400
                            Just _ -> profileResponse pool profile

profileResponse :: (DbOperation a, ToJSON a) => Pool Connection -> Maybe a -> ActionT TL.Text IO ()
profileResponse pool profile = do
                                dbProfile <- liftIO $ insert pool profile
                                case dbProfile of
                                        Nothing -> status status400
                                        Just a -> dbProfileResponse
                                                where dbProfileResponse  = do
                                                                        jsonResponse a
                                                                        status status200

---UPDATE
updateProfile :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
updateProfile pool id = do
                        b <- body
                        profile <- return $ (decode b :: Maybe Profile)
                        case profile of
                            Nothing -> status status400
                            Just _ -> updateProfileResponse pool profile id

updateProfileResponse pool profile id  = do
                                        dbProfile <- liftIO $  update pool profile id
                                        case dbProfile of
                                                Nothing -> status status404
                                                Just a -> dbProfileResponse
                                                        where dbProfileResponse  = do
                                                                                jsonResponse a
                                                                                status status200

--- GET
getProfile :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
getProfile pool idd = do
                        adults <- liftIO $ (find pool idd :: IO (Maybe Profile))
                        jsonResponse adults

--- AUTH
userAuthenticate :: ActionM BL.ByteString -> Pool Connection -> ActionT TL.Text IO ()
userAuthenticate body pool =  do
        b <- body
        let login = (decode b :: Maybe Login)
        result <- liftIO $ findUserByLogin pool (TL.unpack (getUserName login))
        case result of
                Nothing -> do
                        jsonResponse (ErrorMessage "User not found")
                        status forbidden403
                Just a ->
                        if extractPassword (userPassword a) == getPassword login
                        then jsonResponse (LoginResponse (createToken $ userId a) "JWT" 3600 "refreshToken" )
                        else do
                                jsonResponse (ErrorMessage "Wrong password")
                                status forbidden403




createToken u = case hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" (BI.packChars $ convertToString u) of
                Left _ -> ""
                Right (Jwt jwt) -> TL.pack $ BI.unpackChars jwt