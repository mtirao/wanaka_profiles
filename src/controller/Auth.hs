{-# LANGUAGE OverloadedStrings #-}

module Auth(userAuthenticate, decodeToken, validateUserToken, toInt64) where

import ProfileDTO

import Views ( jsonResponse )
import Tenant
import AuthDTO
import ErrorMessage

import Evaluator

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)
import Network.Wai

import Control.Monad.IO.Class

import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.Text as T
import qualified Data.ByteString.Internal as BI
import Data.Time
import Data.Time.Clock.POSIX

import GHC.Int

import Network.HTTP.Types.Status

import Data.Aeson

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt), JwsHeader(JwsHeader))
import Network.Wai.Middleware.HttpAuth
import qualified Data.Text.Encoding as T

--- AUTH
userAuthenticate conn =  do
        curTime <- liftIO getPOSIXTime
        let expDate = tokenExpiration curTime
        h <- header "Authorization"
        case h >>= extractBasicAuth . T.encodeUtf8 . TL.toStrict of
            Just (user, password) -> do
                let userText = T.decodeUtf8 user
                let passwordText = T.decodeUtf8 password
                result <- liftIO $ findTenant userText passwordText conn
                case result of
                   Right [] -> do
                       jsonResponse (ErrorMessage "User not found")
                       status forbidden403
                   Right [a] -> do
                        rest <- liftIO $ updateTokens a (TL.toStrict token) "" conn
                        jsonResponse (LoginResponse token "JWT" "" )
                        where 
                            token = createToken (TL.pack (T.unpack a)) expDate
            Nothing -> status unauthorized401


validateUserToken conn = do
                curTime <- liftIO getPOSIXTime
                authHeader <- header "Authorization"
                case  authHeader >>= extractBearerAuth. T.encodeUtf8 . TL.toStrict  of
                        Nothing -> status unauthorized401
                        Just auth -> case decodeToken (TL.fromStrict $ T.decodeUtf8 auth) of
                                        Nothing -> do
                                                jsonResponse (ErrorMessage "Invalid token payload")
                                                status unauthorized401
                                        Just payload -> if tokenExperitionTime payload  >= toInt64 curTime then do
                                                            jsonResponse $ UserResponse (tokenUserId payload)
                                                        else do
                                                            jsonResponse $ ErrorMessage "Token expired"
                                                            status unauthorized401


-- Token helpers
createToken :: Text -> Int64 -> Text
createToken u  t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> TL.pack $ BI.unpackChars jwt
                    where
                        payload = BI.packChars $ convertToString u t
                        token = hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" payload

-- Helpers

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = toInt64 u + 864000

toInt64 :: NominalDiffTime -> Int64
toInt64 = floor . nominalDiffTimeToSeconds

convertToString :: Text -> Int64 -> [Char]
convertToString u t = BL.unpackChars (encode $ Payload u t)

convertToStrict :: (T.Text, T.Text) -> (Text, Text)
convertToStrict (a, b) = (TL.fromStrict a, TL.fromStrict b)