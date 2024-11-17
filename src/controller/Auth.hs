{-# LANGUAGE OverloadedStrings #-}
module Auth(userAuthenticate, refreshUserToken, decodeToken, toInt64) where

import ProfileDTO

import Views ( jsonResponse )
import Tenant 

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

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

--- AUTH
userAuthenticate body conn =  do
        b <- body
        curTime <- liftIO getPOSIXTime
        let login = (decode b :: Maybe Login)
        let expDate = tokenExpiration curTime
        let refresExpDate = refreshTokenExp curTime
        result <- liftIO $ findTenant (getUserName login) (getPassword login) conn
        case result of
                Right [] -> do
                        jsonResponse (ErrorMessage "User not found")
                        status forbidden403
                Right [a] ->
                        jsonResponse (LoginResponse token "JWT" refreshToken )
                        where 
                                token = createToken (TL.pack (T.unpack a)) expDate
                                refreshToken = createRefreshToken (TL.pack (T.unpack a)) refresExpDate

refreshUserToken conn = do
                h <- header "Authorization"
                rh <- header "X-Refresh-Token"
                case h of
                        Nothing -> status unauthorized401
                        Just auth -> do
                                case parseToken auth of
                                        Nothing -> do
                                                jsonResponse (ErrorMessage "Invalid token payload")
                                                status unauthorized401
                                        Just authToken -> checkTokenAndUpdate authToken rh conn     
                                where 
                                        parseToken tkn = (decodeToken $ breakOnEnd " " tkn ) :: Maybe Payload
                                        parseRefreshToken tkn = (decodeeRefreshToken $ breakOnEnd " " tkn ) :: Maybe Payload 


-- Checker helpers
checkTokenDBUpdate :: Either a1 [a2] -> Text -> Text -> ActionM ()
checkTokenDBUpdate result token refToken =  case result of
                                Right [] ->  status badRequest400
                                Right [a] ->  jsonResponse $ LoginResponse token "JWT" refToken                       

checkTokenAndUpdate authToken rh conn =  do
                                curTime <- liftIO getPOSIXTime
                                let expDate = tokenExpiration curTime
                                let refresExpDate = refreshTokenExp curTime
                                if tokenExperitionTime authToken >= toInt64 curTime then
                                        case rh of 
                                                Nothing -> status unauthorized401
                                                Just refresh -> 
                                                        case parseRefreshToken refresh of
                                                                Nothing -> status unauthorized401
                                                                Just refreToken -> 
                                                                        if tokenExperitionTime authToken >= toInt64 curTime then do
                                                                                let token = createToken (tokenUserId authToken) expDate
                                                                                let refToken = createRefreshToken (tokenUserId authToken) refresExpDate
                                                                                result <- liftIO $ updateTokens (TL.toStrict $ tokenUserId authToken) (TL.toStrict token) (TL.toStrict refToken) conn
                                                                                checkTokenDBUpdate result token refToken 
                                                                        else do 
                                                                                jsonResponse $ ErrorMessage "Token expired"
                                                                                status unauthorized401  
                                else do
                                        jsonResponse (ErrorMessage "Token expired")
                                        status unauthorized401  
                                where 
                                        parseToken tkn = (decodeToken $ breakOnEnd " " tkn ) :: Maybe Payload
                                        parseRefreshToken tkn = (decodeeRefreshToken $ breakOnEnd " " tkn ) :: Maybe Payload 


-- Token helpers
createToken :: Text -> Int64 -> Text
createToken u  t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> TL.pack $ BI.unpackChars jwt
                    where 
                        payload = BI.packChars $ convertToString u t
                        token = hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" payload

decodeToken :: (Text, Text) -> Maybe Payload
decodeToken t = case token of 
                    Left _ -> Nothing
                    Right (_, jwt) -> convertToPayload jwt
                where 
                    token = hmacDecode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" $ tokenFromHeader t

createRefreshToken :: Text -> Int64 -> Text
createRefreshToken u  t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> TL.pack $ BI.unpackChars jwt
                    where 
                        payload = BI.packChars $ convertToString u t
                        token = hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJhbGciOiJIUzI1N" payload

decodeeRefreshToken :: (Text, Text) -> Maybe Payload
decodeeRefreshToken t = case token of 
                    Left _ -> Nothing
                    Right (_, jwt) -> convertToPayload jwt
                where 
                    token = hmacDecode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJhbGciOiJIUzI1N" $ tokenFromHeader t


-- Helpers
nanosSinceEpoch :: NominalDiffTime -> Int64
nanosSinceEpoch = floor  . nominalDiffTimeToSeconds

secondsSinceEpoch :: NominalDiffTime -> Int64
secondsSinceEpoch = nanosSinceEpoch

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = secondsSinceEpoch u + 864000

refreshTokenExp :: NominalDiffTime -> Int64
refreshTokenExp u = secondsSinceEpoch u + 864000 * 2

toInt64 :: NominalDiffTime -> Int64
toInt64 = secondsSinceEpoch

convertToString :: Text -> Int64 -> [Char]
convertToString u t = BL.unpackChars (encode $ Payload u t)                 

convertToPayload :: BI.ByteString -> Maybe Payload
convertToPayload t = ( decode $  BL.packChars $ BI.unpackChars t ) :: Maybe Payload

tokenFromHeader :: (Text, Text) -> BI.ByteString
tokenFromHeader (typ, token) = BL.toStrict $ TL.encodeUtf8 token 
