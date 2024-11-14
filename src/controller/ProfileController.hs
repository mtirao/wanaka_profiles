{-# LANGUAGE OverloadedStrings #-}
module ProfileController(getProfile, createProfile, deleteUserProfile) where

import ProfileDTO

import Views ( jsonResponse )
import Profile 

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Control.Monad.IO.Class

import qualified Data.Text.Lazy as TL

import qualified Data.ByteString.Lazy.Internal as BL 
import qualified Data.Text as T

import Data.Time
import Data.Time.Clock.POSIX

import GHC.Int

import Network.HTTP.Types.Status

import Data.Aeson

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))

import Auth(decodeToken, toInt64)

--- PROFILE
getProfile userId conn =  do
        curTime <- liftIO getPOSIXTime
        h <- header "Authorization"
        result <- liftIO $ findProfile userId conn
        case h of
                Nothing -> status unauthorized401
                Just auth -> do
                        let parse = T.breakOnEnd " " $ TL.toStrict auth 
                        let token = (decodeToken $ convert parse) :: Maybe Payload
                        case token of
                                Nothing -> do
                                        jsonResponse (ErrorMessage "Invalid token payload")
                                        status unauthorized401
                                Just authToken -> 
                                        if (tokenExperitionTime authToken) >= (toInt64 curTime) then 
                                                case result of
                                                        Right [] -> do
                                                                jsonResponse (ErrorMessage "User not found")
                                                                status forbidden403
                                                        Right [a] ->
                                                                jsonResponse $ toProfileDTO a
                                        else do
                                                jsonResponse (ErrorMessage "Token expired")
                                                status unauthorized401                                                      


createProfile body conn =  do
        b <- body
        h <- header "Authorization"
        curTime <- liftIO getPOSIXTime
        let profile = (decode b :: Maybe ProfileDTO)
        case h of
                Nothing -> status unauthorized401
                Just auth -> do
                        let parse = T.breakOnEnd " " $ TL.toStrict auth
                        let token = (decodeToken $ convert parse) :: Maybe Payload
                        case token of
                                Nothing -> do
                                        jsonResponse (ErrorMessage "Invalid token payload")
                                        status unauthorized401
                                Just authToken -> case profile of
                                        Nothing -> status badRequest400
                                        Just a ->  
                                                if (tokenExperitionTime authToken) >= (toInt64 curTime) then 
                                                         do
                                                                result <- liftIO $ insertProfile a conn
                                                                case result of
                                                                        Right [] -> do
                                                                                jsonResponse (ErrorMessage "User not found")
                                                                                status forbidden403
                                                                        Right [a] -> status noContent204
                                                else do
                                                        jsonResponse (ErrorMessage "Token expired")
                                                        status unauthorized401   

deleteUserProfile conn =  do
        curTime <- liftIO getPOSIXTime
        h <- header "Authorization"
        case h of
                Nothing -> status unauthorized401
                Just auth -> do
                                let parse = T.breakOnEnd " " $ TL.toStrict auth 
                                let token = (decodeToken $ convert parse) :: Maybe Payload
                                case token of
                                        Nothing -> do
                                                jsonResponse (ErrorMessage "Invalid token payload")
                                                status unauthorized401
                                        Just authToken -> 
                                                if (tokenExperitionTime authToken) >= (toInt64 curTime) then do
                                                        result <- liftIO $ deleteProfile (convertSingle $ tokenUserId authToken) conn
                                                        case result of
                                                                Right [] -> do
                                                                        jsonResponse (ErrorMessage "User not found")
                                                                        status forbidden403
                                                                Right [a] -> status noContent204
                                                else do
                                                        jsonResponse (ErrorMessage "Token expired")
                                                        status unauthorized401   

convert :: (T.Text, T.Text) ->  (TL.Text, TL.Text)
convert (a, b) = (TL.fromStrict a, TL.fromStrict b)

convertSingle :: TL.Text -> T.Text
convertSingle a = TL.toStrict a