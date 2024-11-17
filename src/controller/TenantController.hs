{-# LANGUAGE OverloadedStrings #-}
module TenantController(createUser, deleteUser, updateUserPassword) where

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
import Jose.Jwt (Jwt(Jwt))

import Auth(decodeToken, toInt64)

--- AUTH
createUser body conn =  do
        curTime <- liftIO getPOSIXTime
        h <- header "Authorization"
        b <- body
        let tenant = (decode b :: Maybe TenantDTO)
        case h of
                Nothing -> status unauthorized401
                Just auth -> do
                                let parse = T.breakOnEnd " " $ TL.toStrict auth 
                                let token = (decodeToken $ convert parse) :: Maybe Payload
                                case token of
                                        Nothing -> do
                                                jsonResponse (ErrorMessage "Invalid token payload")
                                                status unauthorized401
                                        Just authToken -> case tenant of
                                                Nothing -> status badRequest400
                                                Just a -> do
                                                        result <- liftIO $ insertTenant (getTenantName a) (getTenantPassword a) (getTenantId a) (toInt64 curTime) conn
                                                        case result of
                                                                Right [] -> do
                                                                        jsonResponse (ErrorMessage "User not found")
                                                                        status forbidden403
                                                                Right [b] -> status noContent204 

deleteUser conn =  do
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
                                                if tokenExperitionTime authToken >= toInt64 curTime then do
                                                        result <- liftIO $ deleteTenant (TL.toStrict  $ tokenUserId authToken) conn
                                                        case result of
                                                                Right [] -> do
                                                                        jsonResponse (ErrorMessage "User not found")
                                                                        status forbidden403
                                                                Right [a] -> status noContent204
                                                else do
                                                        jsonResponse (ErrorMessage "Token expired")
                                                        status unauthorized401   

updateUserPassword body conn =  do
        curTime <- liftIO getPOSIXTime
        h <- header "Authorization"
        b <- body
        let password = (decode b :: Maybe PasswordDTO)
        case h of
                Nothing -> status unauthorized401
                Just auth -> do
                                let parse = T.breakOnEnd " " $ TL.toStrict auth 
                                let token = (decodeToken $ convert parse) :: Maybe Payload
                                case token of
                                        Nothing -> do
                                                jsonResponse (ErrorMessage "Invalid token payload")
                                                status unauthorized401
                                        Just authToken -> case password of
                                                Nothing -> status badRequest400
                                                Just p -> 
                                                        if tokenExperitionTime authToken >= toInt64 curTime then do
                                                                result <- liftIO $ updatePassword (TL.toStrict $ tokenUserId authToken) (TL.toStrict p.password) conn
                                                                case result of
                                                                        Right [] -> do
                                                                                jsonResponse (ErrorMessage "User not found")
                                                                                status forbidden403
                                                                        Right [a] -> status noContent204
                                                        else do
                                                                jsonResponse (ErrorMessage "Token expired")
                                                                status unauthorized401   

-- Helpers
convert :: (T.Text, T.Text) -> (TL.Text, TL.Text)
convert (a, b) = (TL.fromStrict a, TL.fromStrict b)