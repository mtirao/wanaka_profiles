{-# LANGUAGE OverloadedStrings #-}
module TenantController(createUser, deleteUser, updateUserPassword) where

import ProfileDTO

import Views ( jsonResponse )
import Tenant

import Evaluator

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
import AuthDTO
import TenantDTO
import ErrorMessage

--- AUTH
createUser body conn =  do
        curTime <- liftIO getPOSIXTime
        b <- body
        let tenant = (decode b :: Maybe TenantDTO)
        case tenant of
            Nothing -> status badRequest400
            Just a -> do
                    result <- liftIO $ insertTenant (TL.toStrict a.userName) (TL.toStrict a.userPassword) (TL.toStrict a.userRole) (TL.toStrict a.userId) (toInt64 curTime) conn
                    case result of
                            Right [] -> do
                                    jsonResponse (ErrorMessage "User not found")
                                    status forbidden403
                            Right [b] -> status noContent204

deleteUser userId conn =  do
                            result <- liftIO $ deleteTenant userId conn
                            case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "User not found")
                                            status forbidden403
                                    Right [a] -> status noContent204
                                                

updateUserPassword userId body conn =  do
        curTime <- liftIO getPOSIXTime
        b <- body
        let password = (decode b :: Maybe PasswordDTO)
        case password of
            Nothing -> status badRequest400
            Just p -> do
                        result <- liftIO $ updatePassword userId (TL.toStrict p.password) conn
                        case result of
                                Right [] -> do
                                        jsonResponse (ErrorMessage "User not found")
                                        status forbidden403
                                Right [a] -> status noContent204