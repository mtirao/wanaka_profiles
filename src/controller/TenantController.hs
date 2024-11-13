{-# LANGUAGE OverloadedStrings #-}
module TenantController where

import ProfileDTO

import Views ( jsonResponse )
import Tenant 

import Web.Scotty ( body, status, ActionM )
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

--- AUTH
createUser body conn =  do
        b <- body
        let tenant = (decode b :: Maybe TenantDTO)
        case tenant of
                Nothing -> status badRequest400
                Just a ->  processJson a
        where processJson tenant = do
                result <- liftIO $ insertTenant (getTenantName tenant) (getTenantPassword tenant) conn
                case result of
                        Right [] -> do
                                jsonResponse (ErrorMessage "User not found")
                                status forbidden403
                        Right [a] -> status noContent204