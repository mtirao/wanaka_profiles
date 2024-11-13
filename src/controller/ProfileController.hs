{-# LANGUAGE OverloadedStrings #-}
module ProfileController where

import ProfileDTO

import Views ( jsonResponse )
import Profile 

import Web.Scotty ( body, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Control.Monad.IO.Class

import qualified Data.Text.Internal.Lazy as TL

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

--- PROFILE
getProfile userId conn =  do
        curTime <- liftIO getPOSIXTime
        result <- liftIO $ findProfile userId conn
        case result of
                Right [] -> do
                        jsonResponse (ErrorMessage "User not found")
                        status forbidden403
                Right [a] ->
                        jsonResponse $ toProfileDTO a        