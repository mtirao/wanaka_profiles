{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ProfilesController where

import Domain
import Views
import Profiles
import Db

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)


import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import GHC.Int
import GHC.Generics (Generic)

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types.Status

import Data.Aeson

---CREATE
createProfile pool = do
                        b <- body
                        profile <- return $ (decode b :: Maybe Profile)
                        case profile of
                            Nothing -> status status400
                            Just _ -> profileResponse pool profile

profileResponse pool profile = do 
                                dbProfile <- liftIO $ insert pool profile
                                case dbProfile of
                                        Nothing -> status status400
                                        Just a -> dbProfileResponse 
                                                where dbProfileResponse  = do
                                                                        jsonResponse a
                                                                        status status201

---UPDATE
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
getProfile pool idd = do
                        adults <- liftIO $ (find pool idd :: IO (Maybe Profile))
                        jsonResponse adults