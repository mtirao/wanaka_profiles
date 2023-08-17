{-# LANGUAGE OverloadedStrings #-}

module ProfileController(getProfile, createProfile, deleteUserProfile, updateUserProfile) where

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

import ErrorMessage
import Control.Lens (use)

--- PROFILE
getProfile userId conn =  do
                            result <- liftIO $ findProfile userId conn
                            case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "User not found")
                                            status notFound404
                                    Right [a] ->
                                            jsonResponse $ toProfileDTO a
                                                 

createProfile body conn =  do
        b <- body
        let profile = (decode b :: Maybe ProfileDTO) 
        case profile of
                Nothing -> status badRequest400
                Just a -> do                   
                            result <- liftIO $ insertProfile a conn
                            case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "User can be created")
                                            status forbidden403
                                    Right [a] -> status noContent204
                                                

deleteUserProfile userId conn =  do
                                    result <- liftIO $ deleteProfile userId conn
                                    case result of
                                            Right [] -> do
                                                    jsonResponse (ErrorMessage "User not found")
                                                    status notFound404
                                            Right [a] -> status noContent204
                                                

updateUserProfile userId body conn =  do
        b <- body
        let profile = (decode b :: Maybe ProfileDTO)
        case profile of
            Nothing -> status badRequest400
            Just p -> do
                    result <- liftIO $ updateProfile userId p conn
                    case result of
                            Right [] -> do
                                    jsonResponse (ErrorMessage "User not found")
                                    status notFound404
                            Right [a] -> status noContent204
