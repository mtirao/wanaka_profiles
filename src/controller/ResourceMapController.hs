{-# LANGUAGE OverloadedStrings #-}

module ResourceMapController(getMap, createMap, deleteMap) where

import ResourceMapDTO
import ErrorMessage

import Views ( jsonResponse )
import ResourceMap

import Data.Aeson

import Control.Monad.IO.Class

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Network.HTTP.Types.Status

getMap resource conn =  do
                            result <- liftIO $ findResourceMap resource conn
                            liftIO $ print resource
                            case result of
                                Right [] -> do
                                        jsonResponse (ErrorMessage "User not found")
                                        status forbidden403
                                Right a ->
                                        jsonResponse $ map toResourceMapDTO a 

createMap body conn =  do
                        b <- body 
                        case decode b :: Maybe ResourceMapDTO of
                            Nothing -> status badRequest400
                            Just a -> do                   
                                result <- liftIO $ insertResourceMap a conn
                                case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "User not found")
                                            status forbidden403
                                    Right [a] -> status noContent204

deleteMap userId conn =  do
                                        result <- liftIO $ deleteResourceMap userId conn
                                        case result of
                                            Right [] -> do
                                                    jsonResponse (ErrorMessage "User not found")
                                                    status forbidden403
                                            Right [a] -> status noContent204
 