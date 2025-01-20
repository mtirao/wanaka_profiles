{-# LANGUAGE OverloadedStrings #-}

module PermissionsController(getPermissions, createPermissions, deletePermissions) where

import UserPermissionsDTO
import ErrorMessage

import Views ( jsonResponse )
import UserPermissions

import Data.Aeson

import Control.Monad.IO.Class

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Network.HTTP.Types.Status

getPermissions resource conn =  do
                            result <- liftIO $ findUserPermission resource conn
                            liftIO $ print resource
                            case result of
                                Right [] -> do
                                        jsonResponse (ErrorMessage "User not found")
                                        status forbidden403
                                Right a ->
                                        jsonResponse $ map toUserPermissionsDTO a 

createPermissions body conn =  do
                        b <- body 
                        case decode b :: Maybe UserPermissionsDTO of
                            Nothing -> status badRequest400
                            Just a -> do                   
                                result <- liftIO $ insertUserPermission a conn
                                case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "User not found")
                                            status forbidden403
                                    Right [a] -> status noContent204

deletePermissions userId conn =  do
                                        result <- liftIO $ deleteUserPermission userId conn
                                        case result of
                                            Right [] -> do
                                                    jsonResponse (ErrorMessage "User not found")
                                                    status forbidden403
                                            Right [a] -> status noContent204
 