{-# LANGUAGE OverloadedStrings #-}

module GroupController(getGroup, createGroup, deleteUserGroup) where

import GroupsDTO
import ErrorMessage

import Views ( jsonResponse )
import Group

import Data.Aeson

import Control.Monad.IO.Class

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Network.HTTP.Types.Status

getGroup userId conn =  do
                            result <- liftIO $ findGroup userId conn
                            case result of
                                Right [] -> do
                                        jsonResponse (ErrorMessage "User not found")
                                        status forbidden403
                                Right a ->
                                        jsonResponse $ map toGroupDTO a 

createGroup body conn =  do
                        b <- body 
                        case decode b :: Maybe GroupsDTO of
                            Nothing -> status badRequest400
                            Just a -> do                   
                                result <- liftIO $ insertGroup a conn
                                case result of
                                    Right [] -> do
                                            jsonResponse (ErrorMessage "User not found")
                                            status forbidden403
                                    Right [a] -> status noContent204

deleteUserGroup userId conn =  do
                            result <- liftIO $ deleteGroup userId conn
                            case result of
                                Right [] -> do
                                        jsonResponse (ErrorMessage "User not found")
                                        status forbidden403
                                Right [a] -> status noContent204
 