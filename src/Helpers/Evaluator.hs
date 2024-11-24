{-# LANGUAGE OverloadedStrings #-}

module Evaluator where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)
import Network.HTTP.Types.Status
import ProfileDTO
import Views ( jsonResponse )

maybeEvaluation :: Maybe t -> (t -> ActionT IO ()) -> ActionT IO ()
maybeEvaluation maybe func =  case maybe of
                            Nothing -> do
                                    jsonResponse (ErrorMessage "Invalid token payload")
                                    status badRequest400
                            Just value -> func value