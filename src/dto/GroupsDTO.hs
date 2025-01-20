{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module GroupsDTO where

import Data.Text
import Data.Aeson

-- Tenant
data GroupsDTO = GroupsDTO 
    { groupUserId :: Text
    , groupId :: Text
    } deriving (Show)

instance FromJSON GroupsDTO where
    parseJSON (Object v) = GroupsDTO <$>
        v .: "userid" <*>
        v .: "groupid"

instance ToJSON GroupsDTO where
    toJSON GroupsDTO {..} = object [
            "userid" .= groupUserId ,
            "groupid" .= groupId
        ]

