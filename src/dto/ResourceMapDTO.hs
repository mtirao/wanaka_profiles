{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module ResourceMapDTO where

import Data.Text
import Data.Aeson

-- Tenant
data ResourceMapDTO = ResourceMapDTO
    { resMapUserId :: Text
    , resMapGroupId :: Text
    , resMapResource :: Text
    } deriving (Show)

instance FromJSON ResourceMapDTO where
    parseJSON (Object v) = ResourceMapDTO <$>
        v .: "userid" <*>
        v .: "groupid" <*>
        v .: "resource"

instance ToJSON ResourceMapDTO where
    toJSON ResourceMapDTO {..} = object [
            "userid" .= resMapUserId,
            "groupid" .= resMapGroupId,
            "resource" .= resMapResource
        ]
