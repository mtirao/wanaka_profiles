{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module UserPermissionsDTO where

import Data.Text
import Data.Aeson
import Control.Lens.Internal.CTypes (Int64)

-- Tenant
data UserPermissionsDTO = UserPermissionsDTO
    { permGroupExec :: Int64
    , permGroupRead :: Int64
    , permGroupWrite :: Int64
    , permOtherExec :: Int64
    , permOtherRead :: Int64
    , permOtherWrite :: Int64
    , permResource :: Text
    , permUserExec :: Int64
    , permUserRead :: Int64
    , permUserWrite :: Int64
    } deriving (Show)

instance FromJSON UserPermissionsDTO where
    parseJSON (Object v) = UserPermissionsDTO <$>
        v .: "groupexec" <*>
        v .: "groupread" <*>
        v .: "groupwrite" <*>
        v .: "otherexec" <*>
        v .: "otherread" <*>
        v .: "otherwrite" <*>
        v .: "resource" <*>
        v .: "userexec" <*>
        v .: "userread" <*>
        v .: "userwrite"

instance ToJSON UserPermissionsDTO where
    toJSON UserPermissionsDTO {..} = object [
            "groupexec" .= permGroupExec,
            "groupread" .= permGroupRead,
            "groupwrite" .= permGroupWrite,
            "otherexec" .= permOtherExec,
            "otherread" .= permOtherRead,
            "otherwrite" .= permOtherWrite,
            "resource" .= permResource, 
            "userexec" .= permUserExec,
            "userread" .= permUserRead,
            "userwrite" .= permUserWrite
        ]
