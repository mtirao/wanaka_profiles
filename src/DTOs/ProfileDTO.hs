{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}


module ProfileDTO where

import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import GHC.Generics
import Data.Time.LocalTime
import Data.Int
import Data.Time.Clock.POSIX
import Data.Time
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import qualified Data.Maybe
import qualified Data.ByteString.Lazy.Internal as BI


-- Login Response
data LoginResponse = LoginResponse
    { accessToken :: Text
    , tokenType :: Text
    , refreshToken :: Text
    } deriving (Show)

instance ToJSON LoginResponse where
    toJSON (LoginResponse accessToken tokenType refreshToken) = object
        [
            "accesstoken" .= accessToken,
            "tokentype" .= tokenType,
            "refreshtoken" .= refreshToken
        ]                           

-- Login
data Login = Login Text Text -- username password
    deriving (Show)

instance ToJSON Login where
    toJSON (Login username password) = object
        [
            "username" .= username,
            "password" .= password
        ]

instance FromJSON Login where
    parseJSON (Object v) = Login <$>
        v .:  "username" <*>
        v .:  "password"

-- Getters
getUserName :: Maybe Login -> T.Text
getUserName a = case a of
                Nothing -> ""
                Just (Login u p) -> T.pack (TL.unpack u)

getPassword :: Maybe Login -> T.Text
getPassword a = case a of
                Nothing -> ""
                Just (Login u p) -> T.pack (TL.unpack p)

-- Authentication
data Payload = Payload
    {
        user :: Text,
        exp :: Int64
    } deriving (Show)

instance ToJSON Payload where
    toJSON (Payload user exp) = object
        [
            "user" .= user,
            "exp" .= exp
        ]                           

--ErrorMessage
data ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]

-- Tenant
data TenantDTO = TenantDTO 
    { userName :: Text
    , userPassword :: Text
    , userRole :: Text
    , userId :: Text
    } deriving (Show)

instance FromJSON TenantDTO where
    parseJSON (Object v) = TenantDTO <$>
        v .: "username" <*>
        v .: "userpassword" <*>
        v .: "userrole" <*>
        v .: "userid"

-- Getters
getTenantName :: TenantDTO -> T.Text
getTenantName a = T.pack (TL.unpack (userName a))

getTenantPassword :: TenantDTO -> T.Text
getTenantPassword a = T.pack (TL.unpack (userPassword a))

-- Profile
data ProfileDTO = ProfileDTO
    { cellPhone :: TI.Text
    , email :: TI.Text
    , firstName :: TI.Text
    , lastName :: TI.Text
    , phone :: TI.Text
    , gender :: TI.Text
    , address :: TI.Text
    , city :: TI.Text
    } deriving (Show)


instance ToJSON ProfileDTO where
    toJSON ProfileDTO {..} = object [
            "cellphone" .= cellPhone,
            "email" .= email,
            "firstname" .= firstName,
            "lastname" .= lastName,
            "phone" .= phone,
            "gender" .= gender,
            "address" .= address,
            "city" .= city
        ]

instance FromJSON ProfileDTO where
    parseJSON (Object v) = ProfileDTO <$>
        v .:  "cellphone" <*>
        v .:  "email" <*>
        v .:  "firstname" <*>
        v .:  "lastname" <*>
        v .:  "phone" <*>
        v .: "gender" <*>
        v .: "address" <*>
        v .: "city"
