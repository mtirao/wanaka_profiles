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
data PasswordDTO = PasswordDTO { password :: Text} deriving (Show)
instance FromJSON PasswordDTO where 
    parseJSON (Object v) = PasswordDTO <$>
        v .: "password"

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

instance FromJSON Payload where
    parseJSON (Object v) = Payload <$>
        v .: "user" <*>
        v .: "exp"

tokenExperitionTime :: Payload -> Int64
tokenExperitionTime (Payload u e) = e

tokenUserId :: Payload -> Text
tokenUserId (Payload u e) = u


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

getTenantId :: TenantDTO -> T.Text
getTenantId a = T.pack (TL.unpack (userId a))

-- Profile
data ProfileDTO = ProfileDTO
    { cellPhone :: Text
    , email :: Text
    , firstName :: Text
    , lastName :: Text
    , phone :: Text
    , gender :: Text
    , address :: Text
    , city :: Text
    } deriving (Show)

-- Getters
getCellPhone :: ProfileDTO -> T.Text
getCellPhone a = T.pack (TL.unpack (cellPhone a))

getEmail :: ProfileDTO -> T.Text
getEmail a = T.pack (TL.unpack (email a))

getFirstName :: ProfileDTO -> T.Text
getFirstName a = T.pack (TL.unpack (firstName a))

getLastName :: ProfileDTO -> T.Text
getLastName a = T.pack (TL.unpack (lastName a))

getPhone :: ProfileDTO -> T.Text
getPhone a = T.pack (TL.unpack (phone a))

getGender :: ProfileDTO -> T.Text
getGender a = T.pack (TL.unpack (gender a))

getAddress :: ProfileDTO -> T.Text
getAddress a = T.pack (TL.unpack (address a))

getCity :: ProfileDTO -> T.Text
getCity a = T.pack (TL.unpack (city a))

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
