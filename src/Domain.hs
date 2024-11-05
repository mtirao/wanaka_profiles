{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}


module Domain where

import Data.Text.Lazy
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

-- Authentication
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


--ErrorMessage
data ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]


-- Profile
data Profile = Profile
    {cellPhone :: Text
    , email :: Text
    , firstName :: Text
    , lastName :: Text
    , phone :: Text
    , userName :: Maybe Text
    , userPassword :: Maybe Text
    , userRole :: Maybe Text
    , profileId :: Maybe Integer
    , gender :: Text
    , address :: Text
    , city :: Text
    , userId :: Text
    } deriving (Show)

instance ToJSON Profile where
    toJSON Profile {..} = object [
            "cellphone" .= cellPhone,
            "email" .= email,
            "firstname" .= firstName,
            "lastname" .= lastName,
            "phone" .= phone,
            "username" .= userName,
            "userrole" .= userRole,
            "profileid" .= profileId,
            "gender" .= gender,
            "address" .= address,
            "city" .= city,
            "userid" .= userId
        ]

instance FromJSON Profile where
    parseJSON (Object v) = Profile <$>
        v .:  "cellphone" <*>
        v .:  "email" <*>
        v .:  "firstname" <*>
        v .:  "lastname" <*>
        v .:  "phone" <*>
        v .:?  "username" <*>
        v .:?  "userpassword" <*>
        v .:?  "userrole" <*>
        v .:?  "profileid" <*>
        v .: "gender" <*>
        v .: "address" <*>
        v .: "city" <*>
        v .: "userid"



-- Getters
getUserName :: Maybe Login -> Text
getUserName a = case a of
                Nothing -> ""
                Just (Login u p) -> u

getPassword :: Maybe Login -> Text
getPassword a = case a of
                Nothing -> ""
                Just (Login u p) -> p

extractPassword :: Maybe Text -> Text
extractPassword = Data.Maybe.fromMaybe ""


nanosSinceEpoch :: NominalDiffTime -> Int64
nanosSinceEpoch = floor  . nominalDiffTimeToSeconds

secondsSinceEpoch :: NominalDiffTime -> Int64
secondsSinceEpoch u = (nanosSinceEpoch u) 

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = (secondsSinceEpoch u) + 864000

convertToString :: Text -> Int64 -> [Char]
convertToString u t = BI.unpackChars (encode $ Payload u t)