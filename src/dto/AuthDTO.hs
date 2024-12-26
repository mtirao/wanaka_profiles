{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module AuthDTO where

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
newtype PasswordDTO = PasswordDTO { password :: Text} deriving (Show)
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

-- User Resposne
newtype UserResponse = UserResponse Text
    deriving (Show)

instance ToJSON UserResponse where
    toJSON (UserResponse message) = object
        [
            "user_id" .= message
        ]