{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


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
