{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Profile (findProfile, toProfileDTO) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack)
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.Text.Internal as TI
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Hardcoded

import ProfileDTO

-- Rel8 Schemma Definitions
data Profile f = Profile 
    {cellPhone :: Column f Text
    , email :: Column f Text
    , firstName :: Column f Text
    , lastName :: Column f Text
    , phone :: Column f Text
    , gender :: Column f Text
    , address :: Column f Text
    , city :: Column f Text
    , userId :: Column f Text
    } 
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Profile f)

profileSchema :: TableSchema (Profile Name)
profileSchema = TableSchema
    { name = "profiles"
    , schema = Nothing
    , columns = Profile
        { cellPhone = "cell_phone"
        , email = "email"
        , firstName= "first_name"
        , lastName = "last_name"
        , phone = "phone"
        , gender = "gender"
        , address = "address"
        , city = "city"
        , userId = "user_id"
        }
    }

findProfile userId conn = do 
                            let query = select $ do
                                            p <- each profileSchema
                                            where_ $ (p.userId ==. lit userId)
                                            return p 
                            run (statement () query ) conn

toProfileDTO :: Profile Result -> ProfileDTO
toProfileDTO p = ProfileDTO p.cellPhone p.email p.firstName p.lastName p.phone p.gender p.address p.city