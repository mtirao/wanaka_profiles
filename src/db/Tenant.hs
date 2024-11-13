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

module Tenant (findTenant, getConnection) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import Hardcoded

-- Rel8 Schemma Definitions
data Tenant f = Tenant
    { userName :: Column f Text
    , userPassword :: Column f Text
    , userRole :: Column f Text
    , userId :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Tenant f)

tenantSchema :: TableSchema (Tenant Name)
tenantSchema = TableSchema
    { name = "tenants"
    , schema = Nothing
    , columns = Tenant
        { userName = "user_name"
        , userPassword = "user_password"
        , userRole = "user_role"
        , userId = "user_id"
        }
    }

getConnection :: IO (Either ConnectionError Connection)
getConnection = acquire $ settings Hardcoded.host  Hardcoded.portNumber Hardcoded.user Hardcoded.password Hardcoded.database

findTenant :: Text -> Text -> Connection -> IO (Either QueryError [Text])
findTenant userName password conn =  do 
                            let query = select $ do
                                            p <- each tenantSchema
                                            where_ $ (p.userName ==. lit userName) &&. (p.userPassword ==. lit password)
                                            return p.userId
                            run (statement () query ) conn