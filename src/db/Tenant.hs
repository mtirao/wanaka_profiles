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
{-# language DisambiguateRecordFields #-}

module Tenant (findTenant, insertTenant) where

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

findTenant :: Text -> Text -> Connection -> IO (Either QueryError [Text])
findTenant userName password conn =  do 
                            let query = select $ do
                                            p <- each tenantSchema
                                            where_ $ (p.userName ==. lit userName) &&. (p.userPassword ==. lit password)
                                            return p.userId
                            run (statement () query ) conn


insertTenant :: Text -> Text -> Connection -> IO (Either QueryError [Text])
insertTenant u p conn = do
                                run (statement () (insert1 u p)) conn

insert1 :: Text -> Text -> Statement () [Text]
insert1 u p = insert $ Insert 
            { into = tenantSchema
            , rows = values [ Tenant (lit u) (lit p) "admin" (lit u) ]
            , returning = Projection (.userId)
            , onConflict = Abort
            }
