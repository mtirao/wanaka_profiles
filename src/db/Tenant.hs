{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Tenant (findTenant, findTenantByToken, insertTenant, deleteTenant, updatePassword, updateTokens) where

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
    , authToken :: Column f Text
    , createdAt :: Column f Int64
    , refreshToken :: Column f Text
    , status :: Column f Text
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
        , authToken = "auth_token"
        , createdAt = "created_at"
        , refreshToken = "refresh_token"
        , status = "status"
        }
    }

findTenant :: Text -> Text -> Connection -> IO (Either QueryError [Text])
findTenant userName password conn =  do 
                            let query = select $ do
                                            p <- each tenantSchema
                                            where_ $ (p.userName ==. lit userName) &&. (p.userPassword ==. lit password)
                                            return p.userId
                            run (statement () query ) conn

findTenantByToken :: Text -> Connection -> IO (Either QueryError [Text])
findTenantByToken token conn =  do 
                            let query = select $ do
                                            p <- each tenantSchema
                                            where_ $ p.authToken ==. lit token
                                            return p.userId
                            run (statement () query ) conn                      

-- INSERT
insertTenant :: Text -> Text -> Text -> Int64-> Connection -> IO (Either QueryError [Text])
insertTenant u p i c conn = do
                            run (statement () (insert1 u p i c)) conn

insert1 :: Text -> Text -> Text -> Int64 -> Statement () [Text]
insert1 u p i c = insert $ Insert 
            { into = tenantSchema
            , rows = values [ Tenant (lit u) (lit p) "admin" (lit i) "" (lit c) "" "new" ]
            , returning = Projection (.userId)
            , onConflict = Abort
            }

-- DELETE
deleteTenant :: Text -> Connection -> IO (Either QueryError [Text])
deleteTenant u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = tenantSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- UPDATE
updatePassword :: Text -> Text -> Connection -> IO (Either QueryError [Text])
updatePassword u p conn = do
                        run (statement () (update1 u p)) conn

updateTokens :: Text -> Text -> Text -> Connection -> IO (Either QueryError [Text])
updateTokens u t rt conn = do
                        run (statement () (update2 u t rt)) conn

-- Update password
update1 :: Text -> Text -> Statement () [Text]
update1 u p  = update $ Update
            { target = tenantSchema
            , from = pure ()
            , set = \_ row -> Tenant row.userName (lit p) "admin" row.userId row.authToken row.createdAt row.refreshToken row.status
            , updateWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- Update tokens
update2 :: Text -> Text -> Text -> Statement () [Text]
update2 u t rt  = update $ Update
            { target = tenantSchema
            , from = pure ()
            , set = \_ row -> Tenant row.userName row.userPassword "admin" row.userId (lit t) row.createdAt (lit rt) row.status
            , updateWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }
