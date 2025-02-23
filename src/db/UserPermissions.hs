{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module UserPermissions (findUserPermission, toUserPermissionsDTO, insertUserPermission, deleteUserPermission, updateUserPermission) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Internal as TI
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)

import UserPermissionsDTO

-- Rel8 Schemma Definitions
data UserPermission f = UserPermission
    {permGroupExec :: Column f Int64
    , permGroupRead :: Column f Int64
    , permGroupWrite :: Column f Int64
    , permOtherExec :: Column f Int64
    , permOtherRead :: Column f Int64
    , permOtherWrite :: Column f Int64
    , permResource :: Column f Text
    , permUserExec :: Column f Int64
    , permUserRead :: Column f Int64
    , permUserWrite :: Column f Int64
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (UserPermission f)

userPermissionSchema :: TableSchema (UserPermission Name)
userPermissionSchema = TableSchema
    { name = "permissions"
    , schema = Nothing
    , columns = UserPermission
        { permGroupExec = "group_exec"
        , permGroupRead = "group_read"
        , permGroupWrite = "group_write"
        , permOtherExec = "other_exec"
        , permOtherRead = "other_read"
        , permOtherWrite = "other_write"
        , permResource = "resource"
        , permUserExec = "user_exec"
        , permUserRead = "user_read"
        , permUserWrite = "user_write"
        }
    }

-- Functions
-- GET
findUserPermission :: Text -> Connection -> IO (Either QueryError [UserPermission Result])
findUserPermission resource conn = do
                            let query = select $ do
                                            p <- each userPermissionSchema
                                            where_  (p.permResource ==. lit resource)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertUserPermission :: UserPermissionsDTO -> Connection -> IO (Either QueryError [Text])
insertUserPermission p  conn = do
                            run (statement () (insert1 p)) conn

insert1 :: UserPermissionsDTO -> Statement () [Text]
insert1 p = insert $ Insert
            { into = userPermissionSchema
            , rows = values [ UserPermission (lit p.permGroupExec) (lit p.permGroupRead) (lit p.permGroupWrite) (lit p.permOtherExec) (lit p.permOtherRead) (lit p.permOtherWrite) (lit p.permResource) (lit p.permUserExec) (lit p.permUserRead) (lit p.permUserWrite)]
            , returning = Projection (.permResource)
            , onConflict = Abort
            }

-- DELETE
deleteUserPermission :: Text -> Connection -> IO (Either QueryError [Text])
deleteUserPermission u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: Text -> Statement () [Text]
delete1 r  = delete $ Delete
            { from = userPermissionSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.permResource ==. lit r
            , returning = Projection (.permResource)
            }

-- UPDATE
updateUserPermission :: Text -> UserPermissionsDTO -> Connection -> IO (Either QueryError [Text])
updateUserPermission r p conn = do
                                run (statement () (update1 r p)) conn

update1 :: Text -> UserPermissionsDTO -> Statement () [Text]
update1 r p  = update $ Update
            { target = userPermissionSchema
            , from = pure ()
            , set = \_ row -> UserPermission (lit p.permGroupExec) (lit p.permGroupRead) (lit p.permGroupWrite) (lit p.permOtherExec) (lit p.permOtherRead) (lit p.permOtherWrite) (lit p.permResource) (lit p.permUserExec) (lit p.permUserRead) (lit p.permUserWrite)
            , updateWhere = \t ui -> ui.permResource ==. lit r
            , returning = Projection (.permResource)
            }

-- Helpers
toUserPermissionsDTO :: UserPermission Result -> UserPermissionsDTO
toUserPermissionsDTO p = UserPermissionsDTO p.permGroupExec p.permGroupRead p.permGroupWrite p.permOtherExec p.permOtherRead p.permOtherWrite p.permResource p.permUserExec p.permUserRead p.permUserWrite


