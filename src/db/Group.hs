{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Group (findGroup, toGroupDTO, insertGroup, deleteGroup, updateGroup, getGroupId) where

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

import GroupsDTO

-- Rel8 Schemma Definitions
data Group f = Group
    {userId  :: Column f Text
    , groupId :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Group f)

groupSchema :: TableSchema (Group Name)
groupSchema = TableSchema
    { name = "groups"
    , schema = Nothing
    , columns = Group
        { userId = "user_id"
        , groupId = "group_id"
        }
    }

-- Functions
-- GET
findGroup :: Text -> Connection -> IO (Either QueryError [Group Result])
findGroup userId conn = do
                            let query = select $ do
                                            p <- each groupSchema
                                            where_  (p.userId ==. lit userId)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertGroup :: GroupsDTO -> Connection -> IO (Either QueryError [Text])
insertGroup p  conn = do
                            run (statement () (insert1 p)) conn

insert1 :: GroupsDTO -> Statement () [Text]
insert1 p = insert $ Insert
            { into = groupSchema
            , rows = values [ Group (lit p.groupUserId) (lit p.groupId)]
            , returning = Projection (.userId)
            , onConflict = Abort
            }

-- DELETE
deleteGroup :: Text -> Connection -> IO (Either QueryError [Text])
deleteGroup u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = groupSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- UPDATE
updateGroup :: Text -> GroupsDTO -> Connection -> IO (Either QueryError [Text])
updateGroup u p conn = do
                        run (statement () (update1 u p)) conn

update1 :: Text -> GroupsDTO -> Statement () [Text]
update1 u p  = update $ Update
            { target = groupSchema
            , from = pure ()
            , set = \_ row -> Group (lit p.groupUserId) (lit p.groupId)
            , updateWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- Helpers
toGroupDTO :: Group Result -> GroupsDTO
toGroupDTO p = GroupsDTO p.userId p.groupId

getGroupId :: Group Result -> Text
getGroupId p = p.groupId
