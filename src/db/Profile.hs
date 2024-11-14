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

module Profile (findProfile, toProfileDTO, insertProfile, deleteProfile) where

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

-- Functions
-- GET
findProfile userId conn = do 
                            let query = select $ do
                                            p <- each profileSchema
                                            where_ $ (p.userId ==. lit userId)
                                            return p 
                            run (statement () query ) conn

-- INSERT
insertProfile :: ProfileDTO -> Connection -> IO (Either QueryError [Text])
insertProfile p  conn = do
                            run (statement () (insert1 p)) conn

insert1 :: ProfileDTO -> Statement () [Text]
insert1 p = insert $ Insert 
            { into = profileSchema
            , rows = values [ Profile (lit $ getCellPhone p) (lit $ getEmail p) (lit $ getFirstName p) (lit $ getLastName p) (lit $ getPhone p) (lit $ getGender p) (lit $ getAddress p) (lit $ getCity p) ""]
            , returning = Projection (.userId)
            , onConflict = Abort
            }

-- DELETE
deleteProfile :: Text -> Connection -> IO (Either QueryError [Text])
deleteProfile u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = profileSchema
            , using = pure ()
            , deleteWhere = \t ui -> (ui.userId ==. lit u)
            , returning = Projection (.userId)
            }

-- Helpers
toProfileDTO :: Profile Result -> ProfileDTO
toProfileDTO p = ProfileDTO (convertText $ p.cellPhone) (convertText $ p.email) (convertText $ p.firstName) (convertText $ p.lastName) (convertText $ p.phone) (convertText $ p.gender) (convertText $ p.address)  (convertText $ p.city)

convertText :: Text -> TL.Text
convertText s = TL.fromStrict s

