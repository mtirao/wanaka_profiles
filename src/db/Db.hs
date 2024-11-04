{-# LANGUAGE DeriveGeneric #-}

module Db where

import Domain

import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import GHC.Int
import Web.Scotty (rescue)
import qualified Data.Maybe


-- DbConfig contains info needed to connect to MySQL server
data DbConfig = DbConfig {
        dbName :: String,
        dbUser :: String,
        dbPassword :: String,
        dbHost :: String
    } deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       , connectHost = dbHost conf
                       }

--------------------------------------------------------------------------------
-- Utilities for interacting with the 
-- No transactions.
--
-- Accepts arguments
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

-- No arguments -- just pure sql
fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql

-- Update database
execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
       where ins conn = execute conn sql args

-------------------------------------------------------------------------------
-- Utilities for interacting with the 
-- Transactions.
--
-- Accepts arguments
fetchT :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
      where retrieve conn = withTransaction conn $ query conn sql args

-- No arguments -- just pure sql
fetchSimpleT :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
       where retrieve conn = withTransaction conn $ query_ conn sql

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------

findUserByLogin :: Pool Connection -> String -> IO (Maybe Profile)
findUserByLogin pool login = do
          res <- liftIO $ fetch pool (Only login) "SELECT cell_phone, email, first_name, last_name, phone, user_name, user_password, user_role, id, gender, address, city, user_id FROM profiles WHERE user_name=?" :: IO [(TL.Text, TL.Text,TL.Text,TL.Text,TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe Integer, TL.Text, TL.Text, TL.Text, TL.Text)]
          return $ oneProfile res
            where oneProfile ((cellPhone, email, firstName, lastName, phone, userName, userPassword, userRole, id, gender, address, city, userId) : _) = Just $ Profile cellPhone email firstName lastName phone userName userPassword userRole id gender address city userId
                  oneProfile _ = Nothing


passwordForUser :: Pool Connection -> TL.Text-> IO ( Maybe TL.Text )
passwordForUser pool login = do
       res <- liftIO $ fetch pool (Only login) "SELECT cell_phone, email, first_name, last_name, phone, user_name, user_password, user_role, id, gender, address, city FROM profiles WHERE user_name=?" :: IO [(TL.Text, TL.Text,TL.Text,TL.Text,TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe Integer, TL.Text, TL.Text, TL.Text)]       
       return $ oneProfile res
       where oneProfile ((cellPhone, email, firstName, lastName, phone, userName, userPassword, userRole, id, gender, address, city) : _) = userPassword
             oneProfile _ = Nothing



validatePassword :: Pool Connection -> BL.ByteString -> BL.ByteString -> IO Bool
validatePassword pool login password = do 
       res <- passwordForUser pool (TL.decodeUtf8 login)
       let pass =  Data.Maybe.fromMaybe "" res 
       return $ pass == TL.decodeUtf8 password

--------------------------------------------------------------------------------

class DbOperation a where 
    insert :: Pool Connection -> Maybe a -> IO (Maybe a)  --Pool Connection -> Maybe a -> ActionT TL.Text IO ()
    update :: Pool Connection -> Maybe a -> TL.Text -> IO (Maybe a)
    find :: Pool Connection -> TL.Text -> IO (Maybe a)
    list :: Pool Connection -> IO [a]