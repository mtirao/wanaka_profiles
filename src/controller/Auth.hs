{-# LANGUAGE OverloadedStrings #-}
module Auth where

import ProfileDTO

import Views ( jsonResponse )
import Tenant 

import Web.Scotty ( body, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)

import Control.Monad.IO.Class

import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL 
import qualified Data.Text as T
import qualified Data.ByteString.Internal as BI
import Data.Time
import Data.Time.Clock.POSIX

import GHC.Int

import Network.HTTP.Types.Status

import Data.Aeson

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt))

--- AUTH
userAuthenticate body conn =  do
        b <- body
        curTime <- liftIO getPOSIXTime
        let login = (decode b :: Maybe Login)
        let expDate = tokenExpiration curTime
        result <- liftIO $ findTenant (getUserName login) (getPassword login) conn
        case result of
                Right [] -> do
                        jsonResponse (ErrorMessage "User not found")
                        status forbidden403
                Right [a] ->
                        jsonResponse (LoginResponse (createToken (TL.pack (T.unpack a)) expDate) "JWT" "refreshToken" )


createToken u  t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> TL.pack $ BI.unpackChars jwt
                    where 
                        payload = (BI.packChars $ convertToString u t)
                        token = hmacEncode HS256 "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" payload

-- Helpers
nanosSinceEpoch :: NominalDiffTime -> Int64
nanosSinceEpoch = floor  . nominalDiffTimeToSeconds

secondsSinceEpoch :: NominalDiffTime -> Int64
secondsSinceEpoch u = (nanosSinceEpoch u) 

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = (secondsSinceEpoch u) + 864000

convertToString :: Text -> Int64 -> [Char]
convertToString u t = BL.unpackChars (encode $ Payload u t)                 