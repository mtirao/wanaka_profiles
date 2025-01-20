{-# LANGUAGE OverloadedStrings #-}

module Evaluator where

import Data.Aeson

import qualified Data.Text
import Data.Text.Internal.Lazy
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BL

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json)
import Network.HTTP.Types.Status
import ProfileDTO
import Views ( jsonResponse )
import AuthDTO
import ErrorMessage

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt), JwsHeader(JwsHeader))

-- Convert apply a func to Maybe a and create an ActionT result
liftMaybe :: Maybe t -> (t -> ActionT IO ()) -> ActionT IO ()
liftMaybe Nothing func = do
                                jsonResponse (ErrorMessage "Invalid token payload")
                                status badRequest400
liftMaybe (Just value) func = func value


-- Decode access token from header

convertToPayload :: BI.ByteString -> Maybe Payload
convertToPayload t = ( decode $  BL.packChars $ BI.unpackChars t ) :: Maybe Payload

decodeAuthHdr auth = decodeToken auth :: Maybe Payload

decodeToken :: Text -> Maybe Payload
decodeToken t = case token of
                    Left _ -> Nothing
                    Right (_, jwt) -> convertToPayload jwt
                where
                    token = hmacDecode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" $ tokenFromHeader t
                    tokenFromHeader t = BL.toStrict $ TL.encodeUtf8 t
