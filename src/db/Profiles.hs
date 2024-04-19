{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Profiles where


import Db
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
import GHC.Int
import Data.Time.LocalTime


instance DbOperation Profile where
    insert pool (Just a) = do
        res <- fetch pool (cellPhone a, email a, firstName a, lastName a, phone a, userName a, userPassword a, userRole a, gender a, address a, city a) 
                            "INSERT INTO profiles(cell_phone, email, first_name, last_name, phone, user_name, user_password, user_role, gender, address, city) VALUES(?,?,?,?,?,?,?,?,?,?,?) RETURNING  cell_phone, email, first_name, last_name, phone, user_name, user_password, user_role, id, gender, address, city" :: IO [(TL.Text, TL.Text,TL.Text,TL.Text,TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe Integer, TL.Text, TL.Text, TL.Text)]
        return $ oneAgent res
            where oneAgent ((cellPhone, email, firstName, lastName, phone, userName, userPassword, userRole, id, gender, address, city) : _) = Just $ Profile cellPhone email firstName lastName phone userName userPassword userRole id gender address city
                  oneAgent _ = Nothing
    
    update pool (Just a) id= do
        res <- fetch pool (cellPhone a, email a, firstName a, lastName a, phone a, gender a, address a, city a, id) 
                            "UPDATE profiles SET cell_phone=?, email=?, first_name=?, last_name=?, phone=?, gender=?, address=?, city=? WHERE id=? RETURNING  cell_phone, email, first_name, last_name, phone, user_name, user_password, user_role, id, gender, address, city" :: IO [(TL.Text, TL.Text,TL.Text,TL.Text,TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe Integer, TL.Text, TL.Text, TL.Text)]
        return $ oneAgent res
            where oneAgent ((cellPhone, email, firstName, lastName, phone, userName, userPassword, userRole, id, gender, address, city) : _) = Just $ Profile cellPhone email firstName lastName phone userName userPassword userRole id gender address city
                  oneAgent _ = Nothing

    find  pool id = do 
                        res <- fetch pool (Only id) "SELECT cell_phone, email, first_name, last_name, phone, user_name, user_password, user_role, id, gender, address, city FROM profiles WHERE id=?" :: IO [(TL.Text, TL.Text,TL.Text,TL.Text,TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe Integer, TL.Text, TL.Text, TL.Text)]
                        return $ oneAgent res
                           where oneAgent ((cellPhone, email, firstName, lastName, phone, userName, userPassword, userRole, id, gender, address, city) : _) = Just $ Profile cellPhone email firstName lastName phone userName userPassword userRole id gender address city
                                 oneAgent _ = Nothing
    
    list  pool = do
                    res <- fetchSimple pool "SELECT cell_phone, email, first_name, last_name, phone, user_name, user_password, user_role, id, gender, address, city FROM profiles" :: IO [(TL.Text, TL.Text,TL.Text,TL.Text,TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe TL.Text, Maybe Integer, TL.Text, TL.Text, TL.Text)]
                    return $ map (\(cellPhone, email, firstName, lastName, phone, userName, userPassword, userRole, id, gender, address, city) -> Profile cellPhone email firstName lastName phone userName userPassword userRole id gender address city) res

