{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Authentication where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions
import qualified Data.ByteString.Lazy.Char8 as B           

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant

import           Data.Text

import           Api
import           Models
import           Role

-- All authentication-related values are prefixed with auth-


-- | To return sessionId value if header exists
-- | and return 0 if header does not exist
headerCheck :: Maybe String -> String
headerCheck authVal = case authVal of
    Nothing -> "0"
    Just value -> value
    

-- | To return True if user is logged in
-- | and False if user is not logged in
loginCheck :: ConnectionPool -> String -> IO Bool
loginCheck pool authSessionId = flip runSqlPersistMPool pool $ do
  isLoggedIn <- get $ SessionKey $ read authSessionId
  case isLoggedIn of
    Nothing -> return False
    Just _ -> return True

    
-- | To return False if user is NonAdmin user
-- | And True if user is Admin user
adminAuthCheck :: ConnectionPool -> String -> IO Bool
adminAuthCheck pool authSessionId = flip runSqlPersistMPool pool $ do
  roleOfLoggedInUser <- get $ SessionKey $ read authSessionId
  case roleOfLoggedInUser of
    Nothing -> return False
    Just roleValue -> case (sessionUserRoles roleValue) of 
                        NonAdmin -> return False
                        Admin    -> return True
