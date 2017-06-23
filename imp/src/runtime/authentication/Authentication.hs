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
  

-- | utility function to convert a string to an int
toInt :: String -> Int
toInt str = read str


-- | utility function to convert String value to SessionId
toSessionId :: String -> SessionId
toSessionId val = toSqlKey $ fromIntegral $ toInt val


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
  isLoggedIn <- get $ toSessionId authSessionId
  case isLoggedIn of
    Nothing -> return False
    Just _ -> return True

    
-- | To return False if user is NonAdmin user
-- | And True if user is Admin user
adminAuthCheck :: ConnectionPool -> String -> IO Bool
adminAuthCheck pool authSessionId = flip runSqlPersistMPool pool $ do
  roleOfLoggedInUser <- get $ toSessionId authSessionId
  case roleOfLoggedInUser of
    Nothing -> return False
    Just roleValue -> case (sessionUserRoles roleValue) of 
                        NonAdmin -> return False
                        Admin    -> return True


-- | to check if user passed to the function is logged-in self
isSelfCheck :: ConnectionPool -> String -> String -> IO Bool
isSelfCheck pool userData authSessionId = flip runSqlPersistMPool pool $ do
  roleOfLoggedInUser <- get $ toSessionId authSessionId
  case roleOfLoggedInUser of
    Nothing -> return False
    Just roleValue -> if ((sessionUserEmail roleValue) == userData)
                        then return True
                        else return False


-- | to check if user passed to the function is logged-in admin and not self
isNotAdminSelfCheck :: ConnectionPool -> Text -> String -> IO Bool
isNotAdminSelfCheck pool userData authSessionId = flip runSqlPersistMPool pool $ do
  roleOfLoggedInUser <- get $ toSessionId authSessionId
  case roleOfLoggedInUser of
    Nothing -> return False
    Just roleValue -> case (sessionUserRoles roleValue) of
                        NonAdmin -> return False
                        Admin -> if ((sessionUserEmail roleValue) == unpack(userData))
                                   then return False
                                   else return True
