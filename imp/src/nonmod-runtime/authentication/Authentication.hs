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


toInt :: String -> Int
toInt str = read str


toSessionId :: String -> SessionId
toSessionId val = toSqlKey $ fromIntegral $ toInt val


headerCheck :: Maybe String -> String
headerCheck authVal = case authVal of
    Nothing -> "0"
    Just value -> value

      
loginCheck :: ConnectionPool -> String -> IO Bool
loginCheck pool authSessionId = flip runSqlPersistMPool pool $ do
  isLoggedIn <- get $ toSessionId authSessionId
  case isLoggedIn of
    Nothing -> return False
    Just _ -> return True


adminAuthCheck :: ConnectionPool -> String -> IO Bool
adminAuthCheck pool authSessionId = flip runSqlPersistMPool pool $ do
  roleOfLoggedInUser <- get $ toSessionId authSessionId
  case roleOfLoggedInUser of
    Nothing -> return False
    Just roleValue -> case (sessionUserRoles roleValue) of  
                        [NonAdmin] -> return False
                        [Admin]    -> return True

isSelfCheck :: ConnectionPool -> String -> String -> IO Bool
isSelfCheck pool userData authSessionId = flip runSqlPersistMPool pool $ do
  roleOfLoggedInUser <- get $ toSessionId authSessionId
  case roleOfLoggedInUser of
    Nothing -> return False
    Just roleValue -> if (((sessionUserEmail roleValue) == userData))
                        then return True
                        else return False


isEitherAdminOrSelfCheck :: ConnectionPool -> String -> String -> IO Bool
isEitherAdminOrSelfCheck pool userData authSessionId = flip runSqlPersistMPool pool $ do
  roleOfUser <- get $ toSessionId authSessionId
  case roleOfUser of
    Nothing -> return False
    Just xs -> case (sessionUserRoles xs) of
      [Admin] -> return True
      [NonAdmin] -> if ((sessionUserEmail xs) == userData) 
        then return True
        else do
          userEntity <- selectFirst [UserEmail ==. (sessionUserEmail xs)] []
          case (entityVal <$> userEntity) of
            Nothing -> return False
            Just x -> if ((userName x) == userData)
                      then return True
                      else return False
  

isNotAdminSelfCheck :: ConnectionPool -> Text -> String -> IO Bool
isNotAdminSelfCheck pool userData authSessionId = flip runSqlPersistMPool pool $ do
  roleOfLoggedInUser <- get $ toSessionId authSessionId
  case roleOfLoggedInUser of
    Nothing -> return False
    Just roleValue -> case (sessionUserRoles roleValue) of
                        [NonAdmin] -> return False
                        [Admin] -> if ((sessionUserEmail roleValue) == unpack(userData))
                                   then return False
                                   else return True


hasRole :: ConnectionPool -> Session -> IO (Bool)
hasRole pool session = flip runSqlPersistMPool pool $ do
  user <- selectFirst [UserEmail ==. (sessionUserEmail session)] []
  case user of
    Nothing -> return False
    Just xs -> let
                 roles = userRoles $ entityVal xs
                 sessionRole = Prelude.head (sessionUserRoles session)
               in
                 if (sessionRole `elem` roles)
                 then return True
                 else return False
