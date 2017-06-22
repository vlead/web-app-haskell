{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Controllers where

import           Data.Aeson
import           Data.Text
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
import           Authentication 


-- helper function for showUsersHandler
showAllUsersHelper :: ConnectionPool -> Bool -> IO ([User])
showAllUsersHelper pool authVal = flip runSqlPersistMPool pool $ case authVal of
    False -> return []
    True  -> do
      users <- selectList [] []
      return $ Prelude.map entityVal users


-- helper function for addUserHandler
addUserHelper :: User -> ConnectionPool -> Bool -> IO (Maybe (ResponseUserId))
addUserHelper newUser pool authVal = flip runSqlPersistMPool pool $ case authVal of
  False -> return Nothing
  True  -> do
    exists <- selectFirst [UserName ==. (userName newUser)] []
    case exists of
      Nothing -> Just <$> (ResponseUserId <$> insert newUser)
      Just _  -> return Nothing


-- helper function for deleteUserHandler
deleteUserHelper :: Text -> ConnectionPool -> Bool -> IO ((Maybe (User)))
deleteUserHelper userToDel pool authVal = flip runSqlPersistMPool pool $ case authVal of
  False -> return Nothing
  True  -> do
    deletedUser <- selectFirst [UserEmail ==. unpack(userToDel)] []
    case deletedUser of
      Nothing -> return Nothing
      Just _ -> do
        userIfDeleted <- deleteWhere [UserEmail ==. unpack(userToDel)]
        return $ entityVal <$> deletedUser


-- helper function for loginHandler
loginHelper :: Session -> ConnectionPool -> IO (Maybe (ResponseSessionId))
loginHelper newSession pool = flip runSqlPersistMPool pool $ do
  ifExists <- selectFirst [UserEmail ==. (sessionUserEmail newSession)] []
  case ifExists of
    Nothing -> return Nothing
    Just _  -> Just <$> (ResponseSessionId <$> insert newSession)
  

-- helper function for logoutHandler
logoutHelper :: Session -> ConnectionPool -> Bool -> IO (Maybe (Session))
logoutHelper currentSession pool authVal = flip runSqlPersistMPool pool $ case authVal of
  False -> return Nothing
  True  -> do
    ifExists <- selectFirst [SessionUserEmail ==. (sessionUserEmail currentSession), SessionUserRoles ==. (sessionUserRoles currentSession)] []
    case ifExists of
      Nothing -> return Nothing
      Just _ -> do
        deleteWhere [SessionUserEmail ==. (sessionUserEmail currentSession)]
        return $ entityVal <$> ifExists
