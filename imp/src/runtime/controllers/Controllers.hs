{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Controllers where

import           Data.Aeson
import           Data.Text
import           Data.List 
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
showAllUsersHelper :: ConnectionPool -> Bool -> IO ([ShowUserData])
showAllUsersHelper pool authVal = flip runSqlPersistMPool pool $ case authVal of
    False -> return []
    True  -> do
      users <- selectList [] []
      return $ Prelude.map toShowUserData $ Prelude.map entityVal users


-- helper function for addUserHandler
addUserHelper :: User -> ConnectionPool -> Bool -> IO (Maybe (ResponseUserId))
addUserHelper newUser pool authVal = flip runSqlPersistMPool pool $ case authVal of
  False -> return Nothing
  True  -> do
    exists <- selectFirst [UserEmail ==. (userEmail newUser)] []
    case exists of
      Nothing -> Just <$> (ResponseUserId <$> Database.Persist.Sqlite.insert newUser)
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
  ifLoggedIn <- selectFirst [SessionUserEmail ==. (sessionUserEmail newSession), SessionUserRoles ==. (sessionUserRoles newSession)] []
  case ifLoggedIn of
    Just _ -> return Nothing
    Nothing -> do 
      ifExists <- selectFirst [UserEmail ==. (sessionUserEmail newSession)] []
      case ifExists of
        Nothing -> return Nothing
        Just _  -> Just <$> (ResponseSessionId <$> Database.Persist.Sqlite.insert newSession)
  

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
      
  

setNameHelper :: ConnectionPool -> UpdateUserData -> IO (Maybe (User))
setNameHelper pool userData = flip runSqlPersistMPool pool $ do
  updateWhere [UserName ==. (currentData userData)] [UserName =. (newData userData)]
  ret <- selectFirst [UserName ==. (newData userData)] []
  return $ entityVal <$> ret 
  


setEmailHelper :: ConnectionPool -> UpdateUserData -> IO (Maybe (User))
setEmailHelper pool userData = flip runSqlPersistMPool pool $ do
  updateWhere [UserEmail ==. (currentData userData)] [UserEmail =. (newData userData)]
  ret <- selectFirst [UserEmail ==. (newData userData)] []
  return $ entityVal <$> ret 
  


showUserDetailsHelper :: ConnectionPool -> String -> IO (Maybe (User))
showUserDetailsHelper pool userData = flip runSqlPersistMPool pool $ do
  user <- selectFirst [UserEmail ==. userData] []
  return $ entityVal <$> user


showSessionsHelper :: ConnectionPool -> IO ([Session])
showSessionsHelper pool = flip runSqlPersistMPool pool $ do
  sessions <- selectList [] []
  return $ Prelude.map entityVal sessions

showRolesHelper :: ConnectionPool -> String -> IO ([Role])
showRolesHelper pool userData = flip runSqlPersistMPool pool $ do
  user <- selectFirst [UserEmail ==. userData] []
  case user of
    Nothing -> return []
    Just xs -> return (userRoles $ entityVal xs)
  


addRoleHelper :: ConnectionPool -> String -> Role -> IO (Maybe (User))
addRoleHelper pool userData newRole = flip runSqlPersistMPool pool $ do
  user <- selectFirst [UserEmail ==. userData] []
  case user of
    Nothing -> return Nothing
    Just xs -> let
      roles = userRoles (entityVal xs)
      in 
        if ((newRole `elem` roles) || ((Data.List.length roles) == 2))
        then return Nothing
        else 
          let
            newRoles = roles ++ [newRole]
          in
            do
              updateWhere [UserEmail ==. userData] [UserRoles =. newRoles]
              updatedUser <- selectFirst [UserEmail ==. userData] []
              return $ entityVal <$> updatedUser





-- function to delete role: takes advantage of the fact that
-- there are only two roles
deleteRole :: Role -> [Role] -> [Role]
deleteRole role ls = if ((Data.List.head ls) == role)
  then (Data.List.tail ls)
  else (Data.List.init ls)

  
deleteRoleHelper :: ConnectionPool -> String -> Role -> IO (Maybe (User))
deleteRoleHelper pool userData newRole = flip runSqlPersistMPool pool $ do
  user <- selectFirst [UserEmail ==. userData] []
  case user of
    Nothing -> return Nothing
    Just xs -> let
      roles = userRoles (entityVal xs)
      in 
        if ((newRole `elem` roles) && (Data.List.length roles > 1))
        then let
                 newRoles = deleteRole newRole roles
               in
                 do
                   updateWhere [UserEmail ==. userData] [UserRoles =. newRoles]
                   updatedUser <- selectFirst [UserEmail ==. userData] []
                   return $ entityVal <$> updatedUser
        else return Nothing
