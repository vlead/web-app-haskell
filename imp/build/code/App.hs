{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions

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

toUserDatatype :: UserData -> User
toUserDatatype (UserData userDataName userDataEmail) = User {userName=userDataName,  userEmail=userDataEmail, userRoles=NonAdmin}

toTextDatatype :: UniqueUserData -> Text
toTextDatatype (UniqueUserData userData) = pack(userData)


-- helper function for showUsersHandler
--showAllUsers :: ConnectionPool -> IO ([Maybe User])
showAllUsersHelper pool = flip runSqlPersistMPool pool $ do
  users <- selectList [] []
  return $ Prelude.map entityVal users

-- here we assume that there are non-zero users in the database


-- helper function for addUserHandler
--addUserHelper :: ConnectionPool -> User -> IO (Maybe (Key (User)))
addUserHelper pool newUser = flip runSqlPersistMPool pool $ do
  exists <- selectFirst [UserName ==. (userName newUser)] []
  case exists of
    Nothing -> Just <$> insert newUser
    Just _  -> return Nothing


-- helper function for deleteUserHandler
deleteUserHelper :: ConnectionPool -> Text -> IO ((Maybe (User)))
deleteUserHelper pool userToDel = flip runSqlPersistMPool pool $ do
  deletedUser <- selectFirst [UserEmail ==. unpack(userToDel)] []
  case deletedUser of
    Nothing -> return Nothing
    Just _ -> do 
                 userIfDeleted <- deleteWhere [UserEmail ==. unpack(userToDel)]
                 return $ entityVal <$> deletedUser 
  
server :: ConnectionPool -> Server UserAPI
server pool =
  showUsersHandler
  :<|> addUserHandler
  :<|> deleteUserHandler
  where
    showUsersHandler = liftIO $ showAllUsersHelper pool 
    addUserHandler newUser = liftIO $ addUserHelper pool $ toUserDatatype newUser
    deleteUserHandler userToDel = liftIO $ deleteUserHelper pool $ toTextDatatype userToDel


-- function that takes the server function and returns a WAI application 
app :: ConnectionPool -> Application
app pool = serve userAPI $ server pool
  where
    userAPI :: Proxy UserAPI
    userAPI = Proxy


-- to integrate Persist backend with API
-- createSqlitePool creates a pool of database connections
mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool


-- to run the SQL database
run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 8000 =<< mkApp sqliteFile
