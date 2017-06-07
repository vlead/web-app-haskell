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


-- helper function for showUsersHandler
--showAllUsers :: ConnectionPool -> IO ([Maybe User])
showAllUsersHelper pool = flip runSqlPersistMPool pool $ do
  users <- selectList [] []
  return $ Prelude.map entityVal users

-- here we assume that there are non-zero users in the database


addUserHelper :: ConnectionPool -> User -> IO (Maybe (Key (User)))
addUserHelper pool newUser = flip runSqlPersistMPool pool $ do
  exists <- selectFirst [UserName ==. (userName newUser)] []
  case exists of
    Nothing -> Just <$> insert newUser
    Just _  -> return Nothing

  
-- note: <$> is the infix symbol for =fmap=
-- note: using Prelude.map to avoid confusion with Data.Text.Map 
-- note: DB actions are IO actions, and will therefore return a value of IO ()
-- and therefore we need to use liftIO () to raise it to the Handler monad
-- this is where monad transformations come in
  

server :: ConnectionPool -> Server UserAPI
server pool =
  showUsersHandler :<|> addUserHandler
  where
--    showUsersHandler :: ConnectionPool -> Maybe ([User])
    showUsersHandler = liftIO $ showAllUsersHelper pool 

--    addUsersHandler :: User -> Maybe (Key User)
    addUserHandler newUser = liftIO $ addUserHelper pool newUser


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
