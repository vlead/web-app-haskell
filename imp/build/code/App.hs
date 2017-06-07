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
--showAllUsers :: ConnectionPool -> IO (Maybe User)
showAllUsers pool = flip runSqlPersistMPool pool $ do
  users <- selectList [] []
  return users
-- note: <$> is the infix symbol for =fmap=

  

server :: ConnectionPool -> Server UserAPI
server pool =
  showUsersHandler :<|> addUsersHandler
  where
    showUsersHandler :: ConnectionPool -> Maybe ([User])
    showUsersHandler = showAllUsers $ pool 

    addUsersHandler :: User -> Maybe (Key User)
    addUsersHandler user = return keyUser


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
