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

toUserDatatype :: UserData -> User
toUserDatatype (UserData userDataName userDataEmail) = User {userName=userDataName,  userEmail=userDataEmail, userRoles=NonAdmin}

toTextDatatype :: UniqueUserData -> Text
toTextDatatype (UniqueUserData userData) = pack(userData)


-- helper function for showUsersHandler
showAllUsersHelper :: ConnectionPool -> IO ([User])
showAllUsersHelper pool = flip runSqlPersistMPool pool $ do
  users <- selectList [] []
  return $ Prelude.map entityVal users


-- helper function for addUserHandler
addUserHelper :: ConnectionPool -> User -> IO (Maybe (Key (User)))
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


-- helper function for loginHandler
loginHelper :: ConnectionPool -> Session -> IO (Maybe (Key (Session)))
loginHelper pool newSession = flip runSqlPersistMPool pool $ do
  ifExists <- selectFirst [UserEmail ==. (sessionUserEmail newSession)] []
  case ifExists of
    Nothing -> return Nothing
    Just _  -> Just <$> insert newSession
  


-- helper function for logoutHandler
logoutHelper :: ConnectionPool -> Session -> IO (Maybe (Session))
logoutHelper pool currentSession = flip runSqlPersistMPool pool $ do
  ifExists <- selectFirst [SessionUserEmail ==. (sessionUserEmail currentSession), SessionUserRole ==. (sessionUserRole currentSession)] []
  case ifExists of
    Nothing -> return Nothing
    Just _ -> do
      deleteWhere [SessionUserEmail ==. (sessionUserEmail currentSession)]
      return $ entityVal <$> ifExists
      
  

-- to check if admin user exists
adminUserCheck :: ConnectionPool -> IO(String)
adminUserCheck pool = flip runSqlPersistMPool pool $ do
  adminUser <- selectFirst [UserRoles ==. Admin] []
  case adminUser of
    Nothing -> do
      adminUserId <- insert $ User "admin-user" "admin@email.com" $ Admin
      return "Admin User Added"
    Just _ -> return "Admin User Exists"
    


server :: ConnectionPool -> Server UserAPI
server pool =
       (indexHandler :<|> loginHandler)
  :<|> (showUsersHandler :<|> addUserHandler
  :<|>  deleteUserHandler :<|> logoutHandler )

  where

    indexHandler :: Handler (Text)
    indexHandler = return "Index Page"

    loginHandler :: Session -> Handler (Maybe (Key (Session)))
    loginHandler newSession = liftIO $ loginHelper pool newSession

    showUsersHandler :: Handler ([User])
    showUsersHandler = liftIO $ showAllUsersHelper pool 

    addUserHandler :: UserData -> Handler (Maybe (Key (User)))
    addUserHandler newUser = liftIO $ addUserHelper pool $ toUserDatatype newUser

    deleteUserHandler :: UniqueUserData -> Handler (Maybe (User))
    deleteUserHandler userToDel = liftIO $ deleteUserHelper pool $ toTextDatatype userToDel

    logoutHandler :: Session -> Handler (Maybe (Session))
    logoutHandler currentSession = liftIO $ logoutHelper pool currentSession


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
  adminUserCheck pool
  return $ app pool


-- to run the SQL database
run :: FilePath -> IO ()
run sqliteFile = 
  Warp.run 8000 =<< mkApp sqliteFile
