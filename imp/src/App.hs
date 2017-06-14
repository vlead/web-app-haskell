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
import           Authentication 


toTextDatatype :: UniqueUserData -> Text
toTextDatatype (UniqueUserData userData) = pack(userData)


-- helper function for showUsersHandler
showAllUsersHelper :: ConnectionPool -> Bool -> IO ([User])
showAllUsersHelper pool authVal = flip runSqlPersistMPool pool $ case authVal of
    False -> return []
    True  -> do
      users <- selectList [] []
      return $ Prelude.map entityVal users


-- helper function for addUserHandler
addUserHelper :: User -> ConnectionPool -> Bool -> IO (Maybe (Key (User)))
addUserHelper newUser pool authVal = flip runSqlPersistMPool pool $ case authVal of
  False -> return Nothing
  True  -> do
    exists <- selectFirst [UserName ==. (userName newUser)] []
    case exists of
      Nothing -> Just <$> insert newUser
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
loginHelper :: Session -> ConnectionPool -> IO (Maybe (Key (Session)))
loginHelper newSession pool = flip runSqlPersistMPool pool $ do
  ifExists <- selectFirst [UserEmail ==. (sessionUserEmail newSession)] []
  case ifExists of
    Nothing -> return Nothing
    Just _  -> Just <$> insert newSession
  

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
      
  

-- | To check if admin user exists
adminUserCheck :: ConnectionPool -> IO(String)
adminUserCheck pool = flip runSqlPersistMPool pool $ do
  adminUser <- selectFirst [UserRoles ==. Admin] []
  case adminUser of
    Nothing -> do
      adminUserId <- insert $ User "admin-user" "admin@email.com" $ Admin
      return "Admin User Added"
    Just _ -> return "Admin User Exists"
    

-- | To kill all sessions in database on initialisation
assassinitSessions :: ConnectionPool -> IO ()
assassinitSessions pool = flip runSqlPersistMPool pool $
  deleteWhere ([] :: [Filter Session])
  
server :: ConnectionPool -> Server UserAPI
server pool =
            indexHandler 
       :<|> loginHandler
       :<|> authRoutesHandler

  where

    indexHandler :: Handler (Text)
    indexHandler = return "Welcome to User Directory"

    loginHandler :: Session -> Handler (Maybe (Key (Session)))
    loginHandler newSession = liftIO $ loginHelper newSession pool

    authRoutesHandler authVal =
           showUsersHandler authVal
      :<|> addUserHandler authVal
      :<|> deleteUserHandler authVal
      :<|> logoutHandler authVal

      where

        -- authorisation required: login
        showUsersHandler :: Maybe (String) -> Handler ([User])
        showUsersHandler authVal = liftIO $
          (showAllUsersHelper pool) =<< (loginCheck pool $ headerCheck authVal)

        -- authorisation required: admin login
        addUserHandler :: Maybe (String) -> User -> Handler (Maybe (Key (User)))
        addUserHandler authVal newUser = liftIO $
          (addUserHelper newUser pool) =<< (adminAuthCheck pool $ headerCheck authVal)

        -- authorisation required: admin login
        deleteUserHandler :: Maybe (String) -> UniqueUserData -> Handler (Maybe (User))
        deleteUserHandler authVal userToDel = liftIO $
          (deleteUserHelper (toTextDatatype userToDel) pool) =<< (adminAuthCheck pool $ headerCheck authVal)

        -- authorisation required: login
        logoutHandler :: Maybe (String) -> Session -> Handler (Maybe (Session))
        logoutHandler authVal currentSession = liftIO $
          (logoutHelper currentSession pool) =<< (loginCheck pool $ headerCheck authVal)


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
  assassinitSessions pool
  return $ app pool


-- to run the SQL database
run :: FilePath -> IO ()
run sqliteFile = 
  Warp.run 8000 =<< mkApp sqliteFile
