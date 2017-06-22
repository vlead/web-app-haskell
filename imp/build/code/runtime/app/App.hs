{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

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
import           Controllers 


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
assassinateSessions :: ConnectionPool -> IO ()
assassinateSessions pool = flip runSqlPersistMPool pool $
  deleteWhere ([] :: [Filter Session])
  
server :: ConnectionPool -> Server UserAPI
server pool =
            (indexHandler 
       :<|> loginHandler)
       :<|> (logoutHandler)
       :<|> (showUsersHandler
       :<|> addUserHandler
       :<|> deleteUserHandler)

       where

         
         indexHandler :: Handler (Text)
         indexHandler = return "Welcome to User Directory"


         loginHandler :: Session -> Handler (Maybe (ResponseSessionId))
         loginHandler newSession = liftIO $ loginHelper newSession pool


         -- authorisation required: login
         showUsersHandler :: Maybe (String) -> Handler ([User])
         showUsersHandler authVal = liftIO $
           (showAllUsersHelper pool) =<< (loginCheck pool $ headerCheck authVal)


         -- authorisation required: admin login
         addUserHandler :: Maybe (String) -> User -> Handler (Maybe (ResponseUserId))
         addUserHandler authVal newUser = liftIO $ (addUserHelper newUser pool) =<< (adminAuthCheck pool $ headerCheck authVal)
        
        
         -- authorisation required: admin login
         deleteUserHandler :: Maybe (String) -> UniqueUserData -> Handler (Maybe (User))
         deleteUserHandler authVal userToDel = liftIO $
           (deleteUserHelper (toTextDatatype userToDel) pool)
             =<< (isNotAdminSelfCheck pool (toTextDatatype userToDel) $ headerCheck authVal)


         -- authorisation required: login
         logoutHandler :: Maybe (String) -> Session -> Handler (Maybe (Session))
         logoutHandler authVal currentSession = liftIO $
           (logoutHelper currentSession pool)
             =<< (isSelfCheck pool (sessionToEmail currentSession) $ headerCheck authVal)




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
  assassinateSessions pool
  return $ app pool


-- to run the SQL database
run :: FilePath -> IO ()
run sqliteFile = 
  Warp.run 8000 =<< mkApp sqliteFile