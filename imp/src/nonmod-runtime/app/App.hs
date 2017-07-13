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
  adminUser <- selectFirst [UserRoles ==. [Admin]] []
  case adminUser of
    Nothing -> do
      adminUserId <- insert $ User "admin-user" "admin@email.com" $ [Admin]
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
       :<|> (showUsersHandler
       :<|> logoutHandler
       :<|> setNameHandler
       :<|> setEmailHandler)
       :<|> (showUserDetailsHandler
       :<|> addUserHandler
       :<|> deleteUserHandler
       :<|> showSessionsHandler
       :<|> showRolesHandler
       :<|> addRoleHandler
       :<|> deleteRoleHandler)

            where



        -- authorisation required: none
        indexHandler :: Handler (Text)
        indexHandler = return "Welcome to User Directory"


        -- authorisation required: none
        loginHandler :: Session -> Handler (Maybe (ResponseSessionId))
        loginHandler newSession = do
          isAuthorised <- liftIO (hasRole pool newSession)
          case isAuthorised of
            True  -> do
              ret <- liftIO $ loginHelper newSession pool
              case ret of
                Nothing -> throwError err400  -- error: constraint error
                Just _  -> return ret
            False -> throwError err403


        -- authorisation required: login
        showUsersHandler :: Maybe (String) -> Handler ([ShowUserData])
        showUsersHandler authVal = do
          isAuthorised <- liftIO (loginCheck pool $ headerCheck authVal)
          case isAuthorised of
            True  -> liftIO $ (showAllUsersHelper pool True)
            False -> throwError err403 {errBody = "User not logged in."}


        -- authorisation required: admin login
        addUserHandler :: Maybe (String) -> User -> Handler (Maybe (ResponseUserId))
        addUserHandler authVal newUser = do
          isAuthorised <- liftIO (adminAuthCheck pool $ headerCheck authVal)
          case isAuthorised of
            True  -> do
              ret <- liftIO $ (addUserHelper newUser pool True)
              case ret of
                Nothing -> throwError err404  -- error: constraint error
                Just _ -> return ret
            False -> throwError err401 {errBody = "Permission Denied."} 


        -- authorisation required: admin login
        deleteUserHandler :: Maybe (String) -> UniqueUserData -> Handler (Maybe (User))
        deleteUserHandler authVal userToDel = do
          isAuthorised <- liftIO (isNotAdminSelfCheck pool (toTextDatatype userToDel) $ headerCheck authVal) 
          case isAuthorised of
            True  -> do
              ret <- liftIO (deleteUserHelper (toTextDatatype userToDel) pool True)
              case ret of
                Nothing -> throwError err404  -- error: user not found error
                Just _  -> return ret
            False -> throwError err401 {errBody = "Permission Denied."} 


        -- authorisation required: login
        logoutHandler :: Maybe (String) -> Session -> Handler (Maybe (Session))
        logoutHandler authVal currentSession = do
          isAuthorised <- liftIO (isSelfCheck pool (sessionToEmail currentSession) $ headerCheck authVal) 
          case isAuthorised of
            True  -> do
              ret <- liftIO (logoutHelper currentSession pool True)
              case ret of
                Nothing -> throwError err404  -- error: user not found
                Just _  -> return ret
            False -> throwError err401 {errBody = "Permission Denied."}


        -- authorisation required : either NonAdmin and Self, or Admin
        setNameHandler :: Maybe (String) -> UpdateUserData -> Handler (Maybe (User))
        setNameHandler authVal userData = do
          isAuthorised <- liftIO (isEitherAdminOrSelfCheck pool (currentData userData) $ headerCheck authVal)
          case isAuthorised of
            True  -> do
              ret <- liftIO (setNameHelper pool userData)
              case ret of
                Nothing -> throwError err500
                Just _  -> return ret 
            False -> throwError err401 {errBody = "Permission Denied."}


        -- authorisation required : either NonAdmin and Self, or Admin
        setEmailHandler :: Maybe (String) -> UpdateUserData -> Handler (Maybe (User))
        setEmailHandler authVal userData = do
          isAuthorised <- liftIO (isEitherAdminOrSelfCheck pool (currentData userData) $ headerCheck authVal)
          case isAuthorised of
            True  -> do
              ret <- liftIO (setEmailHelper pool userData)
              case ret of
                Nothing -> throwError err500
                Just _  -> return ret
            False -> throwError err401 {errBody = "Permission Denied."}


        -- authorisation required: admin login
        showUserDetailsHandler :: Maybe (String) -> String -> Handler (Maybe (User))
        showUserDetailsHandler authVal userData = do
          isAuthorised <- liftIO (adminAuthCheck pool $ headerCheck authVal)
          case isAuthorised of
            True  -> do
              ret <- liftIO (showUserDetailsHelper pool userData)
              case ret of
                Nothing -> throwError err500
                Just _  -> return ret
            False -> throwError err401 {errBody = "UNAUTHORISED."}


        -- authorisation required: admin login
        showSessionsHandler :: Maybe (String) -> Handler ([Session])
        showSessionsHandler authVal = do
          isAuthorised <- liftIO (adminAuthCheck pool $ headerCheck authVal)
          case isAuthorised of
            True  -> liftIO (showSessionsHelper pool)
            False -> throwError err401 {errBody = "UNAUTHORISED."}


        -- authorisation required: admin login
        showRolesHandler :: Maybe (String) -> String -> Handler ([Role])
        showRolesHandler authVal userData = do
          isAuthorised <- liftIO (adminAuthCheck pool $ headerCheck authVal)
          case isAuthorised of
            True  -> do
              ret <- liftIO (showRolesHelper pool userData)
              case ret of
                [] -> throwError err500
                otherwise  -> return ret 
            False -> throwError err401 {errBody = "UNAUTHORISED."}


        -- authorisation required: admin login
        addRoleHandler :: Maybe (String) -> String -> Role -> Handler (Maybe (User))
        addRoleHandler authVal userData newRole = do
          isAuthorised <- liftIO (adminAuthCheck pool $ headerCheck authVal)
          case isAuthorised of
            True  -> do
              ret <- liftIO (addRoleHelper pool userData newRole)
              case ret of
                Nothing -> throwError err500
                Just _  -> return ret
            False -> throwError err401 {errBody = "UNAUTHORISED."}


        -- authorisation required: admin login
        deleteRoleHandler :: Maybe (String) -> String -> Role -> Handler (Maybe (User))
        deleteRoleHandler authVal userData roleToDel = do
          isAuthorised <- liftIO (adminAuthCheck pool $ headerCheck authVal)
          case isAuthorised of
            True  -> do
              ret <- liftIO (deleteRoleHelper pool userData roleToDel)
              case ret of
                Nothing -> throwError err500
                Just _  -> return ret
            False -> throwError err401 {errBody = "UNAUTHORISED."}



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
