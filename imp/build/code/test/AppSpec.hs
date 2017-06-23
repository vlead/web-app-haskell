{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}


module AppSpec where


import Data.Aeson
import Data.Proxy 
import Data.Text

import Database.Persist.Sql

import GHC.Generics

import Control.Exception 
import Control.Monad.Trans.Except

import Network.HTTP.Client
import Network.Wai.Handler.Warp
 
import Servant.API
import Servant.Client
import Servant.Server

import App
import Models
import Api
import Role

import Test.Hspec
import Test.Mockery.Directory
import Test.QuickCheck


-- sample data set to be used for testing


-- to create a value of Session datatype
createSession :: Integer -> String -> Role -> Session
createSession id email role = Session (toSqlKey $ fromInteger id) email role


-- to create a value of User datatype
createUser :: String -> String -> Role -> User
createUser name email role = User name email role


-- to create a value of Maybe ResponseSessionId datatype
sessionResponse :: Integer -> Maybe ResponseSessionId
sessionResponse x = Just $ ResponseSessionId (toSqlKey $ fromInteger x)


-- to create a value of Maybe ResponseUserId datatype
userResponse :: Integer -> Maybe ResponseUserId
userResponse x = Just $ ResponseUserId (toSqlKey $ fromInteger x)


-- data for user admin-user
adminOneData :: User
adminOneData = createUser "admin-user" "admin@email.com" Admin


-- session for user admin-user
adminOneSession :: Session
adminOneSession = createSession 1 "admin@email.com" Admin


-- user to add
userOneData :: User
userOneData = createUser "small-cat" "small@cat.com" NonAdmin


-- session for user small-cat
userOneSession :: Session
userOneSession = createSession 2 "small@cat.com" NonAdmin


-- another user to add
userTwoData :: User
userTwoData = createUser "large-cat" "large@cat.com" NonAdmin
 
-- query function types
testIndex :: ClientM Text
testLogin :: Session -> ClientM (Maybe (ResponseSessionId))
testShowUsers :: Maybe (String) ->  ClientM [User]
testAddUser :: Maybe (String) -> User -> ClientM (Maybe (ResponseUserId))
testDeleteUser :: Maybe (String) -> UniqueUserData -> ClientM (Maybe (User))
testLogout :: Maybe (String) -> Session -> ClientM (Maybe (Session))


-- a proxy to our API
userAPI :: Data.Proxy.Proxy UserAPI
userAPI = Data.Proxy.Proxy


-- code that returns the client functions for our API
(testIndex :<|> testLogin) :<|> (testLogout) :<|> (testShowUsers :<|> testAddUser :<|> testDeleteUser) = client userAPI 

spec :: Spec
spec = do
  around withApp $ do


    describe "/index" $ do
      it "Returns value from Index page" $ \ port -> do
        (tryQuery port testIndex) `shouldReturn` pack("Welcome to User Directory")


    describe "/login" $ do
      
      it "Operates successfully" $ \ port -> do
        (tryQuery port $ testLogin adminOneSession) `shouldReturn` (sessionResponse 1)

      it "Attempts to login an user not in the system" $ \ port -> do
        (tryQuery port $ testLogin userOneSession) `shouldReturn` Nothing
        

    describe "/addUser" $ do

      it "Operates successfully" $ \ port -> do
        -- login an Admin user who can add users
        tryQuery port $ testLogin adminOneSession
        -- adds an user
        (tryQuery port $ testAddUser (Just "1") userOneData) `shouldReturn` (userResponse 2)

      it "Only Admin user can add user" $ \ port -> do
        -- login Admin user "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- add NonAdmin user using credentials of "admin-user"
        tryQuery port $ testAddUser (Just "1") userOneData 
       -- login NonAdmin user "small-cat"
        tryQuery port $ testLogin userOneSession
        -- test-add user using credentials of "small-cat"
        (tryQuery port $ testAddUser (Just "2") userTwoData) `shouldThrow` anyErrorCall

      it "Adding user when Session is not present in database" $ \ port -> do
        (tryQuery port $ testAddUser (Just "1") userOneData) `shouldThrow` anyErrorCall


    describe "/deleteUser" $ do

      it "Deletes user successfully" $ \ port -> do
        -- to login "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- to add an user
        tryQuery port $ testAddUser (Just "1") userOneData
        -- to delete same user using credentials of "admin-user"
        (tryQuery port $ testDeleteUser (Just "1") (UniqueUserData "small@cat.com")) `shouldReturn` (Just userOneData)

      it "Only Admin user can delete user" $ \ port -> do
        -- login Admin user "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- add NonAdmin user using credentials of "admin-user"
        tryQuery port $ testAddUser (Just "1") userOneData 
       -- login NonAdmin user "small-cat"
        tryQuery port $ testLogin userOneSession
        -- test-delete user using credentials of "small-cat"
        (tryQuery port $ testDeleteUser (Just "2") (UniqueUserData "admin@email.com")) `shouldThrow` anyException

      it "Cannot delete oneself" $ \ port -> do
        -- login Admin user "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- Admin user attempts to delete self
        (tryQuery port $ testDeleteUser (Just "1") (UniqueUserData "admin@email.com")) `shouldThrow` anyException

      it "Delete when Session not present in database" $ \ port -> do
        -- login Admin user "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- add user to database using credentials of "admin-user"
        tryQuery port $ testAddUser (Just "1") userOneData
        -- test delete user using credentials of non-logged-in user
        (tryQuery port $ testDeleteUser (Just "3") (UniqueUserData "admin@email.com")) `shouldThrow` anyException


    describe "/showUsers" $ do

      it "Shows list of three users successfully" $ \ port -> do
        -- login Admin user "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- add user "small-cat"
        tryQuery port $ testAddUser (Just "1") userOneData
        -- add user "large-cat"
        tryQuery port $ testAddUser (Just "1") userTwoData
        -- test-show all users using credentials of "admin-user"
        (tryQuery port $ testShowUsers (Just "1")) `shouldReturn` [adminOneData, userOneData, userTwoData]

      it "Cannot access list of users when session not in database" $ \ port -> do
        (tryQuery port $ testShowUsers (Just "1")) `shouldThrow` anyException
        

    describe "/logout" $ do

      it "Logs out user successfully" $ \ port -> do
        -- to log in user "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- test-log out user "admin-user" using own credentials
        (tryQuery port $ testLogout (Just "1") adminOneSession) `shouldReturn` (Just adminOneSession)

      it "Cannot log out when session not in database" $ \ port -> do
        -- test-log out random user not logged-in
        (tryQuery port $ testLogout (Just "1") adminOneSession) `shouldThrow` anyException

      it "Cannot log out non-self user" $ \ port -> do
        -- log in Admin user "admin-user"
        tryQuery port $ testLogin adminOneSession
        -- add user "small-cat"
        tryQuery port $ testAddUser (Just "1") userOneData
        -- log in user "small-cat"
        tryQuery port $ testLogin userOneSession
        -- log out "admin-user" using credentials of "small-cat"
        (tryQuery port $ testLogout (Just "2") adminOneSession) `shouldThrow` anyException
        

        
        

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action


errorText :: Text
errorText = pack("Error")


tryQuery port query = do
  manager <- newManager defaultManagerSettings
  res <- runClientM query (ClientEnv manager (BaseUrl Http "localhost" port ""))
  case res of
    Left err -> throwIO $ ErrorCall $ show err
    Right xs -> return xs
