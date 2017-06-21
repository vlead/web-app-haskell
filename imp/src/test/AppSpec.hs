{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}


module AppSpec where


import Data.Aeson
import Data.Proxy 
import Data.Text

import Database.Persist.Sql

import GHC.Generics

import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad.Trans.Except

import Network.HTTP.Client
import Network.Wai.Handler.Warp
 
import Servant.API
import Servant.Client

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

-- session for user admin-user
adminOneSession :: Session
adminOneSession = createSession 1 "admin@email.com" Admin


-- user to add
userOneData :: User
userOneData = createUser "small-cat" "small@cat.com" NonAdmin
 
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
testIndex :<|> testLogin :<|> testShowUsers :<|> testAddUser :<|> testDeleteUser :<|> testLogout = client userAPI 

spec :: Spec
spec = do
  around withApp $ do
    
    describe "/index" $ do
      it "Returns value from Index page" $ \ port -> do
        (try port testIndex) `shouldReturn` pack("Welcome to User Directory")

    describe "/login" $ do
      it "Successfully logs in initial admin user" $ \ port -> do
        (try port $ testLogin $ adminOneSession) `shouldReturn` (sessionResponse 1)

    describe "/addUser" $ do
      it "Successfully adds an user" $ \ port -> do
        (try port $ testAddUser (Just "1") userOneData) `shouldReturn` (userResponse 2)
        
    describe "/logout" $ do
      it "Successfully logs out logged-in user" $ \ port -> do
        (try port $ testLogout (Just "1") adminOneSession) `shouldReturn` Just adminOneSession
        

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action


errorText :: Text
errorText = pack("Error")


try port query = do
  manager <- newManager defaultManagerSettings
  res <- runClientM query (ClientEnv manager (BaseUrl Http "localhost" port ""))
  case res of
    Left err -> throwIO $ ErrorCall $ show err
    Right xs -> return xs
