{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type UserAPI = "user" :> 
  ("showUsers" :> Get '[JSON] [User]
  
  

-- role type 
-- note: Role: use algebraic data types- show

-- note: email :: String for now- complete email type constructor

data User = User
  { id :: Int,
    name :: String,
    email :: String,
    role :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
users1 :: [User]
users1 =
  [ User 1 "Small Cat" "smallcat@gmail.com" "Admin"  
  , User 2 "Large Cat" "largecat@gmail.com" "User"
  ]
server1 :: Server UserAPI
server1 = return users1

userAPI :: Proxy UserAPI
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1

main :: IO ()
main = do
  putStrLn "Running on port 8000"
  run 8000 app1
