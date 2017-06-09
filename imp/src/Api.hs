{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

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

import Data.Proxy
import Data.Text

import Database.Persist

import Models
import Servant.API
data UserData = UserData {
                         userDataName :: String
                         , userDataEmail :: String
                         } deriving (Show, Read, Eq)

instance ToJSON UserData where
  toJSON (UserData userDataName userDataEmail) =
    object ["name" .= userDataName
           ,"email" .= userDataEmail ]

instance FromJSON UserData where
  parseJSON = withObject "UserData" $ \ v ->
    UserData <$> v .: "name"
             <*> v .: "email"
 


data UniqueUserData = UniqueUserData {
                                       userData :: String
                                     } deriving (Eq, Read, Show)


instance ToJSON UniqueUserData where
  toJSON (UniqueUserData userData) =
    object ["data" .= userData]

instance FromJSON UniqueUserData where
  parseJSON = withObject "UniqueUserData" $ \ v ->
    UniqueUserData <$> v .: "data"



type NonSecureRoutes = "index" :> Get '[PlainText] Text
                     :<|> "login" :> ReqBody '[JSON] Session :> Get '[PlainText] Text
                     
 
type SecureRoutes = "showUsers" :> Get '[JSON] [User]
             :<|> "addUser" :> ReqBody '[JSON] UserData :> Post '[JSON] (Maybe (Key User))
             :<|> "deleteUser" :> ReqBody '[JSON] UniqueUserData :> Post '[JSON] (Maybe (User))
             :<|> "logout" :> ReqBody '[JSON] Session :> Get '[PlainText] Text
             
type UserAPI = NonSecureRoutes :<|> SecureRoutes
