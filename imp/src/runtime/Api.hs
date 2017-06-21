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

data UniqueUserData = UniqueUserData {
                                       userData :: String
                                     } deriving (Eq, Read, Show)


instance ToJSON UniqueUserData where
  toJSON (UniqueUserData userData) =
    object ["data" .= userData]

instance FromJSON UniqueUserData where
  parseJSON = withObject "UniqueUserData" $ \ v ->
    UniqueUserData <$> v .: "data"



data ResponseUserId = ResponseUserId {
                                     userIdValue :: (Key (User))
                                     } deriving (Generic, Eq, Read, Show)


                      

data ResponseSessionId = ResponseSessionId {
                                     sessionIdValue :: (Key (Session))
                                     } deriving (Generic, Eq, Read, Show)


type UserAPI = "index" :> Get '[PlainText] Text
          :<|> "login" :> ReqBody '[JSON] Session :> Post '[JSON] (Maybe (ResponseSessionId))
          :<|> Header "Cookie" String :> "showUsers" :> Get '[JSON] [User]
          :<|> Header "Cookie" String :> "addUser" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (ResponseUserId))
          :<|> Header "Cookie" String :> "deleteUser" :> ReqBody '[JSON] UniqueUserData :> Post '[JSON] (Maybe (User))
          :<|> Header "Cookie" String :> "logout" :> ReqBody '[JSON] Session :> Post '[JSON] (Maybe (Session))
