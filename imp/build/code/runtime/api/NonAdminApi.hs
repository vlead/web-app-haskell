{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NonAdminApi where

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

type ShowUsersApi = Header "Cookie" String :> "showUsers" :> Get '[JSON] [ShowUserData] 


type LogoutApi = Header "Cookie" String :> "logout" :> ReqBody '[JSON] Session :> Post '[JSON] (Maybe (Session)) 


type SetNameApi = Header "Cookie" String :> "setName" :> ReqBody '[JSON] UpdateUserData :> Post '[JSON] (Maybe (User)) 


type SetEmailApi = Header "Cookie" String :> "setEmail" :> ReqBody '[JSON] UpdateUserData :> Post '[JSON] (Maybe (User)) 


type NonAdminRoutes = ShowUsersApi :<|> LogoutApi :<|> SetNameApi :<|> SetEmailApi
