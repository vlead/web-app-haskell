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
import Login
import ShowUsers
import Logout
import SetName
import SetEmail
import ShowUserDetails
import AddUser
import DeleteUser
import ShowSessions
import ShowRoles
import AddRole
import DeleteRole
import Index

import Servant.API
type UserAPI = IndexApi 
          :<|> LoginApi
          :<|> ShowUsersApi
          :<|> LogoutApi
          :<|> SetNameApi
          :<|> SetEmailApi
          :<|> ShowUserDetailsApi
          :<|> AddUserApi
          :<|> DeleteUserApi
          :<|> ShowSessionsApi
          :<|> ShowRolesApi
          :<|> AddRoleApi
          :<|> DeleteRoleApi
