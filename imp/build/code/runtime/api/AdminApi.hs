{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module AdminApi where

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
import Role

import Servant.API

type AddUserApi = Header "Cookie" String :> "addUser" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (ResponseUserId))


type DeleteUserApi = Header "Cookie" String :> "deleteUser" :> ReqBody '[JSON] UniqueUserData :> Post '[JSON] (Maybe (User))


type ShowUserDetailsApi = Header "Cookie" String :> "showUserDetails" :> Capture "email" String :> Post '[JSON] (Maybe (User))

type ShowSessionsApi = Header "Cookie" String :> "showSessions" :> Post '[JSON] [Session]

type ShowRolesApi = Header "Cookie" String :> "showRoles" :> Capture "email" String :> Post '[JSON] [Role]

type AddRoleApi = Header "Cookie" String :> "addRole" :> Capture "email" String :> Capture "role" Role :> Post '[JSON] (Maybe (User))

type DeleteRoleApi = Header "Cookie" String :> "deleteRole" :> Capture "email" String :> Capture "role" Role :> Post '[JSON] (Maybe (User))


type AdminRoutes = ShowUserDetailsApi :<|> AddUserApi :<|> DeleteUserApi :<|> ShowSessionsApi :<|> ShowRolesApi :<|> AddRoleApi :<|> DeleteRoleApi
