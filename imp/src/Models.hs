{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Models where
import Data.Aeson
import Data.Text

import Role


import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist 
import           Database.Persist.Sqlite
import           Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name String
  email String
  roles Role
  UniqueName name
  UniqueEmail email
  deriving Eq Read Show
Session
  userEmail String
  userRole String
  deriving Eq Read Show
|]


instance ToJSON User where
  toJSON (User name email roles) =
    object [ "name" .= name
           , "email"  .= email
           , "roles" .= roles]

instance FromJSON Session where
  parseJSON = withObject "Session" $ \ v ->
    Session <$> v .: "email"
            <*> v .: "role"

instance ToJSON Session where
  toJSON (Session sessionEmail sessionRole) =
    object ["email" .= sessionEmail
            , "role" .= sessionRole ]
