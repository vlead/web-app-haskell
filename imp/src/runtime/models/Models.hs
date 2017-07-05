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
import Web.HttpApiData

import Role


import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist 
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name String
  email String
  roles [Role]
  UniqueEmail email
  deriving Eq Read Show
Session
  userMappingToSession UserId
  userEmail String
  userRoles [Role]
  UniqueSessionDetails userEmail userRoles
  deriving Eq Read Show
|]


data UniqueUserData = UniqueUserData {
                                       userData :: String
                                     } deriving (Eq, Read, Show)


instance ToJSON UniqueUserData where
  toJSON (UniqueUserData userData) =
    object ["data" .= userData]

instance FromJSON UniqueUserData where
  parseJSON = withObject "UniqueUserData" $ \ v ->
    UniqueUserData <$> v .: "data"




data UpdateUserData = UpdateUserData {
                                     currentData :: String,
                                     newData :: String
                                     } deriving (Eq, Read, Show)


instance ToJSON UpdateUserData where
  toJSON (UpdateUserData currentData newData) =
    object ["current-data" .= currentData,
            "new-data"     .= newData]


instance FromJSON UpdateUserData where
  parseJSON = withObject "UpdateUserData" $ \ v ->
    UpdateUserData <$> v .: "current-data"
                   <*> v .: "new-data"

data ResponseUserId = ResponseUserId {
                                     userIdValue :: (Key (User))
                                     } deriving (Eq, Read, Show)


                      

data ResponseSessionId = ResponseSessionId {
                                     sessionIdValue :: (Key (Session))
                                     } deriving (Eq, Read, Show)



data ShowUserData = ShowUserData {
                                 showUserName  :: String,
                                 showUserEmail :: String
                                 } deriving (Show, Read, Eq)


instance ToJSON ShowUserData where
  toJSON (ShowUserData showUserName showUserEmail) =
    object ["name"  .= showUserName,
            "email" .= showUserEmail]


instance FromJSON ShowUserData where
  parseJSON = withObject "ShowUserData" $ \ v ->
    ShowUserData <$> v .: "name"
                   <*> v .: "email"


-- function that extracts ShowUserData from User 
toShowUserData :: User -> ShowUserData
toShowUserData (User userName userEmail userRoles) = ShowUserData userName userEmail


-- to extract email from session
sessionToEmail :: Session -> String
sessionToEmail (Session sessionUserId sessionUserEmail sessionUserRoles) = sessionUserEmail


toTextDatatype :: UniqueUserData -> Text
toTextDatatype (UniqueUserData userData) = pack(userData)


instance ToJSON User where
  toJSON (User name email roles) =
    object [ "name" .= name
           , "email"  .= email
           , "roles" .= roles]


instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "email"
         <*> v .: "roles"
instance FromJSON Session where
  parseJSON = withObject "Session" $ \ v ->
    Session <$> v .: "userId"
            <*> v .: "email"
            <*> v .: "roles"

instance ToJSON Session where
  toJSON (Session sessionMappingToUser sessionEmail sessionRoles) =
    object ["userId" .= sessionMappingToUser
          , "email"  .= sessionEmail
          , "roles"  .= sessionRoles]


instance FromJSON ResponseUserId where
  parseJSON (Object o) =
     ResponseUserId <$> o .: "UserId"

instance ToJSON ResponseUserId where
  toJSON (ResponseUserId value)  =
    object ["UserId" .= value]


instance FromJSON ResponseSessionId where
  parseJSON (Object o) =
     ResponseSessionId <$> o .: "SessionId"

instance ToJSON ResponseSessionId where
  toJSON (ResponseSessionId value) =
    object ["SessionId" .= value]
