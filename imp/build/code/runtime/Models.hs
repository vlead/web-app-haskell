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
  UniqueEmail email
  deriving Eq Read Show
Session
  mappingToUser UserId
  userEmail String
  userRoles Role
  UniqueSessionDetails userEmail userRoles
  deriving Eq Read Show
|]
