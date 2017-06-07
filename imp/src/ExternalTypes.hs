-- defining the enumerated Role datatype

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module ExternalTypes where

import Database.Persist.TH
import GHC.Generics


data Role = Admin | NonAdmin
  deriving (Generic, Show, Read, Eq)


-- storing this datatype as a PersistField type
derivePersistField "Role"
