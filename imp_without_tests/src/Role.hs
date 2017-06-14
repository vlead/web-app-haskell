-- Role.hs
-- defining the enumerated Role datatype

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module Role where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson
import Data.Text

data Role = Admin | NonAdmin
  deriving (Generic, Show, Read, Eq)

instance ToJSON Role where
  toJSON Admin = object [pack("role") .= "Admin"]
  toJSON NonAdmin = object [pack("role") .= "NonAdmin"]

instance FromJSON Role where
  parseJSON (Object o) = do
    role <- o .: pack("role")
    case (role :: String) of
      "Admin" -> return Admin
      "NonAdmin" -> return NonAdmin

-- storing this datatype as a PersistField type
derivePersistField "Role"
