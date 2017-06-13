{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Authentication where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions
import qualified Data.ByteString.Lazy.Char8 as B           

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant

import           Data.Text

import           Api
import           Models
import           Role

-- | To check if user is logged in
authCheck :: Maybe String -> ConnectionPool -> IO(Bool)
authCheck authSession pool = flip runSqlPersistMPool pool $ do
  case authSession of
    Nothing -> return False
    Just xs -> do
      ifExists <- selectFirst [SessionUserEmail ==. xs] []
      case ifExists of
        Nothing -> return False
        Just _  -> return True
