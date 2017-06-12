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

-- to check if user passed has session in the session database
-- if so, then return function
sessionCheck :: ConnectionPool -> User -> IO (Bool)
sessionCheck pool currentSession = flip runSqlPersistMPool pool $ do
  ifExists <- selectFirst [SessionUserEmail ==. (userEmail currentSession)] []
  case ifExists of
    Nothing -> return False
    Just _  -> return True

authExec :: String -> ConnectionPool -> IO(Bool)
authExec authSession pool = flip runSqlPersistMPool pool $ do
  ifExists <- selectFirst [SessionUserEmail ==. authSession] []
  case ifExists of
    Nothing -> return False
    Just _  -> return True
