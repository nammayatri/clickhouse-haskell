-- Copyright (c) 2020-present, EMQX, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Connection pool for HTTP connection. User should import Database.ClickHouseDriver.HTTP instead
module Database.ClickHouseDriver.HTTP.Connection
  ( httpConnect,
    httpConnectDb,
    defaultHttpConnection,
    HttpConnection (..),
    createHttpPool,
  )
where

import Data.Bool (bool)
import Data.Default.Class (Default (..))
import Data.Pool (Pool, createPool)
import Data.Time.Clock (NominalDiffTime)
import Database.ClickHouseDriver.HTTP.Types
  ( HttpConnection (..),
    HttpParams
      ( HttpParams,
        httpDatabase,
        httpHost,
        httpPassword,
        httpPort,
        httpUsername
      ),
  )
import Network.HTTP.Client
  ( Manager,
    defaultManagerSettings,
    newManager,
  )
import qualified Network.HTTP.Client.TLS as TLS

#define DEFAULT_USERNAME  "default"
#define DEFAULT_HOST_NAME "localhost"
#define DEFAULT_PASSWORD  ""
--TODO change default password to ""

defaultHttpConnection :: IO (HttpConnection)
defaultHttpConnection = httpConnect DEFAULT_USERNAME DEFAULT_PASSWORD 8123 DEFAULT_HOST_NAME False

instance Default HttpParams where
  def =
    HttpParams
      { httpHost = DEFAULT_HOST_NAME,
        httpPassword = DEFAULT_PASSWORD,
        httpPort = 8123,
        httpUsername = DEFAULT_USERNAME,
        httpDatabase = Nothing
      }

createHttpPool ::
  HttpParams ->
  Bool ->
  Int ->
  NominalDiffTime ->
  Int ->
  IO (Pool HttpConnection)
createHttpPool
  HttpParams
    { httpHost = host,
      httpPassword = password,
      httpPort = port,
      httpUsername = user,
      httpDatabase = db
    }
  needTls =
    createPool
      ( do
          httpConnectDb db user password port host needTls
      )
      (\_ -> return ())

httpConnect :: String -> String -> Int -> String -> Bool -> IO (HttpConnection)
httpConnect = httpConnectDb Nothing

httpConnectDb :: Maybe String -> String -> String -> Int -> String -> Bool -> IO (HttpConnection)
httpConnectDb database user password port host needTls = do
  mng <- bool (newManager defaultManagerSettings) TLS.newTlsManager needTls
  return
    HttpConnection
      { httpParams =
          HttpParams
            { httpHost = host,
              httpPassword = password,
              httpPort = port,
              httpUsername = user,
              httpDatabase = database
            },
        httpManager = mng,
        httpTls = needTls
      }
