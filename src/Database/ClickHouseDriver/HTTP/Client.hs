-- Copyright (c) 2020-present, EMQX, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides implementation of user's API
module Database.ClickHouseDriver.HTTP.Client
  ( -- * Setting
    runQuery,

    -- * Query
    getJSON,
    ping,
    exec,

    -- * Connection
    defaultHttpClient,
    httpClient,
    defaultHttpPool,
  )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad.State.Lazy (MonadIO (..))
import Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
  ( char8,
    lazyByteString,
    toLazyByteString,
  )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.ByteString.UTF8 as BSU
import Data.Default.Class (def)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (Typeable)
import Database.ClickHouseDriver.Defines as Defines
  ( _DEFAULT_HOST,
    _DEFAULT_HTTP_PORT,
  )
import Database.ClickHouseDriver.HTTP.Connection
  ( createHttpPool,
    defaultHttpConnection,
    httpConnect,
  )
import Database.ClickHouseDriver.HTTP.Helpers
  ( extract,
    genURL,
  )
import Database.ClickHouseDriver.HTTP.Types (Format (..), HttpConnection (..), JSONResult)
import Network.HTTP.Client
  ( Manager,
    RequestBody (..),
    httpLbs,
    method,
    parseRequest,
    requestBody,
    responseBody,
    streamFile,
  )

data HttpClient = FetchJSON String | Ping

deriving instance Show HttpClient

deriving instance Typeable HttpClient

deriving instance Eq HttpClient

class FetchData a where
  getData :: a -> HttpConnection -> IO BS.ByteString

instance FetchData HttpClient where
  getData client http = do
    case client of
      FetchJSON query -> fetchData (query ++ " FORMAT JSON") http
      Ping -> fetchData "Ping" http

-- | fetch function
fetchData ::
  String -> --query
  HttpConnection -> --httpManager
  IO BS.ByteString
fetchData query http@HttpConnection {httpManager = mng} = do
  e <- Control.Exception.try $ do
    url <- genURL http query
    req <- parseRequest url
    ans <- responseBody <$> httpLbs req mng
    return $ LBS.toStrict ans
  either
    (pure . BSU.fromString . show)
    (pure)
    (e :: Either SomeException (BS.ByteString))

-- | Fetch data from ClickHouse client in the JSON format.
getJSON :: FromJSON a => String -> HttpConnection -> IO (Either String a)
getJSON query = \conn -> extract <$> getData (FetchJSON query) conn

-- | actual function used by user to perform fetching command
exec :: String -> HttpConnection -> IO (Either C8.ByteString String)
exec cmd' conn@HttpConnection {httpManager = mng} = do
  let cmd = C8.pack cmd'
  url <- genURL conn ""
  req <- parseRequest url
  ans <-
    responseBody
      <$> httpLbs
        req{method = "POST",
            requestBody = RequestBodyLBS cmd
           }
        mng
  if ans /= ""
    then return $ Left ans -- error message
    else return $ Right "Created successfully"

ping :: (HttpConnection -> IO BS.ByteString)
ping = getData Ping

defaultHttpClient :: (MonadIO m) => m HttpConnection
defaultHttpClient = liftIO $ defaultHttpConnection

defaultHttpPool :: (MonadIO m) => Int -> NominalDiffTime -> Int -> m (Pool HttpConnection)
defaultHttpPool numStripes idleTime maxResources = liftIO $ createHttpPool def False numStripes idleTime maxResources

httpClient :: (MonadIO m) => String -> String -> Int -> String -> Bool -> m HttpConnection
httpClient user password port host needTls = liftIO $ httpConnect user password port host needTls

{-# INLINE runQuery #-}
runQuery :: (MonadIO m) => HttpConnection -> (HttpConnection -> IO a) -> m a
runQuery env runFn = liftIO $ runFn env
