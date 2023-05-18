-- Copyright (c) 2020-present, EMQX, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a MIT license,
-- found in the LICENSE file.
{-# LANGUAGE FlexibleInstances #-}

-- | Miscellaneous helper functions. User should not import it.
module Database.ClickHouseDriver.HTTP.Helpers
  ( extract,
    genURL,
  )
where

import Control.Monad.Writer (WriterT (runWriterT), tell)
import qualified Data.Aeson as JP
import Data.Attoparsec.ByteString (IResult (Done, Fail), parse)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Vector (toList)
import Database.ClickHouseDriver.HTTP.Connection
  ( HttpConnection (HttpConnection, httpParams, httpTls),
  )
import Database.ClickHouseDriver.HTTP.Types (Cmd, HttpParams (..), JSONResult)
import qualified Network.URI.Encode as NE

-- | Trim JSON data
extract :: (JP.FromJSON a) => C8.ByteString -> Either String a
extract val = do
  either (Left . C8.unpack) eitherFromJSON $ getData (parse JP.json val)
  where
    getData (Fail e _ _) = Left e
    getData (Done _ (JP.Object x)) = Right $ getData' x
    getData _ = Right []

    getData' = map getObject . maybeArrToList . HM.lookup (pack "data")

    maybeArrToList Nothing = []
    maybeArrToList (Just x) = toList . getArray $ x

    getArray (JP.Array arr) = arr
    getObject (JP.Object x) = x

    eitherFromJSON val =
      case JP.fromJSON $ JP.toJSON val of
        JP.Success a -> Right a
        JP.Error err -> Left err

genURL :: HttpConnection -> Cmd -> IO String
genURL
  HttpConnection
    { httpParams =
        HttpParams
          { httpHost = host,
            httpPassword = pw,
            httpPort = port,
            httpUsername = usr,
            httpDatabase = db
          },
      httpTls = needTls
    }
  cmd = do
    (_, basicUrl) <- runWriterT $ do
      writeIn (bool "http://" "https://" needTls)
      writeIn usr
      writeIn ":"
      writeIn pw
      writeIn "@"
      writeIn host
      writeIn ":"
      writeIn $ show port
      writeIn "/"
      if cmd == "ping" then return () else writeIn "?query="
    let res = basicUrl ++ NE.encode cmd ++ dbUrl db
    return res

dbUrl :: (Maybe String) -> String
dbUrl = fromMaybe "" . fmap ("&database=" ++)

writeIn = tell
