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
    toString,
  )
where

import Control.Monad.Writer (WriterT (runWriterT))
import qualified Data.Aeson as JP
import Data.Attoparsec.ByteString (IResult (Done, Fail), parse)
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Vector (toList)
import Debug.Trace as DB
import Database.ClickHouseDriver.HTTP.Connection
  ( HttpConnection (HttpConnection, httpParams, httpTls),
  )
import Database.ClickHouseDriver.HTTP.Types (Cmd, HttpParams (..), JSONResult)
import Data.Bool (bool)
import Database.ClickHouseDriver.IO.BufferedWriter (writeIn)
import Database.ClickHouseDriver.Types
  ( ClickhouseType (CKArray, CKInt32, CKNull, CKString, CKTuple),
  )
import qualified Network.URI.Encode as NE

-- | Trim JSON data
extract :: C8.ByteString -> JSONResult
extract val = getData $ parse JP.json val
  where
    getData (Fail e _ _) = Left e
    getData (Done _ (JP.Object x)) = Right $ getData' x
    getData _ = Right []

    getData' = map getObject . maybeArrToList . HM.lookup (pack "data")

    maybeArrToList Nothing = []
    maybeArrToList (Just x) = toList . getArray $ x

    getArray (JP.Array arr) = arr
    getObject (JP.Object x) = x

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
    let res = DB.traceShowId $ basicUrl ++ NE.encode cmd ++ dbUrl db
    return res

-- | serialize column type into sql string
toString :: [ClickhouseType] -> String
toString ck = "(" ++ toStr ck ++ ")"

toStr :: [ClickhouseType] -> String
toStr [] = ""
toStr (x : []) = toStr' x
toStr (x : xs) = toStr' x ++ "," ++ toStr xs

toStr' :: ClickhouseType -> String
toStr' (CKInt32 n) = show n
toStr' (CKString str) = "'" ++ C8.unpack str ++ "'"
toStr' (CKArray arr) = "[" ++ (toStr $ toList arr) ++ "]"
toStr' (CKTuple arr) = "(" ++ (toStr $ toList arr) ++ ")"
toStr' CKNull = "null"
toStr' _ = error "unsupported writing type"

dbUrl :: (Maybe String) -> String
dbUrl = fromMaybe "" . fmap ("&database=" ++)
