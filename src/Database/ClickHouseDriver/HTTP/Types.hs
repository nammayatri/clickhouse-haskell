-- | Definition of types
module Database.ClickHouseDriver.HTTP.Types
  ( JSONResult (..),
    Cmd,
    Format (..),
    HttpConnection (..),
    HttpParams (..),
  )
where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Network.HTTP.Client (Manager)

type JSONResult = Either ByteString [HashMap Text Value]

type Cmd = String

data Format = CSV | JSON | TUPLE
  deriving (Eq)

data HttpParams = HttpParams
  { httpHost :: !String,
    httpPort :: {-# UNPACK #-} !Int,
    httpUsername :: !String,
    httpPassword :: !String,
    httpDatabase :: Maybe String
  }

data HttpConnection = HttpConnection
  { -- | basic parameters
    httpParams :: !HttpParams,
    -- | http manager
    httpManager :: !Manager,
    httpTls :: Bool
  }
