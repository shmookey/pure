{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Util.Request
  ( Request(Request)
  , Response(Response)
  , Query
  , buildQuery
  , errorResponse
  , readRequest
  , withPath
  ) where

import GHC.Generics
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.CaseInsensitive as CI

import qualified Network.HTTP.Types.URI as URI
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Wai as Wai


type Query   = Map.Map String String
type Headers = Map.Map String String

data Response = Response { status :: String, message :: String } deriving (Generic, Show)
instance ToJSON Response

data Request = Request 
  { path    :: [String]
  , query   :: Query
  , body    :: BSL.ByteString
  , headers :: Headers
  } deriving (Show)


errorResponse :: String -> Response
errorResponse x = Response { status = "error", message = x }

withPath :: [String] -> Request -> Request
withPath xs req = req { path = xs }

buildQuery :: URI.Query -> Query
buildQuery =
  Map.fromList . map unpackPair . Maybe.catMaybes . map maybeBoth
  where 
    maybeBoth (k, Just v) = Just (k, v)
    maybeBoth _           = Nothing
    unpackPair (k, v)     = (BS.unpack k, BS.unpack v)

readPath :: [Text.Text] -> [String]
readPath = map Text.unpack

readHeaders :: Header.RequestHeaders -> Headers
readHeaders =
  Map.fromList . map (\(k, v) -> (BS.unpack $ CI.original k, BS.unpack v))

readRequest :: Wai.Request -> IO Request
readRequest req =
  do
    body <- Wai.strictRequestBody req
    return $ Request
      { path    = readPath    $ Wai.pathInfo req
      , query   = buildQuery  $ Wai.queryString req
      , headers = readHeaders $ Wai.requestHeaders req
      , body
      }

