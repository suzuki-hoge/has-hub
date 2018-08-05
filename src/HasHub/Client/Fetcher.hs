{-# LANGUAGE OverloadedStrings #-}


module HasHub.Client.Fetcher
(
  Fetcher(..)
, secureFetching
, requestToString
, responseToString
)
where


import qualified Network.HTTP.Client as C
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS (unpack)
import Data.ByteString.Lazy.Char8 as LBS (ByteString, unpack)

import Data.List (intersperse)
import Data.String.Utils (strip)
import Codec.Binary.UTF8.String (decodeString)

import HasHub.Logger


data Fetcher = Fetcher (C.Request -> IO LBS.ByteString)
instance Show Fetcher where
  show _ = "fetcher"


secureFetching :: String -> FilePath -> Fetcher
secureFetching cid logPath = Fetcher $ \request -> do
  manager <- C.newManager tlsManagerSettings

  logging cid logPath (requestToString request)
  response <- C.responseBody <$> C.httpLbs request manager
  logging cid logPath (responseToString response)

  return response


requestToString :: C.Request -> String
requestToString request = parser url request
  where
    method = BS.unpack $ C.method request
    parser = if method == "GET" then get else post method
    host = BS.unpack $ C.host request
    path = BS.unpack $ C.path request
    url = "https://" ++ host ++ path


get :: String -> C.Request -> String
get url request = "[GET] " ++ url ++ parameters
  where
    query = BS.unpack $ C.queryString request
    parameters = if query == "" then "" else "?" ++ query


post :: String -> String -> C.Request -> String
post method url request = "[" ++ method ++ "] " ++ url ++ " " ++ parameters
  where
    (C.RequestBodyLBS body) = C.requestBody request
    parameters = if body == "" then "{}" else decodeString $ LBS.unpack body


responseToString :: LBS.ByteString -> String
responseToString = flat . LBS.unpack
  where
    flat :: String -> String
    flat "" = "{}"
    flat s = (foldl1 (++) . intersperse " " . map strip . lines . decodeString) s
