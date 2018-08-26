{-# LANGUAGE OverloadedStrings #-}


module HasHub.Connection.Connector
(
  getGitHub
, getGitHub'
, getZenHub
, postGitHub
, postZenHub_
, postZenHub'_
, putZenHub_
)
where


import Network.HTTP.Client (parseRequest_, Request(..), RequestBody(..), newManager, responseBody, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (RequestHeaders, Method)

import Data.Aeson (ToJSON, encode, Value(Object), (.:), decode, object, (.=))
import Data.Aeson.Types (parseMaybe)

import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)

import qualified HasHub.Connection.Config.LocalConfig as LC
import HasHub.Connection.Logger (logRequest, logResponse)
import HasHub.Connection.Config.Type


getGitHub :: (QueryParser qp, PaginationQueryParser qp) => qp -> (LBS.ByteString -> IO [a]) -> IO [a]
getGitHub = secureV4RecursiveGet Nothing []


getGitHub' :: (QueryParser qp) => qp -> IO LBS.ByteString
getGitHub' = secureV4Get Nothing


secureV4RecursiveGet :: (QueryParser qp, PaginationQueryParser qp) => Cursor -> [a] -> qp -> (LBS.ByteString -> IO [a]) -> IO [a]
secureV4RecursiveGet currentCursor acc qp parse = do
  lbs <- secureV4Get currentCursor qp

  fetched <- parse lbs
  hasNext <- parseHasNext qp lbs
  let endCursor = parseEndCursor qp lbs

  if hasNext
    then secureV4RecursiveGet endCursor (acc ++ fetched) qp parse
    else return $ acc ++ fetched


secureV4Get :: (QueryParser qp) => Cursor -> qp -> IO LBS.ByteString
secureV4Get currentCursor qp = do
  headers <- LC.getGitHubV4Headers
  owner <- LC.getOwner
  repository <- LC.getRepository

  secureFetching (parseRequest_ "https://api.github.com/graphql") {
      method = "POST"
    , requestHeaders = headers
    , requestBody = RequestBodyLBS $ encode $ object [
        "query" .= toQueryPart qp owner repository currentCursor
      ]
  }


getZenHub :: (ToResource input) => input -> IO LBS.ByteString
getZenHub = secureGet LC.getZenHubHeaders LC.getZenHubEndpoint


postGitHub :: (ToResource input, ToJSON input) => input -> IO LBS.ByteString
postGitHub = secureUpdate "POST" LC.getGitHubV3Headers LC.getGitHubEndpoint


postZenHub_ :: (ToResource input, ToJSON input) => input -> IO ()
postZenHub_ = secureUpdate_ "POST" LC.getZenHubHeaders LC.getZenHubEndpoint


postZenHub'_ :: (ToResource input, ToJSON input) => (RepositoryId -> input) -> IO ()
postZenHub'_ inputF = do
  repositoryId <- LC.getRepositoryId
  let input = inputF repositoryId
  secureUpdate_ "POST" LC.getZenHubHeaders LC.getZenHubEndpoint input


putZenHub_ :: (ToResource input, ToJSON input) => input -> IO ()
putZenHub_ = secureUpdate_ "PUT" LC.getZenHubHeaders LC.getZenHubEndpoint


secureGet :: (ToResource input) => IO RequestHeaders -> IO Endpoint -> input -> IO LBS.ByteString
secureGet ioHeaders ioEndpoint input = do
  headers <- ioHeaders
  endpoint <- ioEndpoint

  secureFetching (parseRequest_ $ endpoint ++ toResource input) {
      method = "GET"
    , requestHeaders = headers
  }


secureUpdate :: (ToResource input, ToJSON input) => Method -> IO RequestHeaders -> IO Endpoint -> input -> IO LBS.ByteString
secureUpdate m ioHeaders ioEndpoint input = do
  headers <- ioHeaders
  endpoint <- ioEndpoint

  secureFetching (parseRequest_ $ endpoint ++ toResource input) {
      method = m
    , requestHeaders = headers
    , requestBody = RequestBodyLBS $ encode input
  }


secureUpdate_ :: (ToResource input, ToJSON input) => Method -> IO RequestHeaders -> IO Endpoint -> input -> IO ()
secureUpdate_ m ioHeaders ioEndpoint input = secureUpdate m ioHeaders ioEndpoint input >>= \_ -> return ()


secureFetching :: Request -> IO LBS.ByteString
secureFetching request = do
  manager <- newManager tlsManagerSettings

  logRequest request
  response <- responseBody <$> httpLbs request manager
  logResponse response

  return response
