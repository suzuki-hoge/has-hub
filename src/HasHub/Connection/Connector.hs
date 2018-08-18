{-# LANGUAGE OverloadedStrings #-}


module HasHub.Connection.Connector
(
  getGitHub
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

import Data.Aeson (ToJSON, encode)

import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)

import qualified HasHub.Connection.Config.LocalConfig as LC
import HasHub.Connection.Logger (logRequest, logResponse)
import HasHub.Connection.Config.Type


getGitHub :: (ToResource input) => input -> IO LBS.ByteString
getGitHub = secureGet LC.getGitHubHeaders LC.getGitHubEndpoint


getZenHub :: (ToResource input) => input -> IO LBS.ByteString
getZenHub = secureGet LC.getZenHubHeaders LC.getZenHubEndpoint


postGitHub :: (ToResource input, ToJSON input) => input -> IO LBS.ByteString
postGitHub = secureUpdate "POST" LC.getGitHubHeaders LC.getGitHubEndpoint


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
