{-# LANGUAGE OverloadedStrings #-}


module HasHub.Connection.Connector
(
  set
, getGitHub
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

import Control.Lens ((^?))
import Data.Aeson.Lens (key, _Integer)
import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)

import qualified HasHub.Connection.LocalSession as LS
import HasHub.Connection.Logger (logRequest, logResponse)
import HasHub.Connection.Type


set :: String -> String -> IO ()
set g z = do
  LS.allocateRequestId
  LS.setLogPath "has-hub.log"
  LS.setGitHubToken g
  LS.setZenHubToken z
  LS.setOwner "suzuki-hoge"
  LS.setRepository "has-hub-workspace"

  json <- getGitHub RepositoryIdInput
  LS.setRepositoryId $ read . show . fromJust $ json ^? key "id" . _Integer

  return ()


getGitHub :: (ToResource input) => input -> IO LBS.ByteString
getGitHub = secureGet LS.getGitHubHeaders LS.getGitHubEndpoint


getZenHub :: (ToResource input) => input -> IO LBS.ByteString
getZenHub = secureGet LS.getZenHubHeaders LS.getZenHubEndpoint


postGitHub :: (ToResource input, ToJSON input) => input -> IO LBS.ByteString
postGitHub = secureUpdate "POST" LS.getGitHubHeaders LS.getGitHubEndpoint


postZenHub_ :: (ToResource input, ToJSON input) => input -> IO ()
postZenHub_ = secureUpdate_ "POST" LS.getZenHubHeaders LS.getZenHubEndpoint


postZenHub'_ :: (ToResource input, ToJSON input) => (RepositoryId -> input) -> IO ()
postZenHub'_ inputF = do
  repositoryId <- LS.getRepositoryId
  let input = inputF repositoryId
  secureUpdate_ "POST" LS.getZenHubHeaders LS.getZenHubEndpoint input


putZenHub_ :: (ToResource input, ToJSON input) => input -> IO ()
putZenHub_ = secureUpdate_ "PUT" LS.getZenHubHeaders LS.getZenHubEndpoint


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
