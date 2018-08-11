{-# LANGUAGE OverloadedStrings #-}


module HasHub.Connection.Connector
(
  set
, getGitHub
, getZenHub
, postGitHub
, postGitHub'_
, postZenHub
, putZenHub'
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
import HasHub.Connection.LocalSession
import HasHub.Connection.Type


set :: String -> String -> IO ()
set g z = do
  LS.allocateRequestId
  LS.setLogPath "has-hub.log"
  LS.setGitHubToken g
  LS.setZenHubToken z
  LS.setOwner "suzuki-hoge"
  LS.setRepository "has-hub-workspace"

  json <- getGitHub ""
  LS.setRepositoryId $ read . show . fromJust $ json ^? key "id" . _Integer

  return ()


getGitHub :: Resource -> IO LBS.ByteString
getGitHub = secureGet LS.getGitHubHeaders LS.getGitHubEndpoint


getZenHub :: Resource -> IO LBS.ByteString
getZenHub = secureGet LS.getZenHubHeaders LS.getZenHubEndpoint


postGitHub :: (ToJSON body) => Resource -> body -> IO LBS.ByteString
postGitHub = secureUpdate "POST" LS.getGitHubHeaders LS.getGitHubEndpoint


postGitHub'_ :: (ToJSON body) => Resource -> (RepositoryId -> body) -> IO ()
postGitHub'_ resource bodyF = do
  repositoryId <- getRepositoryId
  let body = bodyF repositoryId
  secureUpdate_ "POST" LS.getGitHubHeaders LS.getGitHubEndpoint resource body


postZenHub :: (ToJSON body) => Resource -> body -> IO ()
postZenHub = secureUpdate_ "POST" LS.getZenHubHeaders LS.getZenHubEndpoint


putZenHub' :: (ToJSON body) => Resource -> body -> IO ()
putZenHub' = secureUpdate_ "PUT" LS.getZenHubHeaders LS.getZenHubEndpoint


secureGet :: IO RequestHeaders -> IO String -> Resource -> IO LBS.ByteString
secureGet ioHeaders ioEndpoint resource = do
  headers <- ioHeaders
  endpoint <- ioEndpoint

  secureFetching (parseRequest_ $ endpoint ++ resource) {
      method = "GET"
    , requestHeaders = headers
  }


secureUpdate :: (ToJSON body) => Method -> IO RequestHeaders -> IO String -> Resource -> body -> IO LBS.ByteString
secureUpdate m ioHeaders ioEndpoint resource body = do
  headers <- ioHeaders
  endpoint <- ioEndpoint

  secureFetching (parseRequest_ $ endpoint ++ resource) {
      method = m
    , requestHeaders = headers
    , requestBody = RequestBodyLBS $ encode body
  }


secureUpdate_ :: (ToJSON body) => Method -> IO RequestHeaders -> IO String -> Resource -> body -> IO ()
secureUpdate_ m ioHeaders ioEndpoint resource body = secureUpdate m ioHeaders ioEndpoint resource body >>= \_ -> return ()


secureFetching :: Request -> IO LBS.ByteString
secureFetching request = do
  manager <- newManager tlsManagerSettings

  logRequest request
  response <- responseBody <$> httpLbs request manager
  logResponse response

  return response
