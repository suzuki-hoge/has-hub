{-# LANGUAGE OverloadedStrings #-}


module HasHub.Client
(
  Resource
, getClient
, getGitHub
, postGitHub
, getZenHub
, postZenHub
, putZenHub
, HasHub.Client.Data.Client(..)
) where


import Network.HTTP.Client (parseRequest_, Request(..), RequestBody(..))
import Network.HTTP.Types (renderSimpleQuery, Method)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import qualified Data.ByteString.Lazy.Internal as L (ByteString)

import Data.Maybe (fromJust)

import Control.Lens ((^?))
import Data.Aeson.Lens (key, _Integer)
import Data.Aeson (ToJSON, encode)

import HasHub.Client.Data
import HasHub.Client.Fetcher


getClient :: GitHubToken -> Owner -> RepositoryName -> ZenHubToken -> IO Client
getClient gToken owner repoName zToken = do
  putStrLn "\ninitialize"

  let gHeaders = [("User-Agent", "curl"), ("Authorization", pack . ("token " ++) $ gToken)]
  let gEndpoint = ("https://api.github.com/repos/" ++) . (owner ++) . ("/" ++) $ repoName

  let zHeaders = [("content-type", "application/json"), ("User-Agent", "curl"), ("X-Authentication-Token", pack zToken)]
  rid <- getRepositoryId fetcher gEndpoint gHeaders
  let zEndpoint = "https://api.zenhub.io/p1/repositories/" ++ show rid

  return $ Client fetcher (GitHubConnector gHeaders gEndpoint) (ZenHubConnector zHeaders zEndpoint rid)


type Resource = String
type Queries = [(ByteString, ByteString)]


getRepositoryId :: Fetcher -> GitHubEndpoint -> GitHubHeaders -> IO RepositoryId
getRepositoryId (Fetcher fetcher) gEndpoint gHeader = do
  putStrLn "  fetch RepositoryId"

  let request = (parseRequest_ $ gEndpoint) {
      method = "GET"
    , requestHeaders = gHeader
  }
  json <- fetcher request
  return . read . show . fromJust $ json ^? key "id" . _Integer


getGitHub :: Client -> Resource -> Queries -> IO L.ByteString
getGitHub (Client (Fetcher fetcher) (GitHubConnector gHeader gEndpoint) _) resource queries = do
  let request = (parseRequest_ $ gEndpoint ++ resource) {
      method = "GET"
    , requestHeaders = gHeader
    , queryString = renderSimpleQuery False queries
  }
  fetcher request


postGitHub :: (ToJSON body) => Client -> Resource -> body -> IO L.ByteString
postGitHub (Client (Fetcher fetcher) (GitHubConnector gHeader gEndpoint) _) resource body = do
  let request = (parseRequest_ $ gEndpoint ++ resource) {
      method = "POST"
    , requestHeaders = gHeader
    , requestBody = RequestBodyLBS $ encode body
  }
  fetcher request


getZenHub :: Client -> Resource -> IO L.ByteString
getZenHub (Client (Fetcher fetcher) _ (ZenHubConnector zHeader zEndpoint _)) resource = do
  let request = (parseRequest_ $ zEndpoint ++ resource) {
      method = "GET"
    , requestHeaders = zHeader
  }
  fetcher request


postZenHub :: (ToJSON body) => Client -> Resource -> body -> IO L.ByteString
postZenHub = updateZenHub "POST"


putZenHub :: (ToJSON body) => Client -> Resource -> body -> IO L.ByteString
putZenHub = updateZenHub "PUT"


updateZenHub :: (ToJSON body) => Method -> Client -> Resource -> body -> IO L.ByteString
updateZenHub method (Client (Fetcher fetcher) _ (ZenHubConnector zHeader zEndpoint _)) resource body = do
  let request = (parseRequest_ $ zEndpoint ++ resource) {
      method = method
    , requestHeaders = zHeader
    , requestBody = RequestBodyLBS $ encode body
  }
  fetcher request
