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
, asRepositoryId
) where


import Network.HTTP.Client (parseRequest_, Request(..), RequestBody(..))
import Network.HTTP.Types (renderSimpleQuery, Method)

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS (pack)

import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)

import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.List.Utils (replace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)

import Text.Printf (printf)

import Control.Lens ((^?))
import Data.Aeson.Lens (key, _Integer)
import Data.Aeson (ToJSON, encode)

import HasHub.Client.Data
import HasHub.Client.Fetcher


getClient :: GitHubToken -> Owner -> RepositoryName -> ZenHubToken -> FilePath -> IO Client
getClient gToken owner repoName zToken logPath = do
  cid <- getClientId

  printf "\ninitialize Client(%s)\n" cid

  let fetcher = secureFetching cid logPath

  let gHeaders = [("User-Agent", "curl"), ("Authorization", BS.pack . ("token " ++) $ gToken)]
  let gEndpoint = ("https://api.github.com/repos/" ++) . (owner ++) . ("/" ++) $ repoName

  let zHeaders = [("content-type", "application/json"), ("User-Agent", "curl"), ("X-Authentication-Token", BS.pack zToken)]
  rid <- getRepositoryId fetcher gEndpoint gHeaders
  let zEndpoint = "https://api.zenhub.io/p1/repositories/" ++ show rid

  return $ Client fetcher (GitHubConnector gHeaders gEndpoint) (ZenHubConnector zHeaders zEndpoint rid)


getClientId :: IO String
getClientId = do
  utc <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ parse $ utcToLocalTime tz utc

    where
      parse = (replace "-" "") . (replace ":" "") . (replace " " "") . head . (splitOn ".") . show


type Resource = String
type Queries = [(BS.ByteString, BS.ByteString)]


getRepositoryId :: Fetcher -> GitHubEndpoint -> GitHubHeaders -> IO RepositoryId
getRepositoryId (Fetcher fetcher) gEndpoint gHeader = do
  putStrLn "  fetch RepositoryId"

  let request = (parseRequest_ $ gEndpoint) {
      method = "GET"
    , requestHeaders = gHeader
  }
  json <- fetcher request
  return . read . show . fromJust $ json ^? key "id" . _Integer


getGitHub :: Client -> Resource -> Queries -> IO LBS.ByteString
getGitHub (Client (Fetcher fetcher) (GitHubConnector gHeader gEndpoint) _) resource queries = do
  let request = (parseRequest_ $ gEndpoint ++ resource) {
      method = "GET"
    , requestHeaders = gHeader
    , queryString = renderSimpleQuery False queries
  }
  fetcher request


postGitHub :: (ToJSON body) => Client -> Resource -> body -> IO LBS.ByteString
postGitHub(Client (Fetcher fetcher) (GitHubConnector gHeader gEndpoint) _) resource body = do
  let request = (parseRequest_ $ gEndpoint ++ resource) {
      method = "POST"
    , requestHeaders = gHeader
    , requestBody = RequestBodyLBS $ encode body
  }
  fetcher request


getZenHub :: Client -> Resource -> IO LBS.ByteString
getZenHub (Client (Fetcher fetcher) _ (ZenHubConnector zHeader zEndpoint _)) resource = do
  let request = (parseRequest_ $ zEndpoint ++ resource) {
      method = "GET"
    , requestHeaders = zHeader
  }
  fetcher request


postZenHub :: (ToJSON body) => Client -> Resource -> body -> IO LBS.ByteString
postZenHub = updateZenHub "POST"


putZenHub :: (ToJSON body) => Client -> Resource -> body -> IO LBS.ByteString
putZenHub = updateZenHub "PUT"


updateZenHub :: (ToJSON body) => Method -> Client -> Resource -> body -> IO LBS.ByteString
updateZenHub method (Client (Fetcher fetcher) _ (ZenHubConnector zHeader zEndpoint _)) resource body = do
  let request = (parseRequest_ $ zEndpoint ++ resource) {
      method = method
    , requestHeaders = zHeader
    , requestBody = RequestBodyLBS $ encode body
  }
  fetcher request
