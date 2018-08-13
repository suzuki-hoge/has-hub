{-# LANGUAGE OverloadedStrings #-}


module HasHub.Connection.LocalSession where


import Network.HTTP.Types (RequestHeaders)

import System.Environment (setEnv, getEnv)

import qualified Data.ByteString.Char8 as BS (pack)

import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)

import Data.List.Split (splitOn)
import Data.List.Utils (replace)

import HasHub.Connection.Type


allocateRequestId :: IO ()
allocateRequestId = do
  utc <- getCurrentTime
  tz <- getCurrentTimeZone

  let allocated = replace ":" "-" . replace " " "-" . head . splitOn "." . show $ utcToLocalTime tz utc

  setEnv "has-hub.local-session.request-id" allocated


setLogPath :: FilePath -> IO ()
setLogPath = setEnv "has-hub.local-session.log-path"


setGitHubToken :: Token -> IO ()
setGitHubToken = setEnv "has-hub.local-session.git-hub.token"


setZenHubToken :: Token -> IO ()
setZenHubToken = setEnv "has-hub.local-session.zen-hub.token"


setOwner :: Owner -> IO ()
setOwner = setEnv "has-hub.local-session.owner"


setRepository :: Repository -> IO ()
setRepository = setEnv "has-hub.local-session.repository"


setRepositoryId :: RepositoryId -> IO ()
setRepositoryId n = setEnv "has-hub.local-session.repository-id" (show n)


getRequestId :: IO RequestId
getRequestId = getEnv "has-hub.local-session.request-id"


getLogPath :: IO FilePath
getLogPath = getEnv "has-hub.local-session.log-path"


getRepositoryId :: IO RepositoryId
getRepositoryId = read <$> getEnv "has-hub.local-session.repository-id"


getGitHubEndpoint :: IO Endpoint
getGitHubEndpoint = do
  owner <- getEnv "has-hub.local-session.owner"
  repository <- getEnv "has-hub.local-session.repository"

  return $ "https://api.github.com/repos/" ++ owner ++ "/" ++ repository


getZenHubEndpoint :: IO Endpoint
getZenHubEndpoint = do
  rid <- getRepositoryId

  return $ "https://api.zenhub.io/p1/repositories/" ++ show rid


getGitHubHeaders :: IO RequestHeaders
getGitHubHeaders = do
  token <- getEnv "has-hub.local-session.git-hub.token"

  return [("User-Agent", "curl"), ("Authorization", BS.pack $ "token " ++ token)]


getZenHubHeaders :: IO RequestHeaders
getZenHubHeaders = do
  token <- getEnv "has-hub.local-session.zen-hub.token"

  return [("content-type", "application/json"), ("User-Agent", "curl"), ("X-Authentication-Token", BS.pack token)]
