{-# LANGUAGE OverloadedStrings #-}


module HasHub.Connection.LocalSession where


import System.Environment (setEnv, getEnv)

import Data.List.Split (splitOn)
import Data.List.Utils (replace)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)

import qualified Data.ByteString.Char8 as BS (pack)

import Network.HTTP.Types (RequestHeaders)

import HasHub.Connection.Type


allocateRequestId :: IO ()
allocateRequestId = do
  utc <- getCurrentTime
  tz <- getCurrentTimeZone

  let allocated = (replace ":" "-") . (replace " " "-") . head . (splitOn ".") . show $ utcToLocalTime tz utc

  setEnv "has-hub.local-session.request-id" allocated


setLogPath :: FilePath -> IO ()
setLogPath = setEnv "has-hub.local-session.log-path"


setOwner :: Owner -> IO ()
setOwner = setEnv "has-hub.local-session.owner"


setRepository :: Repository -> IO ()
setRepository = setEnv "has-hub.local-session.repository"


setRepositoryId :: RepositoryId -> IO ()
setRepositoryId n = setEnv "has-hub.local-session.repository-id" (show n)


setGitHubToken :: Token -> IO ()
setGitHubToken = setEnv "has-hub.local-session.git-hub.token"


setZenHubToken :: Token -> IO ()
setZenHubToken = setEnv "has-hub.local-session.zen-hub.token"


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


getGitHubHeaders :: IO RequestHeaders
getGitHubHeaders = do
  token <- getEnv "has-hub.local-session.git-hub.token"

  return [("User-Agent", "curl"), ("Authorization", BS.pack $ "token " ++ token)]


getZenHubEndpoint :: IO Endpoint
getZenHubEndpoint = do
  rid <- getRepositoryId

  return $ "https://api.zenhub.io/p1/repositories/" ++ show rid


getZenHubHeaders :: IO RequestHeaders
getZenHubHeaders = do
  token <- getEnv "has-hub.local-session.zen-hub.token"

  return [("content-type", "application/json"), ("User-Agent", "curl"), ("X-Authentication-Token", BS.pack token)]
