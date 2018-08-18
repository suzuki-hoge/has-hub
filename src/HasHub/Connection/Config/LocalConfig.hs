{-# LANGUAGE OverloadedStrings #-}


module HasHub.Connection.Config.LocalConfig where


import Network.HTTP.Types (RequestHeaders)

import System.Environment (setEnv, getEnv, lookupEnv)

import qualified Data.ByteString.Char8 as BS (pack)

import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)

import Data.List.Split (splitOn)
import Data.List.Utils (replace)

import HasHub.Connection.Config.Type


setConfigs :: Configs -> IO ()
setConfigs (Configs owner repository gitHubToken zenHubToken logPath proxy) = do
  setEnv "has-hub.local-config.owner"         owner
  setEnv "has-hub.local-config.repository"    repository
  setEnv "has-hub.local-config.git-hub.token" gitHubToken
  setEnv "has-hub.local-config.zen-hub.token" zenHubToken
  setEnv "has-hub.local-config.log-path"      logPath
  case proxy of
    Just p -> setEnv "has-hub.local-config.proxy" p
    Nothing -> return ()

  utc <- getCurrentTime
  tz <- getCurrentTimeZone

  let allocated = replace ":" "-" . replace " " "-" . head . splitOn "." . show $ utcToLocalTime tz utc

  setEnv "has-hub.local-config.request-id" allocated


setRepositoryId :: RepositoryId -> IO ()
setRepositoryId n = setEnv "has-hub.local-config.repository-id" (show n)


getRequestId :: IO RequestId
getRequestId = getEnv "has-hub.local-config.request-id"


getRepositoryId :: IO RepositoryId
getRepositoryId = read <$> getEnv "has-hub.local-config.repository-id"


getGitHubEndpoint :: IO Endpoint
getGitHubEndpoint = do
  owner <- getEnv "has-hub.local-config.owner"
  repository <- getEnv "has-hub.local-config.repository"

  return $ "https://api.github.com/repos/" ++ owner ++ "/" ++ repository


getZenHubEndpoint :: IO Endpoint
getZenHubEndpoint = do
  rid <- getRepositoryId

  return $ "https://api.zenhub.io/p1/repositories/" ++ show rid


getGitHubHeaders :: IO RequestHeaders
getGitHubHeaders = do
  token <- getEnv "has-hub.local-config.git-hub.token"

  return [("User-Agent", "curl"), ("Authorization", BS.pack $ "token " ++ token)]


getZenHubHeaders :: IO RequestHeaders
getZenHubHeaders = do
  token <- getEnv "has-hub.local-config.zen-hub.token"

  return [("content-type", "application/json"), ("User-Agent", "curl"), ("X-Authentication-Token", BS.pack token)]


getLogPath :: IO FilePath
getLogPath = getEnv "has-hub.local-config.log-path"


getProxy :: IO (Maybe Proxy)
getProxy = lookupEnv "has-hub.local-config.proxy"
