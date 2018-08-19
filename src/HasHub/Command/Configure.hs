{-# LANGUAGE OverloadedStrings #-}


module HasHub.Command.Configure
(
  initialize
, deinitialize
, ConfigurationError(..)
)
where


import Data.Maybe (fromMaybe)

import Control.Lens ((^?))
import Data.Aeson.Lens (key, _Integer)

import HasHub.Connection.Connector (getGitHub)

import HasHub.Connection.Config.Detector
import HasHub.Connection.Config.LocalConfig

import HasHub.FixMe


initialize :: Maybe Owner -> Maybe Repository -> Maybe Token -> Maybe Token -> Maybe FilePath -> IO (Validation [ConfigurationError] ())
initialize owner' repository' gitHubToken' zenHubToken' logPath' = do
  detected <- detectAll owner' repository' gitHubToken' zenHubToken' logPath'

  case detected of
    Success configs -> do
      setConfigs configs

      putStrLn "\nfetch RepositoryId."
      repositoryId <- asJust =<< (\json -> json ^? key "id" . _Integer) <$> getGitHub RepositoryIdInput
      setRepositoryId $ (read . show) repositoryId

      return $ Success ()

    Failure fms -> return $ Failure fms


detectAll :: Maybe Owner -> Maybe Repository -> Maybe Token -> Maybe Token -> Maybe FilePath -> IO (Validation [ConfigurationError] Configs)
detectAll owner' repository' gitHubToken' zenHubToken' logPath' = do
  owner <- detectOwner owner'
  repository <- detectRepository repository'
  gitHubToken <- detectGitHubToken gitHubToken'
  zenHubToken <- detectZenHubToken zenHubToken'
  let logPath = fixLogPath logPath'
  proxy <- fixProxy

  putStrLn "\ndetect configs."
  putStrLn $ "  owner         : " ++ vString owner
  putStrLn $ "  repo          : " ++ vString repository
  putStrLn $ "  git-hub-token : " ++ (mask . vString) gitHubToken
  putStrLn $ "  zen-hub-token : " ++ (mask . vString) zenHubToken
  putStrLn $ "  log           : " ++ vString logPath
  putStrLn $ "  proxy         : " ++ fromMaybe "" proxy

  return $ Configs <$> owner <*> repository <*> gitHubToken <*> zenHubToken <*> logPath


vString :: Validation [ConfigurationError] String -> String
vString (Success  x ) = x
vString (Failure [_]) = ""


mask :: String -> String
mask s = replicate (length s) '*'


deinitialize :: IO ()
deinitialize = unsetAll
