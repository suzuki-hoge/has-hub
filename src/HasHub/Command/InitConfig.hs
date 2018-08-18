{-# LANGUAGE OverloadedStrings #-}


module HasHub.Command.InitConfig
(
  initialize
)
where


import Data.Maybe (fromMaybe)

import Control.Lens ((^?))
import Data.Aeson.Lens (key, _Integer)
import Data.Maybe (fromJust)

import HasHub.Connection.Connector (getGitHub)

import HasHub.Connection.Config.Detector
import HasHub.Connection.Config.LocalConfig

import Data.Either.Validation (Validation(..))


initialize :: Maybe Owner -> Maybe Repository -> Maybe Token -> Maybe Token -> Maybe FilePath -> Maybe Proxy -> IO (Validation [ConfigurationError] ())
initialize owner' repository' gitHubToken' zenHubToken' logPath' proxy' = do
  detected <- detectAll owner' repository' gitHubToken' zenHubToken' logPath' proxy'

  case detected of
    Success configs -> do
      setConfigs configs

      putStrLn "\nfetch RepositoryId."
      json <- getGitHub RepositoryIdInput
      setRepositoryId $ read . show . fromJust $ json ^? key "id" . _Integer

      return $ Success ()

    Failure fms -> return $ Failure fms


detectAll :: Maybe Owner -> Maybe Repository -> Maybe Token -> Maybe Token -> Maybe FilePath -> Maybe Proxy -> IO (Validation [ConfigurationError] Configs)
detectAll owner' repository' gitHubToken' zenHubToken' logPath' proxy' = do
  owner <- detectOwner owner'
  repository <- detectRepository repository'
  gitHubToken <- detectGitHubToken gitHubToken'
  zenHubToken <- detectZenHubToken zenHubToken'
  let logPath = fixLogPath logPath'
  proxy <- fixProxy proxy'

  putStrLn "\ndetect configs."
  putStrLn $ "  owner         : " ++ vString owner
  putStrLn $ "  repo          : " ++ vString repository
  putStrLn $ "  git-hub-token : " ++ (mask . vString) gitHubToken
  putStrLn $ "  zen-hub-token : " ++ (mask . vString) zenHubToken
  putStrLn $ "  log           : " ++ vString logPath
  putStrLn $ "  proxy         : " ++ vString (fromMaybe "" <$> proxy)

  return $ Configs <$> owner <*> repository <*> gitHubToken <*> zenHubToken <*> logPath <*> proxy


vString :: Validation [ConfigurationError] String -> String
vString (Success  x ) = x
vString (Failure [_]) = ""


mask :: String -> String
mask s = replicate (length s) '*'