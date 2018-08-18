module HasHub.Command.EnvChecker where


import Data.Maybe (fromMaybe)

import HasHub.Command.Env

import HasHub.Connection.Type

import HasHub.FixMe (flat, Validation(..))


checkAll :: IO (Validation [ConfigurationError] ())
checkAll = do
  owner <- detectOwner Nothing
  repository <- detectRepository Nothing
  gitHubToken <- detectGitHubToken Nothing
  zenHubToken <- detectZenHubToken Nothing
  let lp = fixLogPath Nothing
  proxy <- fixProxy Nothing

  putStrLn "\ndetecting envs..."
  putStrLn $ "  owner         : " ++ vString owner
  putStrLn $ "  repo          : " ++ vString repository
  putStrLn $ "  git-hub-token : " ++ vString gitHubToken
  putStrLn $ "  zen-hub-token : " ++ vString zenHubToken
  putStrLn $ "  log           : " ++ lp
  putStrLn $ "  proxy         : " ++ fromMaybe "" proxy

  return $ flat [owner, repository, gitHubToken, zenHubToken]


vString :: Validation [ConfigurationError] String -> String
vString (Success  x ) = x
vString (Failure [_]) = ""
