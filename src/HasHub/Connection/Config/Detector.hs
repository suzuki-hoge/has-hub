module HasHub.Connection.Config.Detector
(
  detectOwner
, fixOwner
, detectRepository
, fixRepository
, detectGitHubToken
, detectZenHubToken
, fixToken
, fixLogPath
, fixProxy
, ConfigurationError(..)
, module HasHub.Connection.Config.Type
, module HasHub.FixMe
)
where


import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Environment (lookupEnv)
import Data.List (find, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.String.Utils (startswith)

import HasHub.Connection.Config.Type

import HasHub.FixMe (FixMe(..), Validation(..))


newtype ConfigurationError = ConfigurationError String deriving (Eq, Show)

instance FixMe ConfigurationError where
  toMessage (ConfigurationError s) = "configuration error: " ++ s


type Line = String


detectOwner :: Maybe Owner -> IO (Validation [ConfigurationError] Owner)
detectOwner input = getGitConfigPath >>= fixOwner input


fixOwner :: Maybe Owner -> FilePath -> IO (Validation [ConfigurationError] Repository)
fixOwner input@(Just x) _  = return $ Success x
fixOwner Nothing        fp = (>>> parseOwner) <$> getGitConfigLine fp
  where
    parseOwner :: Line -> Validation [ConfigurationError] Owner
    parseOwner line
      | "https://" `isInfixOf` line = Success $ trimDot $ splitOn "/" line !! 3
      | "git@"     `isInfixOf` line = let
        x = splitOn "/" line !! 0
        in Success $ trimDot $ splitOn ":" x !! 1
      | otherwise                   = Failure [ConfigurationError "invalid remote config."]


detectRepository :: Maybe Repository -> IO (Validation [ConfigurationError] Repository)
detectRepository input = getGitConfigPath >>= fixRepository input


fixRepository :: Maybe Repository -> FilePath -> IO (Validation [ConfigurationError] Repository)
fixRepository input@(Just x) _  = return $ Success x
fixRepository Nothing        fp = (>>> parseRepository) <$> getGitConfigLine fp
  where
    parseRepository :: Line -> Validation [ConfigurationError] Repository
    parseRepository line
      | "https://" `isInfixOf` line = Success $ trimDot $ splitOn "/" line !! 4
      | "git@"     `isInfixOf` line = Success $ trimDot $ splitOn "/" line !! 1
      | otherwise                   = Failure [ConfigurationError "invalid remote config."]


trimDot :: String -> String
trimDot s = "." `splitOn` s !! 0


getGitConfigPath :: IO FilePath
getGitConfigPath = (++ "/.git/config") <$> getCurrentDirectory


getGitConfigLine :: FilePath -> IO (Validation [ConfigurationError] Line)
getGitConfigLine fp = (>>> findUrlLine) <$> readLines fp
  where
    findUrlLine :: [Line] -> Validation [ConfigurationError] Line
    findUrlLine lines = case find (isInfixOf "url") lines of
      Just line -> Success line
      Nothing   -> Failure [ConfigurationError "remote config missing."]


getHasHubConfigPath :: IO FilePath
getHasHubConfigPath = (++ "/.has-hub.conf") <$> getHomeDirectory


detectGitHubToken :: Maybe Token -> IO (Validation [ConfigurationError] Token)
detectGitHubToken input = getHasHubConfigPath >>= fixToken input "git-hub-token:"


detectZenHubToken :: Maybe Token -> IO (Validation [ConfigurationError] Token)
detectZenHubToken input = getHasHubConfigPath >>= fixToken input "zen-hub-token:"


type Key = String

fixToken :: Maybe Token -> Key -> FilePath -> IO (Validation [ConfigurationError] Token)
fixToken input@(Just x) key fp = return $ Success x
fixToken Nothing        key fp = (>>> f key) <$> readLines fp
  where
    f :: Key -> [Line] -> Validation [ConfigurationError] Line
    f key lines = case find (startswith key) lines of
      Just line -> Success $ splitOn ":" line !! 1
      Nothing   -> Failure [ConfigurationError $ (init key) ++ " config missing."]


readLines :: FilePath -> IO (Validation [ConfigurationError] [Line])
readLines fp = do
  b <- doesFileExist fp
  if b
    then Success . lines <$> readFile fp
    else return $ Failure [ConfigurationError $ fp ++ " is not found."]


(>>>) :: Validation [ConfigurationError] a -> (a -> Validation [ConfigurationError] b) -> Validation [ConfigurationError] b
(>>>) (Success line)  f = f line
(>>>) (Failure error) f = Failure error


fixLogPath :: Maybe FilePath -> IO (Validation [ConfigurationError] FilePath)
fixLogPath (Just x) = return $ Success x
fixLogPath Nothing  = do
  home <- getHomeDirectory
  return $ Success $ home ++ "/has-hub.log"


fixProxy :: IO (Maybe Proxy)
fixProxy = do
  p1 <- lookupEnv "https_proxy"
  p2 <- lookupEnv "HTTPS_PROXY"

  return $ case (p1, p2) of
    (Nothing, Nothing) -> Nothing
    (p,       Nothing) -> p
    (_      , p      ) -> p

