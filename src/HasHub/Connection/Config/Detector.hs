module HasHub.Connection.Config.Detector
(
  detectOwner
, detectRepository
, detectGitHubToken
, detectZenHubToken
, fixConfig
, detectLogPath
, fixLogPath
, detectProxy
, ConfigurationError(..)
, module HasHub.Connection.Config.Type
, module HasHub.FixMe
)
where


import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath.Posix (takeDirectory, (</>))
import System.Environment (lookupEnv)
import Data.List (find)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Data.String.Utils (startswith)

import HasHub.Connection.Config.Type

import HasHub.FixMe (FixMe(..), Validation(..))


newtype ConfigurationError = ConfigurationError String deriving (Eq, Show)

instance FixMe ConfigurationError where
  toMessage (ConfigurationError s) = "configuration error: " ++ s


detectOwner :: Maybe Owner -> IO (Validation [ConfigurationError] Owner)
detectOwner input = do
  current <- getCurrentDirectory
  fixConfig input current "owner:"


detectRepository :: Maybe Repository -> IO (Validation [ConfigurationError] Repository)
detectRepository input = do
  current <- getCurrentDirectory
  fixConfig input current "repository:"


detectGitHubToken :: Maybe Token -> IO (Validation [ConfigurationError] Token)
detectGitHubToken input = do
  current <- getCurrentDirectory
  fixConfig input current "git-hub-token:"


detectZenHubToken :: Maybe Token -> IO (Validation [ConfigurationError] Token)
detectZenHubToken input = do
  current <- getCurrentDirectory
  fixConfig input current "zen-hub-token:"


detectLogPath :: Maybe FilePath -> IO (Validation [ConfigurationError] FilePath)
detectLogPath input = do
  current <- getCurrentDirectory
  fixLogPath input current "log-full-path:"


type Line = String

type Key = String


fixConfig :: Maybe String -> FilePath -> Key -> IO (Validation [ConfigurationError] String)
fixConfig input@(Just x) _         _   = return $ Success x
fixConfig Nothing        directory key = do
  found <- findByKeyOrUpper directory key
  return $ case found of
    Just x  -> Success x
    Nothing -> Failure [ConfigurationError $ init key ++ " not found in config"]


fixLogPath :: Maybe FilePath -> FilePath -> Key -> IO (Validation [ConfigurationError] FilePath)
fixLogPath input@(Just x) _         _   = toAbsolute x
fixLogPath Nothing        directory key = do
  found <- findByKeyOrUpper directory key
  case found of
    Just x  -> toAbsolute x
    Nothing -> return $ Failure [ConfigurationError $ init key ++ " not found in config"]


toAbsolute :: FilePath -> IO (Validation [ConfigurationError] FilePath)
toAbsolute fp = do
  home <- getHomeDirectory
  return $ Success $ replace "~" home fp


findByKeyOrUpper :: FilePath -> Key -> IO (Maybe String)
findByKeyOrUpper directory key = do
  let config = directory </> ".has-hub.conf"
  found <- findBy key <$> readLine config

  case (found, directory == "/" || directory == ".") of
    (Just x,  _)     -> return $ Just x
    (Nothing, True)  -> return Nothing
    (Nothing, False) -> findByKeyOrUpper (takeDirectory directory) key


readLine :: FilePath -> IO [Line]
readLine fp = do
  b <- doesFileExist fp
  if b
    then lines <$> readFile fp
    else return []


findBy :: Key -> [Line] -> Maybe String
findBy key lines = case find (startswith key) lines of
  Just line -> Just $ splitOn ":" line !! 1
  Nothing   -> Nothing


detectProxy :: IO (Maybe Proxy)
detectProxy = do
  p1 <- lookupEnv "https_proxy"
  p2 <- lookupEnv "HTTPS_PROXY"

  return $ case (p1, p2) of
    (Nothing, Nothing) -> Nothing
    (p,       Nothing) -> p
    (_      , p      ) -> p
