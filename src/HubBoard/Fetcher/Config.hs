{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Fetcher.Config (
    initialize
  , getOwner
  , getRepository
  , getGitHubToken
  , getZenHubToken
  , getRepositoryId
  , Owner
  , Repository
  , RepositoryId
  , Token
  , ConfigError
) where

import           System.Directory            ( getCurrentDirectory )
import           System.Environment          ( setEnv, getEnv, lookupEnv )
import           Data.Yaml                   ( decodeFileEither, ParseException(..) )
import           Data.Maybe                  ( catMaybes, mapMaybe, fromMaybe )
import qualified Data.ByteString.Char8 as BS ( pack, unpack, ByteString )
import           Control.Applicative         ( (<|>) )
import           Control.Exception           ( SomeException, catch, evaluate )

import           HubBoard.Fetcher.Core

type Owner = String
type Repository = String
type RepositoryId = String
type Token = BS.ByteString
type HttpsProxy = String
type ConfigError = String

data ConfigYaml = ConfigYaml { gitHubToken :: Maybe String, zenHubToken :: Maybe String, owner :: Maybe String, repository :: Maybe String }

instance FromJSON ConfigYaml where
    parseJSON (Object v) = ConfigYaml <$> (v .:? "git-hub-token") <*> (v .:? "zen-hub-token") <*> (v .:? "owner") <*> (v .:? "repository")

initialize :: IO [ConfigError]
initialize = setEnvs >>= setRepositoryId
  where
    setEnvs :: IO [ConfigError]
    setEnvs = do
        putStrLn "\nload configs..."
        configs <- getConfigs

        e1 <- setGitHubToken configs
        e2 <- setZenHubToken configs
        e3 <- setOwner configs
        e4 <- setRepository configs

        return $ e1 ++ e2 ++ e3 ++ e4
          where
            getConfigs :: IO [ConfigYaml]
            getConfigs = catMaybes <$> (getCurrentDirectory >>= allUpperDirs >>= (mapM readConfig))
              where
                allUpperDirs :: FilePath -> IO [FilePath]
                allUpperDirs ""  = return []
                allUpperDirs dir = (dir :) <$> allUpperDirs (upper dir)
                  where
                    upper :: FilePath -> FilePath
                    upper = reverse . tail . dropWhile (/= '/') . reverse

                readConfig :: FilePath -> IO (Maybe ConfigYaml)
                readConfig dir = do
                    e <- (decodeFileEither (dir ++ "/.hub-board-config.yaml") >>= evaluate) `catch` failure

                    return $ case e of
                        Right xs -> Just xs
                        _        -> Nothing
                  where
                    failure :: SomeException -> IO (Either ParseException ConfigYaml)
                    failure e = return $ Left (InvalidYaml Nothing)

            setGitHubToken :: [ConfigYaml] -> IO [ConfigError]
            setGitHubToken configs = case mapMaybe gitHubToken configs of
                [] -> return ["git-hub-token not found."]
                (x:_) -> setEnv "hub-board.git-hub-token" x >>= return . const []

            setZenHubToken :: [ConfigYaml] -> IO [ConfigError]
            setZenHubToken configs = case mapMaybe zenHubToken configs of
                [] -> return ["zen-hub-token not found."]
                (x:_) -> setEnv "hub-board.zen-hub-token" x >>= return . const []

            setOwner :: [ConfigYaml] -> IO [ConfigError]
            setOwner configs = case mapMaybe owner configs of
                []    -> return ["owner not found."]
                (x:_) -> setEnv "hub-board.owner" x >>= return . const []

            setRepository :: [ConfigYaml] -> IO [ConfigError]
            setRepository configs = case mapMaybe repository configs of
                []    -> return ["repository not found."]
                (x:_) -> setEnv "hub-board.repository" x >>= return . const []

    setRepositoryId :: [ConfigError] -> IO [ConfigError]
    setRepositoryId errors@(e:es) = return errors
    setRepositoryId [] = do
        putStrLn "  refer RepositoryId"

        gToken <- getGitHubToken
        zToken <- getZenHubToken
        owner <- getOwner
        repository <- getRepository
        proxy <- getProxy

        putStrLn $ printf "    GitHubToken ( %s )" (mask gToken)
        putStrLn $ printf "    ZenHubToken ( %s )" (mask zToken)
        putStrLn $ printf "    Owner       ( %s )" owner
        putStrLn $ printf "    Repository  ( %s )" repository
        putStrLn $ printf "    HttpsProxy  ( %s )" (fromMaybe "" proxy)

        let headers = [("User-Agent", "curl"), ("Authorization", gToken)]
        let query = printf "{ repository( owner:\"%s\", name:\"%s\" ) { databaseId } }" owner repository :: String
        let body = object ["query" .= query]

        show . parse <$> secureFetch "https://api.github.com/graphql" "POST" headers body >>= setEnv "hub-board.repository-id"

        return []
          where
            parse :: ByteString -> Int
            parse = fromJust . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: "databaseId"))

getGitHubToken :: IO Token
getGitHubToken = BS.pack . ("token " ++) <$> getEnv "hub-board.git-hub-token"

getZenHubToken :: IO Token
getZenHubToken = BS.pack <$> getEnv "hub-board.zen-hub-token"

getOwner :: IO Owner
getOwner = getEnv "hub-board.owner"

getRepository :: IO Repository
getRepository = getEnv "hub-board.repository"

getProxy :: IO (Maybe HttpsProxy)
getProxy = do
    p1 <- lookupEnv "https_proxy"
    p2 <- lookupEnv "HTTPS_PROXY"

    return $ p1 <|> p2

getRepositoryId :: IO RepositoryId
getRepositoryId = getEnv "hub-board.repository-id"

mask :: BS.ByteString -> String
mask token = head ++ map (const '.') remains' ++ reverse tail'
  where
    (head, remains) = splitAt 2 (BS.unpack token)
    (tail', remains') = splitAt 2 $ reverse remains
