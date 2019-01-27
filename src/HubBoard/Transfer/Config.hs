{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.Config
    ( validate
    , getGitHubToken
    , getZenHubToken
    , getOwner
    , getRepository
    )
where

import           System.Directory
import           HubBoard.Transfer.Core.Type

import           Data.String.Utils              ( startswith )

import           Data.Yaml                      ( decodeFileEither
                                                , ParseException(..)
                                                , YamlException(..)
                                                )

import           Data.Aeson              hiding ( Success )
import           Data.Maybe
import           Data.Either.Validation
import           Control.Exception

validate :: IO (Validation [String] ())
validate = ap <$> getConfigs
  where
    ap :: [ConfigYaml] -> Validation [String] ()
    ap configs =
        (\_ _ _ _ -> ())
            <$> getGitHubToken' configs
            <*> getZenHubToken' configs
            <*> getOwner' configs
            <*> getRepository' configs

getConfigs :: IO [ConfigYaml]
getConfigs =
    catMaybes <$> (getCurrentDirectory >>= allUpperDirs >>= (mapM readConfig))
  where
    allUpperDirs :: FilePath -> IO [FilePath]
    allUpperDirs ""  = return []
    allUpperDirs dir = (dir :) <$> allUpperDirs (upper dir)
      where
        upper :: FilePath -> FilePath
        upper = reverse . tail . dropWhile (/= '/') . reverse
    readConfig :: FilePath -> IO (Maybe ConfigYaml)
    readConfig dir = do
        e <-
            (decodeFileEither (dir ++ "/.hub-board-config.yaml") >>= evaluate)
                `catch` failure

        return $ case e of
            Right xs -> Just xs
            _        -> Nothing

      where
        failure :: SomeException -> IO (Either ParseException ConfigYaml)
        failure e = return $ Left (InvalidYaml Nothing)

data ConfigYaml = ConfigYaml (Maybe Token) (Maybe Token) (Maybe Owner) (Maybe Repository) deriving Show

instance FromJSON ConfigYaml where
    parseJSON (Object v) =
        ConfigYaml
            <$> (v .:? "git-hub-token")
            <*> (v .:? "zen-hub-token")
            <*> (v .:? "owner")
            <*> (v .:? "repository")

getGitHubToken :: IO Token
getGitHubToken = do
    (Success x) <- getGitHubToken' <$> getConfigs
    return x

getZenHubToken :: IO Token
getZenHubToken = do
    (Success x) <- getZenHubToken' <$> getConfigs
    return x

getOwner :: IO Owner
getOwner = do
    (Success x) <- getOwner' <$> getConfigs
    return x

getRepository :: IO Repository
getRepository = do
    (Success x) <- getRepository' <$> getConfigs
    return x

getGitHubToken' :: [ConfigYaml] -> Validation [String] Token
getGitHubToken' configs = case mapMaybe (\(ConfigYaml x _ _ _) -> x) configs of
    [] -> Failure ["git-hub-token not found."]
    xs -> Success (xs !! 0)

getZenHubToken' :: [ConfigYaml] -> Validation [String] Token
getZenHubToken' configs = case mapMaybe (\(ConfigYaml _ x _ _) -> x) configs of
    [] -> Failure ["zen-hub-token not found."]
    xs -> Success (xs !! 0)

getOwner' :: [ConfigYaml] -> Validation [String] Owner
getOwner' configs = case mapMaybe (\(ConfigYaml _ _ x _) -> x) configs of
    [] -> Failure ["owner not found."]
    xs -> Success (xs !! 0)

getRepository' :: [ConfigYaml] -> Validation [String] Repository
getRepository' configs = case mapMaybe (\(ConfigYaml _ _ _ x) -> x) configs of
    [] -> Failure ["repository not found."]
    xs -> Success (xs !! 0)
