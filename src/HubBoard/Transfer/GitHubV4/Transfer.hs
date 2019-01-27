{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.GitHubV4.Transfer where -- todo out and local where

import           Network.HTTP.Client
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as BS
                                                ( pack )
import qualified Data.ByteString.Lazy.Internal as LBS

import           HubBoard.Transfer.GitHubV4.Type
import           HubBoard.Transfer.Config
import           Network.HTTP.Types
import           HubBoard.Transfer.Core.Transfer
import           Data.Maybe
import           Text.Printf                    ( printf )

getGitHub :: Mapper a -> IO [a]
getGitHub = getGitHubV4 Nothing []

  where
    getGitHubV4 :: Cursor -> [a] -> Mapper a -> IO [a]
    getGitHubV4 cursor acc mapper@(Mapper toQuery asHasNext asCursor parse) =
        do
            lbs <- fetchLbs toQuery cursor

            let accumulated = acc ++ (parse lbs)

            if asHasNext lbs
                then getGitHubV4 (asCursor lbs) accumulated mapper
                else return accumulated



getRepositoryId :: IO RepositoryId
getRepositoryId = (show . asInt) <$> fetchLbs toQuery Nothing
  where
    toQuery :: Owner -> Repository -> Cursor -> String
    toQuery owner repository cursor = printf
        "query { repository(owner:\"%s\", name:\"%s\") { databaseId } }"
        owner
        repository

    asInt :: LBS.ByteString -> Int
    asInt lbs =
        fromJust
            $   decode lbs
            >>= parseMaybe (.: "data")
            >>= parseMaybe (.: "repository")
            >>= parseMaybe (.: "databaseId")

fetchLbs :: (Owner -> Repository -> Cursor -> String) -> Cursor -> IO LBS.ByteString
fetchLbs toQuery cursor = do
    headers    <- getHeaders
    owner      <- getOwner
    repository <- getRepository

    secureFetch (parseRequest_ "https://api.github.com/graphql")
        { method         = "POST"
        , requestHeaders = headers
        , requestBody    = RequestBodyLBS $ encode $ object
                               ["query" .= toQuery owner repository cursor]
        }

  where
    getHeaders :: IO RequestHeaders
    getHeaders = do
        token <- getGitHubToken
        return
            [ ("content-type" , "application/json")
            , ("User-Agent"   , "curl")
            , ("Authorization", BS.pack $ "bearer " ++ token)
            ]
