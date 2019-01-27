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
import           HubBoard.Transfer.Core.Transfer

getGitHub :: Mapper a -> IO [a]
getGitHub = getGitHubV4 Nothing []

  where
    getGitHubV4 :: Cursor -> [a] -> Mapper a -> IO [a]
    getGitHubV4 cursor acc mapper@(Mapper toQuery asHasNext asCursor parse) = do
        headers      <- getHeaders
        owner      <- getOwner
        repository <- getRepository

        lbs <- secureFetch (parseRequest_ "https://api.github.com/GitHubV4")
            { method         = "POST"
            , requestHeaders = headers
            , requestBody    = RequestBodyLBS $ encode $ object
                                   ["query" .= toQuery owner repository cursor]
            }

        let accumulated = acc ++ (parse lbs)

        if asHasNext lbs
            then getGitHubV4 (asCursor lbs) accumulated mapper
            else return accumulated

getHeaders :: IO RequestHeaders
getHeaders = do
    token <- getGitHubToken
    return [ ("content-type" , "application/json")
           , ("User-Agent"   , "curl")
           , ("Authorization", BS.pack $ "bearer " ++ token)
           ]