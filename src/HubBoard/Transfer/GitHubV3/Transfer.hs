{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.GitHubV3.Transfer where -- todo out and local where

import           Network.HTTP.Client
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as BS
                                                ( pack )
import qualified Data.ByteString.Lazy.Internal as LBS

import           HubBoard.Transfer.GitHubV3.Type
import           HubBoard.Transfer.Config
import           Network.HTTP.Types
import           HubBoard.Transfer.Core.Transfer
import           Data.Maybe
import           Text.Printf                    ( printf )

postGitHub :: (ToJSON input, FromJSON output) => PostMapper input output -> IO output
postGitHub (PostMapper toResource input parse) = do
    headers    <- getHeaders
    owner      <- getOwner
    repository <- getRepository
    let resource = toResource owner repository

    parse <$> secureFetch (parseRequest_ $ "https://api.github.com/repos/" ++ resource)
        { method         = "POST"
        , requestHeaders = headers
        , requestBody = RequestBodyLBS $ encode input
        }
      where
        getHeaders :: IO RequestHeaders
        getHeaders = do
            token <- getGitHubToken
            return [("User-Agent", "curl"), ("Authorization", BS.pack $ "token " ++ token)]