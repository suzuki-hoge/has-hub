{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Fetcher.GitHubV3 (
    module HubBoard.Fetcher.Config
  , module HubBoard.Fetcher.Core
  , postToGitHub
) where

import           HubBoard.Fetcher.Config
import           HubBoard.Fetcher.Core

type Resource = String
type ToResource = Owner -> Repository -> Resource
type Parse a = ByteString -> a

postToGitHub :: ToResource -> Value -> Parse a -> IO a
postToGitHub toResource value parse = do
    token <- getGitHubToken
    owner <- getOwner
    repository <- getRepository

    parse <$> secureFetch
        ("https://api.github.com/repos/" ++ toResource owner repository)
        "POST"
        [("User-Agent", "curl"), ("Authorization", token)]
        value
