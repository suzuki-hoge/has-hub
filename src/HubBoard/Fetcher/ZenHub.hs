{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Fetcher.ZenHub (
    module HubBoard.Fetcher.Config
  , module HubBoard.Fetcher.Core
  , getFromZenHub
  , updateZenHub
) where

import           HubBoard.Fetcher.Config
import           HubBoard.Fetcher.Core

type Resource = String
type ToResource = RepositoryId -> Resource
type ParseG a = ByteString -> [a]

getFromZenHub :: ToResource -> ParseG a -> IO [a]
getFromZenHub toResource parse = do
    token <- getZenHubToken
    rid <- getRepositoryId

    parse <$> secureFetch
        ("https://api.zenhub.io/p1/repositories/" ++ toResource rid)
        "GET"
        [("content-type", "application/json"), ("User-Agent", "curl"), ("X-Authentication-Token", token)]
        (object [])

type ParseU a = ByteString -> a

updateZenHub :: ToResource -> Value -> Method -> ParseU a -> IO a
updateZenHub toResource value method parse = do
    token <- getZenHubToken
    rid <- getRepositoryId

    parse <$> secureFetch
        ("https://api.zenhub.io/p1/repositories/" ++ toResource rid)
        method
        [("content-type", "application/json"), ("User-Agent", "curl"), ("X-Authentication-Token", token)]
        value
