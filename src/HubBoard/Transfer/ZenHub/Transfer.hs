{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.ZenHub.Transfer where -- todo out and local where

import           Network.HTTP.Client
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as BS
                                                ( pack )
import qualified Data.ByteString.Lazy.Internal as LBS

import           HubBoard.Transfer.ZenHub.Type
import           HubBoard.Transfer.Config
import           HubBoard.Transfer.Core.Transfer

getZenHub :: (FromJSON a) => Mapper a -> IO [a]
getZenHub (Mapper toResource parse) = do
    headers <- getHeaders
    rid   <- return "131548410" -- todo

    parse <$> secureFetch (  parseRequest_ $  "https://api.zenhub.io/p1/repositories/" ++ (toResource rid))
        { method         = "GET"
        , requestHeaders = headers
        }

getHeaders :: IO RequestHeaders
getHeaders = do
    token <- getZenHubToken
    return [ ("content-type"          , "application/json")
           , ("User-Agent"            , "curl")
           , ("X-Authentication-Token", BS.pack token)
           ]