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
import           Network.HTTP.Types
import           HubBoard.Transfer.Core.Transfer

getZenHub :: (FromJSON output) => GetMapper output -> IO [output]
getZenHub (GetMapper toResource parse) = do
    headers <- getHeaders
    rid     <- getRepositoryIdFromEnv
    let url = "https://api.zenhub.io/p1/repositories/" ++ (toResource rid)

    parse <$> secureFetch (parseRequest_ url) { method         = "GET"
                                              , requestHeaders = headers
                                              }

updateZenHub :: (ToJSON input) => UpdateMapper input -> IO ()
updateZenHub (UpdateMapper method toResource input) = do
    headers    <- getHeaders
    rid <- getRepositoryIdFromEnv
    let url = "https://api.zenhub.io/p1/repositories/" ++ (toResource rid)

    (\lbs -> ()) <$> secureFetch (parseRequest_ url)
        { method         = method
        , requestHeaders = headers
        , requestBody = RequestBodyLBS $ encode input
        }

getHeaders :: IO RequestHeaders
getHeaders = do
    token <- getZenHubToken
    return
        [ ("content-type"          , "application/json")
        , ("User-Agent"            , "curl")
        , ("X-Authentication-Token", BS.pack token)
        ]