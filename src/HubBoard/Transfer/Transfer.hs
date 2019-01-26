{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.Transfer where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as BS
                                                ( pack )
import qualified Data.ByteString.Lazy.Internal as LBS

import           HubBoard.Transfer.Type

getGitHub :: Mapper a -> IO [a]
getGitHub = getGitHubRecursive Nothing []

getGitHubRecursive :: Cursor -> [a] -> Mapper a -> IO [a]
getGitHubRecursive cursor acc mapper@(Mapper toQuery asHasNext asCursor parse)
    = do
        headers <- headers'
        let owner      = "suzuki-hoge"
        let repository = "has-hub-workspace"

        lbs <- secureFetch (parseRequest_ "https://api.github.com/graphql")
            { method         = "POST"
            , requestHeaders = headers
            , requestBody    = RequestBodyLBS $ encode $ object
                                   ["query" .= toQuery owner repository Nothing]
            }

        let accumulated = acc ++ (parse lbs)

        if asHasNext lbs
            then getGitHubRecursive (asCursor lbs) accumulated mapper
            else return accumulated

secureFetch :: Request -> IO LBS.ByteString
secureFetch request = do
    manager  <- newManager tlsManagerSettings

    response <- responseBody <$> httpLbs request manager

    return response

headers' :: IO RequestHeaders
headers' = do
    let token = "xxx"
    return
        [ ("content-type" , "application/json")
        , ("User-Agent"   , "curl")
        , ("Authorization", BS.pack $ "bearer " ++ token)
        ]
