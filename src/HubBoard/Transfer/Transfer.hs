{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.Transfer where -- todo out and local where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as BS
                                                ( pack )
import qualified Data.ByteString.Lazy.Internal as LBS

import           HubBoard.Transfer.Type
import           HubBoard.Transfer.Config

getGitHub :: Mapper a -> IO [a]
getGitHub = getGraphQL Nothing []

getGraphQL :: Cursor -> [a] -> Mapper a -> IO [a]
getGraphQL cursor acc mapper@(Mapper toQuery asHasNext asCursor parse)
    = do
        headers <- graphQLHeaders
        owner <- getOwner
        repository <- getRepository

        lbs <- secureFetch (parseRequest_ "https://api.github.com/graphql")
            { method         = "POST"
            , requestHeaders = headers
            , requestBody    = RequestBodyLBS $ encode $ object
                                   ["query" .= toQuery owner repository Nothing]
            }

        let accumulated = acc ++ (parse lbs)

        if asHasNext lbs
            then getGraphQL (asCursor lbs) accumulated mapper
            else return accumulated

secureFetch :: Request -> IO LBS.ByteString
secureFetch request = do
    manager  <- newManager tlsManagerSettings

    response <- responseBody <$> httpLbs request manager

    return response

graphQLHeaders :: IO RequestHeaders
graphQLHeaders = do
    token <- getGitHubToken
    return
        [ ("content-type" , "application/json")
        , ("User-Agent"   , "curl")
        , ("Authorization", BS.pack $ "bearer " ++ token)
        ]
