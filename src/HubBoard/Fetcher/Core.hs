{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Fetcher.Core (
    module Network.HTTP.Client
  , module Network.HTTP.Types
  , module Data.Aeson
  , module Data.Aeson.Types
  , module Data.Maybe
  , module Control.Monad
  , module Text.Printf
  , module Data.ByteString.Lazy.Char8
  , secureFetch
) where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS             ( tlsManagerSettings )
import           Network.HTTP.Types
import           Data.Aeson
import           Data.Aeson.Types                    ( parseMaybe )
import           Data.Maybe                          ( fromJust, maybe )
import           Control.Monad
import qualified Data.ByteString.Char8         as BS ( ByteString )
import           Data.ByteString.Lazy.Char8          ( ByteString, unpack)
import           Text.Printf                         ( printf )
import           Data.Time.Clock                     ( getCurrentTime )
import           Data.Time.LocalTime                 ( getCurrentTimeZone, utcToLocalTime )
import           Data.List.Split                     ( splitOn )
import           Codec.Binary.UTF8.String            ( decodeString )
import           Data.List.Utils                     ( replace )

type Url = String
type LogLine = String
type Now = String

secureFetch :: Url -> Method -> RequestHeaders -> Value -> IO ByteString
secureFetch url method headers value = do
    manager  <- newManager tlsManagerSettings

    logging $ requestString url method value

    let request = (parseRequest_ url)
          { method = method
          , requestHeaders = headers
          , requestBody = RequestBodyLBS $ encode value
          }

    response <- responseBody <$> httpLbs request manager

    logging $ responseString response

    return response
      where
        requestString :: Url -> Method -> Value -> Now -> LogLine
        requestString url method value now = printf "%s | in : %s [%s] %s\n" now (toLogLine $ encode value) (methodString method) url

        responseString :: ByteString -> Now -> LogLine
        responseString lbs now = printf "%s | out: %s\n\n" now (toLogLine lbs)

        toLogLine :: ByteString -> LogLine
        toLogLine = replace "\n" "" . decodeString . unpack

        methodString :: Method -> String
        methodString "GET" = "GET"
        methodString "POST" = "POST"
        methodString "PUT" = "PUT"

        logging :: (Now -> LogLine) -> IO ()
        logging toLine = do
            utc <- getCurrentTime
            tz <- getCurrentTimeZone
        
            let now = head . splitOn "." . show $ utcToLocalTime tz utc

            appendFile "./hub-board.log" $ toLine now
