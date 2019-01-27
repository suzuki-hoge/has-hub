{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Pipeline.Mapper
    ( refer
    )
where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)

import           HubBoard.Object.Pipeline.Type
import Data.Maybe
import           HubBoard.Transfer.ZenHub.Type
import Data.Aeson.Types
import           Text.Printf                    ( printf )

instance FromJSON Pipeline where
    parseJSON (Object v) = Pipeline <$> (v .: "id") <*> (v .: "name")

refer :: GetMapper Pipeline
refer = GetMapper (printf "%s/board") parse
  where
    parse :: LBS.ByteString -> [Pipeline]
    parse = fromJust . parseInObject
      where
        parseInObject :: LBS.ByteString -> Maybe [Pipeline]
        parseInObject lbs = decode lbs >>= parseMaybe (\(Object v) -> v .: "pipelines")
        -- https://artyom.me/aeson#parsing-without-creating-extra-types
