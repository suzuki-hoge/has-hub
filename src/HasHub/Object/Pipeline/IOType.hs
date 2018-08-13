{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)
import Data.Aeson.Types (parseMaybe)

import Data.Maybe (fromJust)

import HasHub.Object.Pipeline.Type

import HasHub.Connection.Type (ToResource(..))


data ReferInput = ReferInput

instance ToResource ReferInput where
  toResource _ = "/board"

instance FromJSON Pipeline where
  parseJSON (Object v) = Pipeline <$> (PipelineId <$> v .: "id") <*> (PipelineName <$> v .: "name")


asPipelines :: LBS.ByteString -> [Pipeline]
asPipelines = fromJust . parseInObject
  where
    parseInObject :: LBS.ByteString -> Maybe [Pipeline]
    parseInObject json = decode json >>= parseMaybe (\(Object v) -> v .: "pipelines")
    -- https://artyom.me/aeson#parsing-without-creating-extra-types
