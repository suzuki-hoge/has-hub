{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)
import Data.Aeson.Types (parseMaybe)

import Data.Maybe (fromJust)

import HasHub.Connection.Type (ToResource(..))


data ReferInput = ReferInput
instance ToResource ReferInput where
  toResource _ = "/board"


newtype PipelineId = PipelineId String deriving (Eq, Show)


newtype PipelineName = PipelineName String deriving (Eq, Show)


data Pipeline = Pipeline PipelineId PipelineName deriving (Eq, Show)
instance FromJSON Pipeline where
  parseJSON (Object v) = Pipeline <$> (PipelineId <$> v .: "id") <*> (PipelineName <$> v .: "name")


decodeJust :: LBS.ByteString -> [Pipeline]
decodeJust = fromJust . parseInObject
  where
    parseInObject :: LBS.ByteString -> Maybe [Pipeline]
    parseInObject json = decode json >>= parseMaybe (\(Object v) -> v .: "pipelines")
    -- https://artyom.me/aeson#parsing-without-creating-extra-types


_name :: Pipeline -> PipelineName
_name (Pipeline _ name) = name
