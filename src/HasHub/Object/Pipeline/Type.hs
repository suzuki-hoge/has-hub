{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)
import Data.Aeson.Types (Parser, parseMaybe)

import Data.Maybe (fromJust)


newtype PipelineId2 = PipelineId2 String deriving (Eq, Show)


newtype PipelineName2 = PipelineName2 String deriving (Eq, Show)


data Pipeline2 = Pipeline2 PipelineId2 PipelineName2 deriving (Eq, Show)
instance FromJSON Pipeline2 where
  parseJSON (Object v) = Pipeline2 <$> (PipelineId2 <$> v .: "id") <*> (PipelineName2 <$> v .: "name")


decodeJust :: LBS.ByteString -> [Pipeline2]
decodeJust = fromJust . parseInObject
  where
    parseInObject :: LBS.ByteString -> Maybe [Pipeline2]
    parseInObject json = decode json >>= parseMaybe (\(Object v) -> v .: "pipelines")
    -- https://artyom.me/aeson#parsing-without-creating-extra-types
