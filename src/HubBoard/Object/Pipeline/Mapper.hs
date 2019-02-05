{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Pipeline.Mapper (
    refer
) where

import           HubBoard.Object.Pipeline.Type
import           HubBoard.Fetcher

instance FromJSON Pipeline where
    parseJSON (Object v) = Pipeline <$> (v .: "id") <*> (v .: "name")

refer :: IO [Pipeline]
refer = getFromZenHub toResource parse
  where
    toResource = printf "%s/board"
    parse = fromJust . (decode >=> parseMaybe (.: "pipelines"))
