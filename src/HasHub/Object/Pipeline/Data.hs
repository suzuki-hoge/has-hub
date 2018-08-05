{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Pipeline.Data where


import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy.Internal as L (ByteString)


newtype PipelineName = PipelineName String deriving (Eq)
instance Show PipelineName where
  show (PipelineName name) = "PipelineName(" ++ name ++ ")"


data Pipeline = Pipeline String String deriving (Eq)
instance FromJSON Pipeline where
  parseJSON (Object v) = Pipeline <$> (v .: "id") <*> (v .: "name")
instance Show Pipeline where
  show (Pipeline id name) = "Pipeline(" ++ name ++ ")"


parseInList :: L.ByteString -> Maybe [Pipeline]
parseInList json = parseMaybe extract =<< decode json
  where
    extract :: Value -> Parser [Pipeline]
    extract (Object v) = v .: "pipelines"
    -- https://artyom.me/aeson#parsing-without-creating-extra-types