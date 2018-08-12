{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)

import Data.Maybe (fromJust)


newtype Label = Label String deriving (Eq, Show)
instance FromJSON Label where
  parseJSON (Object v) = Label <$> (v .: "name")


decodeJust :: LBS.ByteString -> [Label]
decodeJust = fromJust . decode
