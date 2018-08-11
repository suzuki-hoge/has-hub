{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)

import Data.Maybe (fromJust)


newtype Label2 = Label2 String deriving (Eq, Show)
instance FromJSON Label2 where
  parseJSON (Object v) = Label2 <$> (v .: "name")


decodeJust :: LBS.ByteString -> [Label2]
decodeJust = fromJust . decode
