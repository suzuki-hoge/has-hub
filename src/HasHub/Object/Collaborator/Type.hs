{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)

import Data.Maybe (fromJust)


newtype Collaborator2 = Collaborator2 String deriving (Eq, Show)
instance FromJSON Collaborator2 where
  parseJSON (Object v) = Collaborator2 <$> (v .: "login")


decodeJust :: LBS.ByteString -> [Collaborator2]
decodeJust = fromJust . decode
