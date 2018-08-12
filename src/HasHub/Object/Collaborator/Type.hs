{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)

import Data.Maybe (fromJust)


newtype Collaborator = Collaborator String deriving (Eq, Show)
instance FromJSON Collaborator where
  parseJSON (Object v) = Collaborator <$> (v .: "login")


decodeJust :: LBS.ByteString -> [Collaborator]
decodeJust = fromJust . decode
