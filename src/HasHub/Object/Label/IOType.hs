{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)

import Data.Maybe (fromJust)

import HasHub.Object.Label.Type

import HasHub.Connection.Config.Type (ToResource(..))


-- input


data ReferInput = ReferInput

instance ToResource ReferInput where
  toResource _ = "/labels"


-- output


instance FromJSON Label where
  parseJSON (Object v) = Label <$> (v .: "name")

asLabels :: LBS.ByteString -> [Label]
asLabels = fromJust . decode
