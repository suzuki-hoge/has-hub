{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)

import HasHub.Object.Label.Type

import HasHub.Connection.Config.Type (ToResource(..))

import HasHub.FixMe (asJust)


-- input


data ReferInput = ReferInput

instance ToResource ReferInput where
  toResource _ = "/labels"


-- output


instance FromJSON Label where
  parseJSON (Object v) = Label <$> (v .: "name")


asLabels :: LBS.ByteString -> IO [Label]
asLabels lbs = asJust $ decode lbs
