{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Label.Mapper (refer) where

import           HubBoard.Object.Label.Type
import           HubBoard.Transfer.Type

import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types

instance FromJSON Label where
    parseJSON (Object v) = Label <$> (v .: "name")

refer :: Mapper Label
refer = Mapper (mkToQuery "labels" "first:100" "name")
               (mkAsHasNext "labels")
               (mkAsCursor "labels")
               (mkParse "labels")
