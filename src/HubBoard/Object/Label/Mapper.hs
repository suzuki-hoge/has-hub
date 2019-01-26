{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Label.Mapper where

import           HubBoard.Object.Label.Type
import           HubBoard.Transfer.Type

import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types

instance FromJSON Label where
    parseJSON (Object v) = Label <$> (v .: "name")

mapper :: Mapper Label
mapper = Mapper (mkToQuery "labels" "first:100" "name")
                (mkAsHasNext "labels")
                (mkAsCursor "labels")
                (mkParse "labels")
