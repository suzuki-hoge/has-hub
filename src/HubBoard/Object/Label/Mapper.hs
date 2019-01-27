{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Label.Mapper
    ( refer
    )
where

import           Data.Aeson

import           HubBoard.Object.Label.Type
import           HubBoard.Transfer.GitHubV4.Type

instance FromJSON Label where
    parseJSON (Object v) = Label <$> (v .: "name")

refer :: GetMapper Label
refer = mkGetMapper "labels" "first:100" "name"
