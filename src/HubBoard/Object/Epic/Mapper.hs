{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Epic.Mapper
    ( refer
    )
where

import           Data.Aeson

import           HubBoard.Object.Epic.Type
import           HubBoard.Transfer.GitHubV4.Type

instance FromJSON Epic where
    parseJSON (Object v) = Epic <$> (v .: "number") <*> (v .: "title")

refer :: Mapper Epic
refer = mkMapper "issues"
                 "first:100, states:OPEN, labels:[\"Epic\"]"
                 "number, title"
