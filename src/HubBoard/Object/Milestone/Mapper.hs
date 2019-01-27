{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Milestone.Mapper
    ( refer
    )
where

import           Data.Aeson

import           HubBoard.Object.Milestone.Type
import           HubBoard.Transfer.GitHubV4.Type

instance FromJSON Milestone where
    parseJSON (Object v) = Milestone <$> (v .: "number") <*> (v .: "title")

refer :: Mapper Milestone
refer = mkMapper "milestones" "first:100, states:OPEN" "number, title"
