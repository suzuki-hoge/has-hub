{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Collaborator.Mapper
    ( refer
    )
where

import           Data.Aeson

import           HubBoard.Object.Collaborator.Type
import           HubBoard.Transfer.GitHubV4.Type

instance FromJSON Collaborator where
    parseJSON (Object v) = Collaborator <$> (v .: "login")

refer :: GetMapper Collaborator
refer = mkGetMapper "assignableUsers" "first:100" "login"
