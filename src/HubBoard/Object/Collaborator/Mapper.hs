{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Collaborator.Mapper (
    refer
) where

import           HubBoard.Object.Collaborator.Type
import           HubBoard.Fetcher

instance FromJSON Collaborator where
    parseJSON (Object v) = Collaborator <$> (v .: "login")

refer :: IO [Collaborator]
refer = getFromGitHub toValue (pagenateWith "assignableUsers") parse
  where
    toValue owner repository after = let query = printf "{ repository( owner:\"%s\", name:\"%s\" ) { assignableUsers( first:100%s ) { nodes { login }, pageInfo { hasNextPage, endCursor } } } }" owner repository after :: String
                                     in  object ["query" .= query]
    parse = fromJust . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: "assignableUsers") >=> parseMaybe (.: "nodes"))
