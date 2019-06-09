{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Label.Mapper (
    refer
) where

import           HubBoard.Object.Label.Type
import           HubBoard.Fetcher

instance FromJSON Label where
    parseJSON (Object v) = Label <$> (v .: "name")

refer :: IO [Label]
refer = getFromGitHub toValue (pagenateWith "labels") parse
  where
    toValue owner repository after = let query = printf "{ repository( owner:\"%s\", name:\"%s\" ) { labels( first:100%s ) { nodes { name }, pageInfo { hasNextPage, endCursor } } } }" owner repository after :: String
                                     in  object ["query" .= query]
    parse = fromJust . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: "labels") >=> parseMaybe (.: "nodes"))
