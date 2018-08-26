{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)
import Data.Aeson.Types (parseMaybe)

import HasHub.Object.Label.Type

import HasHub.Connection.Config.Type (QueryParser(..), PaginationQueryParser(..), mkAfter)

import HasHub.FixMe (asJust)


-- refer labels


data ReferLabelsInput = ReferLabelsInput

instance QueryParser ReferLabelsInput where
  toQueryPart _ owner repository cursor = unlines [
      "query {"
    , "  repository(owner:\"" ++ owner ++ "\", name:\"" ++ repository ++ "\") {"
    , "    labels(first:100" ++ mkAfter cursor ++ ") {"
    , "      nodes {"
    , "        name"
    , "      }"
    , "      pageInfo {"
    , "        hasNextPage"
    , "        endCursor"
    , "      }"
    , "    }"
    , "  }"
    , "}"
    ]

instance PaginationQueryParser ReferLabelsInput where
  parseHasNext _ = asJust . parse
    where
      parse :: LBS.ByteString -> Maybe Bool
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "labels")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "hasNextPage")
  parseEndCursor _ = parse
    where
      parse :: LBS.ByteString -> Maybe String
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "labels")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "endCursor")


instance FromJSON Label where
  parseJSON (Object v) = Label <$> (v .: "name")


asLabels :: LBS.ByteString -> IO [Label]
asLabels lbs = asJust $ parse lbs
  where
    parse :: LBS.ByteString -> Maybe [Label]
    parse json = decode json
        >>= parseMaybe (.: "data")
        >>= parseMaybe (.: "repository")
        >>= parseMaybe (.: "labels")
        >>= parseMaybe (.: "nodes")
