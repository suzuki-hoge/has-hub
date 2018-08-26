{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)
import Data.Aeson.Types (parseMaybe)

import HasHub.Object.Collaborator.Type

import HasHub.Connection.Config.Type (QueryParser(..), PaginationQueryParser(..), mkAfter)

import HasHub.FixMe (asJust)


-- refer collaborators


data ReferCollaboratorsInput = ReferCollaboratorsInput

instance QueryParser ReferCollaboratorsInput where
  toQueryPart _ owner repository cursor = unlines [
      "query {"
    , "  repository(owner:\"" ++ owner ++ "\", name:\"" ++ repository ++ "\") {"
    , "    assignableUsers(first:100" ++ mkAfter cursor ++ ") {"
    , "      nodes {"
    , "        login"
    , "      }"
    , "      pageInfo {"
    , "        hasNextPage"
    , "        endCursor"
    , "      }"
    , "    }"
    , "  }"
    , "}"
    ]

instance PaginationQueryParser ReferCollaboratorsInput where
  parseHasNext _ = asJust . parse
    where
      parse :: LBS.ByteString -> Maybe Bool
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "assignableUsers")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "hasNextPage")
  parseEndCursor _ = parse
    where
      parse :: LBS.ByteString -> Maybe String
      parse lbs = decode lbs
          >>= parseMaybe (.: "data")
          >>= parseMaybe (.: "repository")
          >>= parseMaybe (.: "assignableUsers")
          >>= parseMaybe (.: "pageInfo")
          >>= parseMaybe (.: "endCursor")


instance FromJSON Collaborator where
  parseJSON (Object v) = Collaborator <$> (v .: "login")


asCollaborators :: LBS.ByteString -> IO [Collaborator]
asCollaborators lbs = asJust $ parse lbs
  where
    parse :: LBS.ByteString -> Maybe [Collaborator]
    parse json = decode json
        >>= parseMaybe (.: "data")
        >>= parseMaybe (.: "repository")
        >>= parseMaybe (.: "assignableUsers")
        >>= parseMaybe (.: "nodes")
