{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.Type (Owner, Repository, Token, Endpoint, Cursor, Mapper(..), mkToQuery, mkAsHasNext, mkAsCursor, mkParse) where

import qualified Data.Text.Internal            as T
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import Text.Printf (printf)

type Owner = String
type Repository = String
type Token = String
type Endpoint = String
type Cursor = Maybe String

type ToQuery = Owner -> Repository -> Cursor -> String
type AsHasNext = LBS.ByteString -> Bool
type AsCursor = LBS.ByteString -> Cursor
type Parse a = LBS.ByteString -> [a]

data Mapper a = Mapper ToQuery AsHasNext AsCursor (Parse a)

mkToQuery :: String -> String -> String -> ToQuery
mkToQuery name filters elements owner repository cursor = unlines
    [ printf "query {"
    , printf "  repository(owner:\"%s\", name:\"%s\") {" owner repository
    , printf "    %s(%s%s) {"                            name filters (mkAfter cursor)
    , printf "      nodes { %s }"                        elements
    , printf "      pageInfo { hasNextPage, endCursor }"
    , printf "    }"
    , printf "  }"
    , printf "}"
    ]
  where
    mkAfter :: Cursor -> String
    -- mkAfter = maybe "" ((", after:\"" ++) . (++ "\""))
    mkAfter = maybe "" (printf ", after:\"%s\"")

mkAsHasNext :: T.Text -> AsHasNext
mkAsHasNext name lbs =
    fromJust
        $   decode lbs
        >>= parseMaybe (.: "data")
        >>= parseMaybe (.: "repository")
        >>= parseMaybe (.: name)
        >>= parseMaybe (.: "pageInfo")
        >>= parseMaybe (.: "hasNextPage")

mkAsCursor :: T.Text -> AsCursor
mkAsCursor name lbs =
    decode lbs
        >>= parseMaybe (.: "data")
        >>= parseMaybe (.: "repository")
        >>= parseMaybe (.: name)
        >>= parseMaybe (.: "pageInfo")
        >>= parseMaybe (.: "hasNextPage")

mkParse :: (FromJSON a) => T.Text -> Parse a
mkParse name lbs =
    fromJust
        $   decode lbs
        >>= parseMaybe (.: "data")
        >>= parseMaybe (.: "repository")
        >>= parseMaybe (.: name)
        >>= parseMaybe (.: "nodes")
