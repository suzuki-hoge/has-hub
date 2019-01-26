{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.Type where

import qualified Data.Text.Internal            as T
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types

type Owner = String
type Repository = String
type Cursor = Maybe String

type ToQuery = Owner -> Repository -> Cursor -> String
type AsHasNext = LBS.ByteString -> Bool
type AsCursor = LBS.ByteString -> Cursor
type Parse a = LBS.ByteString -> [a]

data Mapper a = Mapper
    (Owner -> Repository -> Cursor -> String) -- toQuery
    (LBS.ByteString -> Bool)                  -- asHasNext
    (LBS.ByteString -> Cursor)                -- asCursor
    (LBS.ByteString -> [a])                   -- parse

mkToQuery :: String -> String -> String -> ToQuery
mkToQuery name filters elements owner repository cursor = unlines
    [ "query {"
    , "  repository(owner:\"" ++ owner ++ "\", name:\"" ++ repository ++ "\") {"
    , "    " ++ name ++ "(" ++ filters ++ (mkAfter cursor) ++ ") {"
    , "      nodes { " ++ elements ++ " }"
    , "      pageInfo { hasNextPage, endCursor }"
    , "    }"
    , "  }"
    , "}"
    ]
  where
    mkAfter :: Cursor -> String
    mkAfter = maybe "" ((", after:\"" ++) . (++ "\""))

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
