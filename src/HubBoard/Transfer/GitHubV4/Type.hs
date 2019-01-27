{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.GitHubV4.Type
    ( GetMapper(..)
    , mkGetMapper
    , module HubBoard.Transfer.Core.Type
    )
where

import qualified Data.Text.Internal            as T
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Text.Printf                    ( printf )

import           HubBoard.Transfer.Core.Type

type ToQuery = Owner -> Repository -> Cursor -> String
type AsHasNext = LBS.ByteString -> Bool
type AsCursor = LBS.ByteString -> Cursor
type Parse output = LBS.ByteString -> [output]

data GetMapper output = GetMapper ToQuery AsHasNext AsCursor (Parse output)

mkGetMapper :: (FromJSON output) => T.Text -> String -> String -> GetMapper output
mkGetMapper name filters elements = GetMapper
    (mkToQuery name filters elements)
    (mkAsHasNext name)
    (mkAsCursor name)
    (mkParse name)

  where
    mkToQuery :: T.Text -> String -> String -> ToQuery
    mkToQuery name filters elements owner repository cursor = unlines
        [ printf "query {"
        , printf "  repository(owner:\"%s\", name:\"%s\") {" owner repository
        , printf "    %s(%s%s) {" name filters (mkAfter cursor)
        , printf "      nodes { %s }" elements
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

    mkParse :: (FromJSON output) => T.Text -> Parse output
    mkParse name lbs =
        fromJust
            $   decode lbs
            >>= parseMaybe (.: "data")
            >>= parseMaybe (.: "repository")
            >>= parseMaybe (.: name)
            >>= parseMaybe (.: "nodes")
