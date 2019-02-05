{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Fetcher.GitHubV4 (
    module HubBoard.Fetcher.Config
  , module HubBoard.Fetcher.Core
  , getFromGitHub
  , noPagenate
  , pagenateWith
) where

import qualified Data.Text.Internal       as T ( Text )

import           HubBoard.Fetcher.Config
import           HubBoard.Fetcher.Core

type After = String
type AsHasNext = ByteString -> Bool
type AsAfter = ByteString -> After
type ToValue = Owner -> Repository -> After -> Value
type Pagenation = (AsHasNext, AsAfter)
type Parse a = ByteString -> [a]

getFromGitHub :: ToValue -> Pagenation -> Parse a -> IO [a]
getFromGitHub toValue pagenation parse = do
    token <- getGitHubToken
    owner <- getOwner
    repository <- getRepository

    recGet token owner repository "" toValue pagenation [] parse

      where
        recGet :: Token -> Owner -> Repository -> After -> ToValue -> Pagenation -> [a] -> Parse a -> IO [a]
        recGet token owner repository after toValue pagenation@(asHasNext, asAfter) acc parse = do
            lbs <- secureFetch
                "https://api.github.com/graphql"
                "POST"
                [("User-Agent", "curl"), ("Authorization", token)]
                (toValue owner repository after)

            let accumulated = acc ++ (parse lbs)

            if asHasNext lbs
                then recGet token owner repository (asAfter lbs) toValue pagenation accumulated parse
                else return accumulated

noPagenate :: Pagenation
noPagenate = (const False, const "")

pagenateWith :: T.Text -> Pagenation
pagenateWith resource = (mkAsHasNext resource, mkAsCursor resource)

  where
    mkAsHasNext :: T.Text -> AsHasNext
    mkAsHasNext resource = fromJust . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: resource) >=> parseMaybe (.: "pageInfo") >=> parseMaybe (.: "hasNextPage"))

    mkAsCursor :: T.Text -> AsAfter
    mkAsCursor resource = mkAfter . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: resource) >=> parseMaybe (.: "pageInfo") >=> parseMaybe (.: "endCursor"))

      where
        mkAfter :: Maybe String -> After
        mkAfter = maybe "" (printf ", after:\"%s\"")
