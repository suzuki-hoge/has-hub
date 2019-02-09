{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Epic.Mapper (
    refer
  , create
) where

import           HubBoard.Object.Epic.Type
import           HubBoard.Object.Issue.Type
import qualified HubBoard.Object.Issue.Mapper as I
import           HubBoard.Fetcher

instance FromJSON EpicNumber where
    parseJSON (Object v) = EpicNumber <$> (v .: "number")

refer :: IO [EpicNumber]
refer = getFromGitHub toValue (pagenateWith "issues") parse
  where
    toValue owner repository after = let query = printf "{ repository( owner:\"%s\", name:\"%s\" ) { issues( first:100, states:OPEN, labels:[\"Epic\"]%s ) { nodes { number, title }, pageInfo { hasNextPage, endCursor } } } }" owner repository after :: String
                                     in  object ["query" .= query]
    parse = fromJust . (decode >=> parseMaybe (.: "data") >=> parseMaybe (.: "repository") >=> parseMaybe (.: "issues") >=> parseMaybe (.: "nodes"))

create :: Epic -> IO EpicNumber
create (NewEpic title body labels collaborators milestoneNumber pipeline estimate issues) = do
    issueNumber <- I.create $ Issue title body labels collaborators milestoneNumber pipeline estimate

    toEpic issueNumber
      where
        toEpic (IssueNumber number) = do
            updateZenHub toResource value "POST" parse
            return $ EpicNumber number
          where
            toResource rid = printf "%s/issues/%d/convert_to_epic" rid number
            value = object []
            parse = const ()