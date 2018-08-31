{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Parser where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?))

import Data.List (nub)
import Data.Maybe (mapMaybe)

import HasHub.Yaml.Reader (readYamls, YamlReadingError(..))

import HasHub.Object.Object.Type
import HasHub.Object.Pipeline.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type

import HasHub.FixMe (Validation(..))


data YamlObject = YamlEpic
                  EpicLinkNumber
                  Title
                  Body
                  (Maybe PipelineName)
                  [Label]
                  [Collaborator]
                  (Maybe MilestoneTitle)
                  (Maybe Estimate)
                  [LinkingEpicNumber]
                | YamlIssue
                  Title
                  Body
                  (Maybe PipelineName)
                  [Label]
                  [Collaborator]
                  (Maybe MilestoneTitle)
                  (Maybe Estimate)
                  [LinkingEpicNumber]
                deriving (Eq, Show)


data WrappedYamlObject = WrappedYamlObject
                         (Maybe String)   -- epic-link-number
                         String           -- title
                         (Maybe String)   -- body
                         (Maybe String)   -- pipeline
                         (Maybe [String]) -- labels
                         (Maybe [String]) -- assignees
                         (Maybe String)   -- milestone
                         (Maybe Double)   -- estimate
                         (Maybe [String]) -- epics
                       deriving Show
instance FromJSON WrappedYamlObject where
  parseJSON (Object v) = WrappedYamlObject <$> (v .:? "epic-link-number") <*> (v .: "title") <*> (v .:? "body") <*> (v .:? "pipeline") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "milestone") <*> (v .:? "estimate") <*> (v .:? "epics")


readObjects :: [FilePath] -> IO (Validation [YamlReadingError] [YamlObject])
readObjects = readYamls mapping
  where
    mapping :: WrappedYamlObject -> YamlObject
    mapping (WrappedYamlObject meln t mb mpn ls cs mmt me es) = createEither meln (Title t) (Body <?> mb) (PipelineName <$> mpn) (Label <??> ls) (Collaborator <??> cs) (MilestoneTitle <$> mmt) (Estimate <$> me) (toLinking <??> es)
      where
        createEither (Just eln) = YamlEpic (EpicLinkNumber eln)
        createEither Nothing    = YamlIssue

        (<?>) :: (String -> a) -> Maybe String -> a
        f <?> Nothing = f ""
        f <?> (Just xs) = f xs


        (<??>) :: (a -> b) -> Maybe [a] -> [b]
        f <??> Nothing = []
        f <??> (Just xs) = map f xs

        toLinking :: String -> LinkingEpicNumber
        toLinking s = if head s == '#' then SharpEpicNumber s else QuestionEpicNumber s


_epicLinkNumbers :: [YamlObject] -> [EpicLinkNumber]
_epicLinkNumbers = nub . _epicLinkNumbersWithDuplication


_epicLinkNumbersWithDuplication :: [YamlObject] -> [EpicLinkNumber]
_epicLinkNumbersWithDuplication = mapMaybe extract
  where
    extract (YamlEpic x _ _ _ _ _ _ _ _) = Just x
    extract (YamlIssue  _ _ _ _ _ _ _ _) = Nothing


_pipelineNames :: [YamlObject] -> [PipelineName]
_pipelineNames = nub . mapMaybe _pipelineName

_pipelineName :: YamlObject -> Maybe PipelineName
_pipelineName (YamlEpic _ _ _ x _ _ _ _ _) = x
_pipelineName (YamlIssue  _ _ x _ _ _ _ _) = x


_labels :: [YamlObject] -> [Label]
_labels = nub . concatMap _label

_label :: YamlObject -> [Label]
_label (YamlEpic _ _ _ _ x _ _ _ _) = x
_label (YamlIssue  _ _ _ x _ _ _ _) = x


_collaborators :: [YamlObject] -> [Collaborator]
_collaborators = nub . concatMap _collaborator

_collaborator :: YamlObject -> [Collaborator]
_collaborator (YamlEpic _ _ _ _ _ x _ _ _) = x
_collaborator (YamlIssue  _ _ _ _ x _ _ _) = x


_milestoneTitles :: [YamlObject] -> [MilestoneTitle]
_milestoneTitles = nub . mapMaybe _milestoneTitle

_milestoneTitle :: YamlObject -> Maybe MilestoneTitle
_milestoneTitle (YamlEpic _ _ _ _ _ _ x _ _) = x
_milestoneTitle (YamlIssue  _ _ _ _ _ x _ _) = x


_linkingEpicNumbers :: [YamlObject] -> [LinkingEpicNumber]
_linkingEpicNumbers = nub . concatMap _linkingEpicNumber

_linkingEpicNumber :: YamlObject -> [LinkingEpicNumber]
_linkingEpicNumber (YamlEpic _ _ _ _ _ _ _ _ x) = x
_linkingEpicNumber (YamlIssue  _ _ _ _ _ _ _ x) = x


_linkeds :: [YamlObject] -> [Linked]
_linkeds yamls = mapMaybe _linked (zip [1..] yamls)

_linked :: (LineNum, YamlObject) -> Maybe Linked
_linked (n, YamlEpic x _ _ _ _ _ _ _ _) = Just (n, x)
_linked (_, YamlIssue  _ _ _ _ _ _ _ _) = Nothing


_linkings :: [YamlObject] -> [Linking]
_linkings yamls = concatMap _linking (zip [1..] yamls)

_linking :: (LineNum, YamlObject) -> [Linking]
_linking (n, YamlEpic _ _ _ _ _ _ _ _ xs) = map (\x -> (n, x)) xs
_linking (n, YamlIssue  _ _ _ _ _ _ _ xs) = map (\x -> (n, x)) xs
