{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Object.Parser
(
  readObjects
, YamlObject(..)
, YamlWrappedObject(..)
, _epicLinkNumbers
, _epicLinkNumbersWithDuplication
, _pipelineNames
, _labels
, _collaborators
, _milestoneTitles
, _parentEpicNumbers
, _definitionEpicLinkNumbers
, _parentEpicLinkNumbers
, module HasHub.Object.Object.Type
, module HasHub.Object.Pipeline.Type
, module HasHub.Object.Label.Type
, module HasHub.Object.Collaborator.Type
, module HasHub.Object.Milestone.Type
, module HasHub.FixMe
)
where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?))

import Data.List (nub)
import Data.Maybe (mapMaybe)

import HasHub.Yaml.Reader (readYaml, YamlReadingError(..))

import HasHub.Object.Object.Type
import HasHub.Object.Pipeline.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type

import HasHub.FixMe (Validation(..))


data YamlObject = EpicYamlObject
                  EpicLinkNumber
                  Title
                  Body
                  (Maybe PipelineName)
                  [Label]
                  [Collaborator]
                  (Maybe MilestoneTitle)
                  (Maybe Estimate)
                  [ParentEpicNumber]
            | IssueYamlObject
                  Title
                  Body
                  (Maybe PipelineName)
                  [Label]
                  [Collaborator]
                  (Maybe MilestoneTitle)
                  (Maybe Estimate)
                  [ParentEpicNumber]
            deriving (Eq, Show)


data YamlWrappedObject = YamlWrappedObject
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
instance FromJSON YamlWrappedObject where
  parseJSON (Object v) = YamlWrappedObject <$> (v .:? "epic-link-number") <*> (v .: "title") <*> (v .:? "body") <*> (v .:? "pipeline") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "milestone") <*> (v .:? "estimate") <*> (v .:? "epics")


readObjects :: FilePath -> IO (Validation [YamlReadingError] [YamlObject])
readObjects = readYaml mapping
  where
    mapping :: YamlWrappedObject -> YamlObject
    mapping (YamlWrappedObject meln t mb mpn ls cs mmt me es) = createEither meln (Title t) (Body <?> mb) (PipelineName <$> mpn) (Label <??> ls) (Collaborator <??> cs) (MilestoneTitle <$> mmt) (Estimate <$> me) (toParent <??> es)
      where
        createEither (Just eln) = EpicYamlObject (EpicLinkNumber eln)
        createEither Nothing    = IssueYamlObject

        (<?>) :: (String -> a) -> Maybe String -> a
        f <?> Nothing = f ""
        f <?> (Just xs) = f xs


        (<??>) :: (a -> b) -> Maybe [a] -> [b]
        f <??> Nothing = []
        f <??> (Just xs) = map f xs

        toParent :: String -> ParentEpicNumber
        toParent s = if head s == '#' then SharpEpicNumber s else QuestionEpicNumber s


_epicLinkNumbers :: [YamlObject] -> [EpicLinkNumber]
_epicLinkNumbers = nub . _epicLinkNumbersWithDuplication


_epicLinkNumbersWithDuplication :: [YamlObject] -> [EpicLinkNumber]
_epicLinkNumbersWithDuplication = mapMaybe extract
  where
    extract (EpicYamlObject x _ _ _ _ _ _ _ _) = Just x
    extract (IssueYamlObject  _ _ _ _ _ _ _ _) = Nothing


_pipelineNames :: [YamlObject] -> [PipelineName]
_pipelineNames = nub . mapMaybe extract
  where
    extract (EpicYamlObject _ _ _ x _ _ _ _ _) = x
    extract (IssueYamlObject  _ _ x _ _ _ _ _) = x


_labels :: [YamlObject] -> [Label]
_labels = nub . concatMap extract
  where
    extract (EpicYamlObject _ _ _ _ x _ _ _ _) = x
    extract (IssueYamlObject  _ _ _ x _ _ _ _) = x


_collaborators :: [YamlObject] -> [Collaborator]
_collaborators = nub . concatMap extract
  where
    extract (EpicYamlObject _ _ _ _ _ x _ _ _) = x
    extract (IssueYamlObject  _ _ _ _ x _ _ _) = x


_milestoneTitles :: [YamlObject] -> [MilestoneTitle]
_milestoneTitles = nub . mapMaybe extract
  where
    extract (EpicYamlObject _ _ _ _ _ _ x _ _) = x
    extract (IssueYamlObject  _ _ _ _ _ x _ _) = x


_parentEpicNumbers :: [YamlObject] -> [ParentEpicNumber]
_parentEpicNumbers = nub . concatMap extract
  where
    extract (EpicYamlObject _ _ _ _ _ _ _ _ x) = x
    extract (IssueYamlObject  _ _ _ _ _ _ _ x) = x


_definitionEpicLinkNumbers :: [YamlObject] -> [Definition]
_definitionEpicLinkNumbers objects = mapMaybe extract (zip [1..] objects)
  where
    extract (n, EpicYamlObject x _ _ _ _ _ _ _ _) = Just (n, x)
    extract (_, IssueYamlObject  _ _ _ _ _ _ _ _) = Nothing


_parentEpicLinkNumbers :: [YamlObject] -> [Parent]
_parentEpicLinkNumbers objects = concatMap extract (zip [1..] objects)
  where
    extract (n, EpicYamlObject _ _ _ _ _ _ _ _ xs) = map (\x -> (n, x)) xs
    extract (n, IssueYamlObject  _ _ _ _ _ _ _ xs) = map (\x -> (n, x)) xs
