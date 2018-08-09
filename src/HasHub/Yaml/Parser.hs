module HasHub.Yaml.Parser
(
  parseObjects
, parseMilestones
)
where


import Data.Either.Validation (Validation(..))
import Data.Yaml (decodeFileEither, ParseException(..), YamlException(..))
import Data.Aeson (FromJSON(..))
import Data.String.Utils (startswith)

import Control.Exception

import HasHub.Object.Milestone.Data
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import HasHub.Object.Pipeline.Data
import HasHub.Object.Object.Data
import HasHub.Object.Milestone.Data


parse :: (FromJSON a) => (a -> b) -> FilePath -> IO (Validation [String] [b])
parse mapper path = do
  e <- (decodeFileEither path >>= evaluate) `catch` failure

  return $ case e of
    Right(xs)                                    -> Success $ map mapper xs
    Left(InvalidYaml (Just (YamlException msg)))
      | "Yaml file not found:" `startswith` msg  -> Failure ["no such File(" ++ path ++ ")"]
      | otherwise                                -> Failure ["invalid yaml file"]
    otherwise                                    -> Failure ["invalid yaml file"]

    where
      failure :: (FromJSON a) => SomeException -> IO (Either ParseException [a])
      failure e = return $ Left (InvalidYaml Nothing)


parseObjects :: FilePath -> IO (Validation [String] [YamlObject])
parseObjects = parse rawToEpicOrIssue
  where
    rawToEpicOrIssue :: YamlWrappedObject -> YamlObject
    rawToEpicOrIssue (YamlWrappedObject meln t b es me mmt ls cs mpn) = createEither meln (Title t) (Body <?> b) (toEpicLinkNumber <??> es) (Estimate <$> me) (MilestoneTitle <$> mmt) (Label <??> ls) (Collaborator <??> cs) (PipelineName <$> mpn)


    createEither (Just eln) = EpicYamlObject (toEpicLinkNumber eln)
    createEither Nothing    = IssueYamlObject

    (<?>) :: (String -> a) -> Maybe String -> a
    f <?> Nothing = f ""
    f <?> (Just xs) = f xs


    (<??>) :: (a -> b) -> Maybe [a] -> [b]
    f <??> Nothing = []
    f <??> (Just xs) = map f xs

    toEpicLinkNumber :: String -> EpicLinkNumber
    toEpicLinkNumber s = if head s == '#' then EpicSharpNumber n else EpicQuestionNumber n
      where
        n :: Int
        n = read $ tail s


parseMilestones :: FilePath -> IO (Validation [String] [YamlWrappedMilestone])
parseMilestones = parse id
