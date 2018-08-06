module HasHub.Yaml.Parser
(
  parseObjects
, parseMilestones
, module HasHub.Yaml.Parser.Data
)
where


import Data.Either.Validation (Validation(..))
import Data.Yaml (decodeFileEither, ParseException(..), YamlException(..))
import Data.Aeson (FromJSON(..))
import Data.String.Utils (startswith)

import Control.Exception

import HasHub.Yaml.Parser.Data


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


parseObjects :: FilePath -> IO (Validation [String] [Object])
parseObjects = parse rawToEpicOrIssue


parseMilestones :: FilePath -> IO (Validation [String] [Milestone])
parseMilestones = parse id
