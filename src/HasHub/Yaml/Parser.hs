module HasHub.Yaml.Parser
(
  parseObjects
, module HasHub.Yaml.Parser.Data
)
where


import Data.Either.Validation (Validation(..))

import Data.Yaml (decodeFileEither, ParseException(..), YamlException(..))

import Data.String.Utils (startswith)

import Control.Exception

import HasHub.Yaml.Parser.Data


parseObjects :: FilePath -> IO (Validation [String] [Object])
parseObjects path = do
  e <- (decodeFileEither path >>= evaluate) `catch` parseFailure

  return $ case e of
    Right(rawObjects)                                                       -> Success $ map rawToEpicOrIssue rawObjects
    Left(InvalidYaml (Just (YamlException msg)))
      | "Yaml file not found:" `startswith` msg                             -> Failure ["no such File(" ++ path ++ ")"]
      | otherwise                                                           -> Failure ["invalid yaml file"]
    otherwise                                                               -> Failure ["invalid yaml file"]

    where
      parseFailure :: SomeException -> IO (Either ParseException [RawObject])
      parseFailure e = return $ Left (InvalidYaml Nothing)
