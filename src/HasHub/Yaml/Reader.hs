module HasHub.Yaml.Reader
(
  readYaml
, YamlReadingError(..)
, module HasHub.FixMe
)
where


import Data.Yaml (decodeFileEither, ParseException(..), YamlException(..))
import Data.Aeson (FromJSON(..))
import Data.String.Utils (startswith)

import Control.Exception

import HasHub.FixMe (FixMe(..), Validation(..))


data YamlReadingError = YamlParseError | NoYamlError FilePath deriving (Eq, Show)

instance FixMe YamlReadingError where
  toMessage (YamlParseError) = "yaml parse error"
  toMessage (NoYamlError fp) = "no yaml error: " ++ fp


readYaml :: (FromJSON a) => (a -> b) -> FilePath -> IO (Validation [YamlReadingError] [b])
readYaml mapper path = do
  e <- (decodeFileEither path >>= evaluate) `catch` failure

  return $ case e of
    Right xs                                     -> Success $ map mapper xs
    Left(InvalidYaml (Just (YamlException msg)))
      | "Yaml file not found:" `startswith` msg  -> Failure [NoYamlError path]
      | otherwise                                -> Failure [YamlParseError]
    _                                            -> Failure [YamlParseError]

    where
      failure :: (FromJSON a) => SomeException -> IO (Either ParseException [a])
      failure e = return $ Left (InvalidYaml Nothing)
