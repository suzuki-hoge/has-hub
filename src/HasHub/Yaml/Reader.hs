module HasHub.Yaml.Reader
(
  readYaml
)
where


import Data.Yaml (decodeFileEither, ParseException(..), YamlException(..))
import Data.Aeson (FromJSON(..))
import Data.String.Utils (startswith)

import Control.Exception

import HasHub.FixMe (Validation(..), Error)


readYaml :: (FromJSON a) => (a -> b) -> FilePath -> IO (Validation [Error] [b])
readYaml mapper path = do
  e <- (decodeFileEither path >>= evaluate) `catch` failure

  return $ case e of
    Right xs                                     -> Success $ map mapper xs
    Left(InvalidYaml (Just (YamlException msg)))
      | "Yaml file not found:" `startswith` msg  -> Failure ["no such File(" ++ path ++ ")"]
      | otherwise                                -> Failure ["invalid yaml file"]
    _                                            -> Failure ["invalid yaml file"]

    where
      failure :: (FromJSON a) => SomeException -> IO (Either ParseException [a])
      failure e = return $ Left (InvalidYaml Nothing)
