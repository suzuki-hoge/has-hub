module HasHub.Yaml.Reader
(
  readYamls
, YamlReadingError(..)
, module HasHub.FixMe
)
where


import Text.Printf (printf)

import Data.Yaml (decodeFileEither, ParseException(..), YamlException(..))
import Data.Aeson (FromJSON(..))
import Data.String.Utils (startswith)

import Control.Exception

import HasHub.FixMe (FixMe(..), Validation(..))


data YamlReadingError = YamlParseError | NoYamlError FilePath deriving (Eq, Show)

instance FixMe YamlReadingError where
  toMessage  YamlParseError  = "yaml parse error"
  toMessage (NoYamlError fp) = "no yaml error: " ++ fp


readYamls :: (FromJSON a) => (a -> b) -> [FilePath] -> IO (Validation [YamlReadingError] [b])
readYamls mapper fps = mergeYamls <$> mapM (readYaml mapper) fps


readYaml :: (FromJSON a) => (a -> b) -> FilePath -> IO (Validation [YamlReadingError] [b])
readYaml mapper fp = do
  putStrLn "\nparse yaml file."
  printf "  %s\n" fp


  e <- (decodeFileEither fp >>= evaluate) `catch` failure

  return $ case e of
    Right xs                                     -> Success $ map mapper xs
    Left(InvalidYaml (Just (YamlException msg)))
      | "Yaml file not found:" `startswith` msg  -> Failure [NoYamlError fp]
      | otherwise                                -> Failure [YamlParseError]
    _                                            -> Failure [YamlParseError]

    where
      failure :: (FromJSON a) => SomeException -> IO (Either ParseException [a])
      failure e = return $ Left (InvalidYaml Nothing)


mergeYamls :: [Validation [YamlReadingError] [b]] -> Validation [YamlReadingError] [b]
mergeYamls = foldl1 merge
  where
    merge (Success xs) (Success ys) = Success $ xs ++ ys
    merge (Failure xs) (Success __) = Failure xs
    merge (Success __) (Failure ys) = Failure ys
    merge (Failure xs) (Failure ys) = Failure $ xs ++ ys
