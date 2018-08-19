module HasHub.FixMe
(
  asJust
, isWritable
, areAllIn
, flat
, _message
, printMessages
, printFixMes
, FixMe(..)
, NotWritableError(..)
, NonExistentError(..)
, (??)
, module Data.Either.Validation
)
where


import System.Directory (doesPathExist, getPermissions, writable)
import System.FilePath.Posix (takeDirectory)

import System.Exit (die)

import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Either.Validation (Validation(..))


class FixMe a where
  toMessage :: a -> String


asJust :: Maybe a -> IO a
asJust (Just a) = return a
asJust Nothing  = do
  putStrLn "\nunexpected connection error."
  putStrLn "  please check log"
  die "\nhas-hub is aborted.\n"


data NonExistentError a = NonExistentError a deriving (Eq, Show)


areAllIn :: (Eq a) => [a] -> [a] -> Validation [NonExistentError a] ()
areAllIn needles haystacks = map (contains haystacks) needles ?? ()
  where
    contains :: (Eq a) => [a] -> a -> Maybe (NonExistentError a)
    contains haystacks needle = if needle `elem` haystacks
      then Nothing
      else Just $ NonExistentError needle


(??) :: [Maybe a] -> b -> Validation [a] b
(??) xs success = case catMaybes xs of
  [] -> Success success
  xs -> Failure xs


data NotWritableError = NotWritableFileError FilePath | NotWritableDirectoryError FilePath deriving (Eq, Show)

instance FixMe NotWritableError where
  toMessage (NotWritableFileError      fp) = "not writable file: " ++ fp
  toMessage (NotWritableDirectoryError dp) = "not writable directory: " ++ dp


data Checked = Writable | NotExist | NotWritable deriving (Eq, Show)


isWritable :: FilePath -> IO (Validation [NotWritableError] ())
isWritable fp = do
  let dp = takeDirectory fp
  fc <- check fp
  dc <- check dp

  return $ case (fc, dc) of
    (Writable,    _)        -> Success ()
    (NotExist,    Writable) -> Success ()
    (NotWritable, _)        -> Failure [NotWritableFileError fp]
    (NotExist,    _)        -> Failure [NotWritableDirectoryError dp]

    where
      check :: FilePath -> IO Checked
      check fp = do
        e <- doesPathExist fp
        if e
          then (\w -> if w then Writable else NotWritable) . writable <$> getPermissions fp
          else return NotExist


flat :: [Validation [a] b] -> Validation [a] ()
flat vs = case concatMap extract vs of
  [] -> Success ()
  xs -> Failure xs
  where
    extract :: Validation [a] b -> [a]
    extract (Success _ ) = []
    extract (Failure xs) = xs


_message :: (FixMe fm) => Validation [fm] () -> Validation [String] ()
_message (Success ()) = Success ()
_message (Failure xs) = Failure $ map toMessage xs


printMessages :: [String] -> IO ()
printMessages messages = do
  putStrLn "\nthere are several validation errors. please fix following errors."
  mapM_ (\error -> putStrLn $ "  " ++ error) messages
  die "\nhas-hub is aborted.\n"


printFixMes :: (FixMe fm) => [fm] -> IO ()
printFixMes = printMessages . map toMessage
