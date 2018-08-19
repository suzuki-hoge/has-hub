module HasHub.FixMe
(
  asJust
, areAllIn
, flat
, _message
, printMessages
, printFixMes
, FixMe(..)
, NonExistentError(..)
, (??)
, module Data.Either.Validation
)
where


import System.Exit (die)

import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Either.Validation (Validation(..))


class FixMe a where
  toMessage :: a -> String


data NonExistentError a = NonExistentError a deriving (Eq, Show)


asJust :: Maybe a -> IO a
asJust (Just a) = return a
asJust Nothing  = do
  putStrLn "\nunexpected connection error."
  putStrLn "  please check log"
  die "\nhas-hub is aborted.\n"


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
