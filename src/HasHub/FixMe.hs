module HasHub.FixMe
(
  areAllIn
, FixMe(..)
, NonExistentError(..)
, (??)
, module Data.Either.Validation
)
where


import Data.Maybe (mapMaybe, catMaybes)
import Data.Either.Validation (Validation(..))


class FixMe a where
  toMessage :: a -> String


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
