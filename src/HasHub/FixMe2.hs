module HasHub.FixMe2
(
  areAllIn
, Error2
, module Data.Either.Validation
)
where


import Data.Maybe (catMaybes)
import Data.Either.Validation (Validation(..))


type Error2 = String


areAllIn :: (Eq a, Show a) => [a] -> [a] -> Validation [Error2] ()
areAllIn needles haystacks = case catMaybes $ map (contains haystacks) needles of
  [] -> Success ()
  xs -> Failure xs

  where
    contains :: (Eq a, Show a) => [a] -> a -> Maybe Error2
    contains haystacks needle = if needle `elem` haystacks
      then Nothing
      else Just $ show needle

