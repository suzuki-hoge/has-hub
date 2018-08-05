module HasHub.Object.FixMe
(
  FixMe
, areAllContains
)
where


import Data.Either.Validation (Validation(..))


type Error = String


data FixMe = FixMe [String] [Error] deriving Show


areAllContains :: (Eq a, Show a) => [a] -> [a] -> Validation [FixMe] [a]
areAllContains haystacks needles = errorToFixMe merged haystacks
  where
    vs = map (contains haystacks) needles
    merged = if vs == [] then Success haystacks else foldl1 merge vs



errorToFixMe :: (Eq a, Show a) => Validation [Error] [a] -> [a] -> Validation [FixMe] [a]
errorToFixMe (Success ls) _ = Success ls
errorToFixMe (Failure es) allows = Failure [FixMe (map show allows) es]


contains :: (Eq a, Show a) => [a] -> a -> Validation [Error] [a]
contains haystacks needle =
  if needle `elem` haystacks
    then Success [needle]
    else Failure ["no such " ++ show needle]


merge :: Validation [Error] [a] -> Validation [Error] [a] -> Validation [Error] [a]
merge (Success x) (Success y) = Success x
merge (Success x) (Failure y) = Failure y
merge (Failure x) (Success y) = Failure x
merge (Failure x) (Failure y) = Failure $ x ++ y
