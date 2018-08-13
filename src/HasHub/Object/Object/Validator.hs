module HasHub.Object.Object.Validator
(
  areAllIn
, noDuplication
, linkNumberFormat
, parentNumberFormat
, linking
, module HasHub.FixMe
)
where


import Text.Regex.Posix ((=~))

import Data.List (nub, sort, (\\))
import Data.Maybe (mapMaybe, listToMaybe)

import HasHub.Object.Object.Type

import HasHub.FixMe (Error, Validation(..))
import qualified HasHub.FixMe as F (areAllIn)


areAllIn :: [ParentEpicNumber] -> [Epic] -> Validation [Error] ()
areAllIn needles haystacks = concatMap toEpicNumberIfSharp needles `F.areAllIn` map _number haystacks
  where
    toEpicNumberIfSharp :: ParentEpicNumber -> [EpicNumber]
    toEpicNumberIfSharp (SharpEpicNumber s) = [EpicNumber $ (read . tail) s]
    toEpicNumberIfSharp (QuestionEpicNumber _) = []


noDuplication :: [EpicLinkNumber] -> Validation [Error] ()
noDuplication numbers = case dups of
  [] -> Success ()
  xs -> Failure $ map (\(EpicLinkNumber s) -> "duplicate error: " ++ s) xs
  where
    dups = sort numbers \\ (nub . sort) numbers


linkNumberFormat :: [EpicLinkNumber] -> Validation [Error] () -- todo (FormatError, ?1)
linkNumberFormat numbers = case mapMaybe validate numbers of
  [] -> Success () -- todo delegate to FixMe
  xs -> Failure xs
  where
    validate :: EpicLinkNumber -> Maybe Error
    validate (EpicLinkNumber s) = s `isNumberedBy` '?'


parentNumberFormat :: [ParentEpicNumber] -> Validation [Error] () -- todo (FormatError, ?1)
parentNumberFormat numbers = case mapMaybe validate numbers of
  [] -> Success () -- todo delegate to FixMe
  xs -> Failure xs
  where
    validate :: ParentEpicNumber -> Maybe Error
    validate (SharpEpicNumber s)    = s `isNumberedBy` '#'
    validate (QuestionEpicNumber s) = s `isNumberedBy` '?'


isNumberedBy :: String -> Char -> Maybe Error
isNumberedBy s c = if s =~ ("^\\" ++ [c] ++ "[0-9]+$")
  then Nothing
  else Just $ "format error: " ++ s


type LineNum = Int
type Definition = (LineNum, EpicLinkNumber)
type Parent = (LineNum, ParentEpicNumber)

linking :: [Definition] -> [Parent] -> Validation [Error] ()
linking definitions parents = case mapMaybe (validate definitions) parents of
  [] -> Success () -- todo delegate to FixMe
  xs -> Failure xs
  where
    validate :: [Definition] -> Parent -> Maybe Error
    validate definitions (_, (SharpEpicNumber _)) = Nothing
    validate definitions parent = case definitions `filterBy` parent of
      Just (dn, _) -> if dn < _n parent then Nothing else Just $ "use " ++ _s parent ++ " on line " ++ (show . _n) parent ++ ", but " ++ _s parent ++ " is defined at line " ++ show dn
      Nothing -> Just $ "use " ++ _s parent ++ " on line " ++ (show . _n) parent ++ ", but " ++ _s parent ++ " is not defined"
      where
        filterBy :: [Definition] -> Parent -> Maybe Definition
        filterBy definitions parent = listToMaybe $ filter (cond parent) definitions
          where
            cond :: Parent -> Definition -> Bool
            cond (_, QuestionEpicNumber(n1)) (_, EpicLinkNumber(n2)) = n1 == n2
        _n :: Parent -> LineNum
        _n (n, _) = n

        _s :: Parent -> String
        _s (_, QuestionEpicNumber(s)) = s


-- todo instance FixMe ???
