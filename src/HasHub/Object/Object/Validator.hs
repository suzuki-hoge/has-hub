{-# LANGUAGE FlexibleInstances #-}


module HasHub.Object.Object.Validator
(
  areAllIn
, noDuplication
, linkNumberFormat
, parentNumberFormat
, linking
, DuplicationError(..)
, FormatError(..)
, LinkError(..)
, module HasHub.FixMe
)
where


import Text.Regex.Posix ((=~))

import Data.List (nub, sort, (\\), find)
import Data.Maybe (mapMaybe)

import HasHub.Object.Object.Type
import qualified HasHub.Object.Object.Type as T ((==?))

import HasHub.FixMe (Validation(..), FixMe(..), NonExistentError(..))
import qualified HasHub.FixMe as F (areAllIn, (??))


instance FixMe (NonExistentError EpicNumber) where
  toMessage (NonExistentError (EpicNumber n)) = "no such epic: #" ++ show n


areAllIn :: [ParentEpicNumber] -> [Epic] -> Validation [NonExistentError EpicNumber] ()
areAllIn needles haystacks = mapMaybe toEpicNumberIfSharp needles `F.areAllIn` map _number haystacks
  where
    toEpicNumberIfSharp :: ParentEpicNumber -> Maybe EpicNumber
    toEpicNumberIfSharp (QuestionEpicNumber _) = Nothing
    toEpicNumberIfSharp (SharpEpicNumber s)
      | s `_isNumberedBy` '#' = Just $ _toEpicNumber s
      | otherwise             = Nothing


newtype DuplicationError = DuplicationError EpicLinkNumber deriving (Eq, Show)

instance FixMe DuplicationError where
  toMessage (DuplicationError (EpicLinkNumber s)) = "duplicate definition: " ++ s


noDuplication :: [EpicLinkNumber] -> Validation [DuplicationError] ()
noDuplication numbers = case dups of
  [] -> Success ()
  xs -> Failure $ map DuplicationError xs
  where
    dups = sort numbers \\ (nub . sort) numbers


newtype FormatError a = FormatError a deriving (Eq, Show) -- todo refactor for milestone creation.

instance FixMe (FormatError EpicLinkNumber) where
  toMessage (FormatError (EpicLinkNumber s)) = "not satisfied ^?\\d$ format: " ++ s


linkNumberFormat :: [EpicLinkNumber] -> Validation [FormatError EpicLinkNumber] ()
linkNumberFormat numbers = map validate numbers F.?? ()
  where
    validate :: EpicLinkNumber -> Maybe (FormatError EpicLinkNumber)
    validate number@(EpicLinkNumber s) = s `isNumberedBy` '?' $ number


instance FixMe (FormatError ParentEpicNumber) where
  toMessage (FormatError (SharpEpicNumber s))    = "not satisfied ^#\\d$ format: " ++ s
  toMessage (FormatError (QuestionEpicNumber s)) = "not satisfied ^?\\d$ format: " ++ s


parentNumberFormat :: [ParentEpicNumber] -> Validation [FormatError ParentEpicNumber] ()
parentNumberFormat numbers = map validate numbers F.?? ()
  where
    validate :: ParentEpicNumber -> Maybe (FormatError ParentEpicNumber)
    validate number@(SharpEpicNumber s)    = s `isNumberedBy` '#' $ number
    validate number@(QuestionEpicNumber s) = s `isNumberedBy` '?' $ number


isNumberedBy :: String -> Char -> (a -> Maybe (FormatError a))
isNumberedBy s c number = if s `_isNumberedBy` c
  then Nothing
  else Just $ FormatError number


_isNumberedBy :: String -> Char -> Bool
_isNumberedBy s c = s =~ ("^\\" ++ [c] ++ "[0-9]+$")

_isNotNumberedBy :: String -> Char -> Bool
_isNotNumberedBy s c = not $ s `_isNumberedBy` c


data LinkError = DefineLineError Definition Parent | NotDefinedError Parent deriving (Eq, Show)

instance FixMe LinkError where
  toMessage (DefineLineError (dn, _) (pn, QuestionEpicNumber s)) = "can't resolve definition link: use " ++ s ++ " on line " ++ show pn ++ ", but " ++ s ++ " is defined at line " ++ show dn
  toMessage (NotDefinedError         (pn, QuestionEpicNumber s)) = "can't resolve definition link: use " ++ s ++ " on line " ++ show pn ++ ", but " ++ s ++ " is not defined"


linking :: [Definition] -> [Parent] -> Validation [LinkError] ()
linking definitions parents = map (validate definitions) parents F.?? ()
  where
    validate :: [Definition] -> Parent -> Maybe LinkError
    validate definitions        (_, SharpEpicNumber _) = Nothing
    validate definitions parent@(_, QuestionEpicNumber qen)
      | qen `_isNotNumberedBy` '?' = Nothing
      | otherwise                  = case find (==? parent) definitions of
        Just definition
          | definition <? parent  -> Nothing
          | otherwise             -> Just $ DefineLineError definition parent
        Nothing                   -> Just $ NotDefinedError parent
      where
        (<?) :: Definition -> Parent -> Bool
        (<?) (dn, _) (pn, _) = dn < pn

        (==?) :: Definition -> Parent -> Bool
        (==?) (_, epicLinkNumber) (_, questionEpicNumber) = epicLinkNumber T.==? questionEpicNumber
