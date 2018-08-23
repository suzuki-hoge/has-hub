{-# LANGUAGE FlexibleInstances #-}


module HasHub.Object.Object.Validator
(
  areAllIn
, noDuplication
, linkNumberFormat
, linkingNumberFormat
, linkTo
, DuplicationError(..)
, FormatError(..)
, LinkError(..)
, module HasHub.FixMe
, module HasHub.Object.Object.Type
)
where


import Text.Regex.Posix ((=~))

import Data.List (nub, sort, (\\), find)
import Data.Maybe (mapMaybe)

import HasHub.Object.Object.Type
import qualified HasHub.Object.Object.Type as T ((==?))

import HasHub.FixMe (Validation(..), FixMe(..), NonExistentError(..), FormatError(..))
import qualified HasHub.FixMe as F (areAllIn, (??))


instance FixMe (NonExistentError EpicNumber) where
  toMessage (NonExistentError (EpicNumber n)) = "no such epic: #" ++ show n


areAllIn :: [LinkingEpicNumber] -> [Epic] -> Validation [NonExistentError EpicNumber] ()
areAllIn needles haystacks = mapMaybe toEpicNumberIfSharp needles `F.areAllIn` map _number haystacks
  where
    toEpicNumberIfSharp :: LinkingEpicNumber -> Maybe EpicNumber
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


instance FixMe (FormatError EpicLinkNumber) where
  toMessage (FormatError (EpicLinkNumber s)) = "not satisfied ^?\\d$ format: " ++ s


linkNumberFormat :: [EpicLinkNumber] -> Validation [FormatError EpicLinkNumber] ()
linkNumberFormat numbers = map validate numbers F.?? ()
  where
    validate :: EpicLinkNumber -> Maybe (FormatError EpicLinkNumber)
    validate number@(EpicLinkNumber s) = s `isNumberedBy` '?' $ number


instance FixMe (FormatError LinkingEpicNumber) where
  toMessage (FormatError (SharpEpicNumber s))    = "not satisfied ^#\\d$ format: " ++ s
  toMessage (FormatError (QuestionEpicNumber s)) = "not satisfied ^?\\d$ format: " ++ s


linkingNumberFormat :: [LinkingEpicNumber] -> Validation [FormatError LinkingEpicNumber] ()
linkingNumberFormat numbers = map validate numbers F.?? ()
  where
    validate :: LinkingEpicNumber -> Maybe (FormatError LinkingEpicNumber)
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


data LinkError = DefineLineError Linked Linking | NotDefinedError Linking deriving (Eq, Show)

instance FixMe LinkError where
  toMessage (DefineLineError (dn, _) (pn, QuestionEpicNumber s)) = "can't resolve linking epic: use " ++ s ++ " on line " ++ show pn ++ ", but " ++ s ++ " is defined at line " ++ show dn
  toMessage (NotDefinedError         (pn, QuestionEpicNumber s)) = "can't resolve linking epic: use " ++ s ++ " on line " ++ show pn ++ ", but " ++ s ++ " is not defined"


linkTo :: [Linking] -> [Linked] -> Validation [LinkError] ()
linkTo linkings linkeds = map (validate linkeds) linkings F.?? ()
  where
    validate :: [Linked] -> Linking -> Maybe LinkError
    validate linkeds         (_, SharpEpicNumber _) = Nothing
    validate linkeds linking@(_, QuestionEpicNumber qen)
      | qen `_isNotNumberedBy` '?' = Nothing
      | otherwise                  = case find (==? linking) linkeds of
        Just linkeds
          | linkeds <? linking    -> Nothing
          | otherwise             -> Just $ DefineLineError linkeds linking
        Nothing                   -> Just $ NotDefinedError linking
      where
        (<?) :: Linked -> Linking -> Bool
        (<?) (dn, _) (pn, _) = dn < pn

        (==?) :: Linked -> Linking -> Bool
        (==?) (_, epicLinkNumber) (_, questionEpicNumber) = epicLinkNumber T.==? questionEpicNumber
