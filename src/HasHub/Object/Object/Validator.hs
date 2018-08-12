module HasHub.Object.Object.Validator
(
  areAllIn
)
where


import Text.Regex.Posix ((=~))

import HasHub.FixMe (Error, Validation)
import qualified HasHub.FixMe as F (areAllIn)

import HasHub.Object.Object.Type


--type RowNum = Int
--type Definition = (RowNum, EpicLinkNumber)
--type Parent = (RowNum, ParentEpicNumber)
--
--
--validateLinking :: [Definition] -> [Parent] -> Validation [String] [()]
--validateLinking definitions parents = mergeAll (map validate parents) []
--  where
--    filterBy :: [Definition] -> Parent -> Maybe Definition
--    filterBy definitions parent = listToMaybe $ filter (cond parent) definitions
--      where
--        cond :: Parent -> Definition -> Bool
--        cond (_, QuestionEpicNumber(qen)) (_, EpicLinkNumber(eln)) = qen == eln
--        cond _ _ = False
--
--    validate :: Parent -> Validation [String] [()]
--    validate (pn, eln) = case filterBy definitions (pn, eln) of
--      Just (dn, _) -> if dn < pn then Success [()] else Failure ["invalid order"]
--      Nothing -> Failure ["no link"]
--
--
--epicLinkNumberFormat :: [EpicLinkNumber] -> Validation [Error] ()
--epicLinkNumberFormat elns = mergeAll (map validate elns) []
--  where
--    validate :: EpicLinkNumber -> Validation [Error] ()
--    validate (EpicLinkNumber s) = if s `isNumberedBy` '?' then Success () else Failure ["format error"] -- todo (FormatError, ?1)
--
--
--
--parentEpicNumberFormat :: [ParentEpicNumber] -> Validation [Error] [()]
--parentEpicNumberFormat pens = mergeAll (map validate pens) []
--  where
--    validate :: ParentEpicNumber -> Validation [Error] ()
--    validate (SharpEpicNumber s) = if s `isNumberedBy` '#' then Success () else Failure ["format error"] -- todo (FormatError, ?1)
--    validate (QuestionEpicNumber s) = if s `isNumberedBy` '?' then Success () else Failure ["format error"] -- todo (FormatError, ?1)
--
--
--isNumberedBy :: String -> Char -> Bool
--isNumberedBy s c = s =~ ("^\\" ++ [c] ++ "[0-9]+$")
--
--
--validateNoDuplicate :: [EpicLinkNumber] -> Validation [Error] ()
--validateNoDuplicate xs = if dups == [] then Success () else Failure ["dup"]
--  where
--    dups = sort xs \\ (nub . sort) xs


areAllIn :: [ParentEpicNumber] -> [EpicNumber] -> Validation [Error] ()
areAllIn needles haystacks = (concatMap toEpicNumberIfSharp needles) `F.areAllIn` haystacks
  where
    toEpicNumberIfSharp :: ParentEpicNumber -> [EpicNumber]
    toEpicNumberIfSharp (SharpEpicNumber s) = [EpicNumber $ (read . tail) s]
    toEpicNumberIfSharp (QuestionEpicNumber _) = []
