module HasHub.Object.Object.Validator
(
  areAllIn
)
where


import Text.Regex.Posix ((=~))

import HasHub.FixMe2 (Error2, Validation)
import qualified HasHub.FixMe2 as F (areAllIn)

import HasHub.Object.Object.Type


--type RowNum = Int
--type Definition = (RowNum, EpicLinkNumber2)
--type Parent = (RowNum, ParentEpicNumber2)
--
--
--validateLinking :: [Definition] -> [Parent] -> Validation [String] [()]
--validateLinking definitions parents = mergeAll (map validate parents) []
--  where
--    filterBy :: [Definition] -> Parent -> Maybe Definition
--    filterBy definitions parent = listToMaybe $ filter (cond parent) definitions
--      where
--        cond :: Parent -> Definition -> Bool
--        cond (_, QuestionEpicNumber2(qen)) (_, EpicLinkNumber2(eln)) = qen == eln
--        cond _ _ = False
--
--    validate :: Parent -> Validation [String] [()]
--    validate (pn, eln) = case filterBy definitions (pn, eln) of
--      Just (dn, _) -> if dn < pn then Success [()] else Failure ["invalid order"]
--      Nothing -> Failure ["no link"]
--
--
--epicLinkNumberFormat :: [EpicLinkNumber2] -> Validation [Error2] ()
--epicLinkNumberFormat elns = mergeAll (map validate elns) []
--  where
--    validate :: EpicLinkNumber2 -> Validation [Error2] ()
--    validate (EpicLinkNumber2 s) = if s `isNumberedBy` '?' then Success () else Failure ["format error"] -- todo (FormatError, ?1)
--
--
--
--parentEpicNumberFormat :: [ParentEpicNumber2] -> Validation [Error2] [()]
--parentEpicNumberFormat pens = mergeAll (map validate pens) []
--  where
--    validate :: ParentEpicNumber2 -> Validation [Error2] ()
--    validate (SharpEpicNumber2 s) = if s `isNumberedBy` '#' then Success () else Failure ["format error"] -- todo (FormatError, ?1)
--    validate (QuestionEpicNumber2 s) = if s `isNumberedBy` '?' then Success () else Failure ["format error"] -- todo (FormatError, ?1)
--
--
--isNumberedBy :: String -> Char -> Bool
--isNumberedBy s c = s =~ ("^\\" ++ [c] ++ "[0-9]+$")
--
--
--validateNoDuplicate :: [EpicLinkNumber2] -> Validation [Error2] ()
--validateNoDuplicate xs = if dups == [] then Success () else Failure ["dup"]
--  where
--    dups = sort xs \\ (nub . sort) xs


areAllIn :: [ParentEpicNumber2] -> [EpicNumber2] -> Validation [Error2] ()
areAllIn needles haystacks = (concatMap toEpicNumberIfSharp needles) `F.areAllIn` haystacks
  where
    toEpicNumberIfSharp :: ParentEpicNumber2 -> [EpicNumber2]
    toEpicNumberIfSharp (SharpEpicNumber2 s) = [EpicNumber2 $ (read . tail) s]
    toEpicNumberIfSharp (QuestionEpicNumber2 _) = []
