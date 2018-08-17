module HasHub.Object.Object.Type where


import Data.List.Utils (replace)


newtype EpicNumber = EpicNumber Int deriving (Eq)
instance Show EpicNumber where
  show (EpicNumber n) = "#" ++ show n


newtype EpicLinkNumber = EpicLinkNumber String deriving (Eq, Ord, Show)


data ParentEpicNumber = SharpEpicNumber String
                      | QuestionEpicNumber String
                      deriving (Eq, Ord, Show)


data LinkedEpic = LinkedEpic EpicLinkNumber Epic deriving Show


data Epic = Epic EpicNumber Title deriving (Eq, Show)


newtype IssueNumber = IssueNumber Int deriving (Eq)
instance Show IssueNumber where
  show (IssueNumber n) = "#" ++ show n


newtype Title = Title String deriving (Eq, Show)


newtype Body = Body String deriving (Eq, Show)


newtype Estimate = Estimate Double deriving (Eq)
instance Show Estimate where
  show (Estimate d) = replace ".0" "" $ show d


_epicNumber :: IssueNumber -> EpicNumber
_epicNumber (IssueNumber n) = EpicNumber n


_number :: Epic -> EpicNumber
_number (Epic number _) = number


type LineNum = Int
type Definition = (LineNum, EpicLinkNumber)
type Parent = (LineNum, ParentEpicNumber)


findIn :: [LinkedEpic] -> [Epic] -> ParentEpicNumber -> [Epic]
findIn linkedEpics referredEpics number@(SharpEpicNumber _) = filtered
  where
    filtered :: [Epic]
    filtered = filter (\(Epic epicNumber _) -> epicNumber ==? number) referredEpics

    (==?) :: EpicNumber -> ParentEpicNumber -> Bool
    (==?) (EpicNumber n) (SharpEpicNumber sen) = "#" ++ show n == sen

findIn linkedEpics referredEpics questionEpicNumber  = map _epic filtered
  where
    filtered :: [LinkedEpic]
    filtered = filter (\(LinkedEpic epicLinkNumber _) -> epicLinkNumber ==? questionEpicNumber) linkedEpics

    _epic :: LinkedEpic -> Epic
    _epic (LinkedEpic _ epic) = epic


_toEpicNumber :: String -> EpicNumber
_toEpicNumber s = EpicNumber $ (read . tail) s


(==?) :: EpicLinkNumber -> ParentEpicNumber -> Bool
(==?) (EpicLinkNumber eln) (QuestionEpicNumber qen) = eln == qen
