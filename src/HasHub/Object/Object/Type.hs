module HasHub.Object.Object.Type where


import Data.List.Utils (replace)


newtype EpicNumber = EpicNumber Int deriving (Eq)
instance Show EpicNumber where
  show (EpicNumber n) = "#" ++ show n


newtype EpicLinkNumber = EpicLinkNumber String deriving (Eq, Ord, Show)


data LinkingEpicNumber = SharpEpicNumber String
                       | QuestionEpicNumber String
                       deriving (Eq, Ord, Show)


data LinkedEpic = LinkedEpic EpicLinkNumber Epic deriving Show


data Epic = Epic EpicNumber Title deriving (Eq, Show)
instance Ord Epic where
  (Epic (EpicNumber n1) _) <  (Epic (EpicNumber n2) _) = n1 <  n2
  (Epic (EpicNumber n1) _) <= (Epic (EpicNumber n2) _) = n1 <= n2
  (Epic (EpicNumber n1) _) >  (Epic (EpicNumber n2) _) = n1 >  n2
  (Epic (EpicNumber n1) _) >= (Epic (EpicNumber n2) _) = n1 >= n2


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
type Linked = (LineNum, EpicLinkNumber)
type Linking = (LineNum, LinkingEpicNumber)


findIn :: [LinkedEpic] -> [Epic] -> LinkingEpicNumber -> [Epic]
findIn linkedEpics referredEpics number@(SharpEpicNumber _) = filtered
  where
    filtered :: [Epic]
    filtered = filter (\(Epic epicNumber _) -> epicNumber ==? number) referredEpics

    (==?) :: EpicNumber -> LinkingEpicNumber -> Bool
    (==?) (EpicNumber n) (SharpEpicNumber sen) = "#" ++ show n == sen

findIn linkedEpics referredEpics questionEpicNumber  = map _epic filtered
  where
    filtered :: [LinkedEpic]
    filtered = filter (\(LinkedEpic epicLinkNumber _) -> epicLinkNumber ==? questionEpicNumber) linkedEpics

    _epic :: LinkedEpic -> Epic
    _epic (LinkedEpic _ epic) = epic


_toEpicNumber :: String -> EpicNumber
_toEpicNumber s = EpicNumber $ (read . tail) s


(==?) :: EpicLinkNumber -> LinkingEpicNumber -> Bool
(==?) (EpicLinkNumber eln) (QuestionEpicNumber qen) = eln == qen
