module HasHub.Object.Object.Type where


newtype EpicNumber = EpicNumber Int deriving (Eq, Show)


newtype EpicLinkNumber = EpicLinkNumber String deriving (Eq, Ord, Show)


data ParentEpicNumber = SharpEpicNumber String
                      | QuestionEpicNumber String
                      deriving (Eq, Ord, Show)


data LinkedEpic = LinkedEpic EpicLinkNumber EpicNumber deriving Show


newtype IssueNumber = IssueNumber Int deriving (Eq, Show)


newtype Title = Title String deriving (Eq, Show)


newtype Body = Body String deriving (Eq, Show)


newtype Estimate = Estimate Double deriving (Eq, Show)


_epicNumber :: IssueNumber -> EpicNumber
_epicNumber (IssueNumber n) = EpicNumber n
