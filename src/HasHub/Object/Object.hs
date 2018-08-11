module HasHub.Object.Object
(
  validateAllExists
, validateNoDuplicate
, validateLinking
, epicLinkNumberFormat
, parentEpicNumberFormat
, createEpic
, createIssue
, module HasHub.Object.Object.Data
)
where


import Data.Either.Validation (Validation(..))

import Text.Printf (printf)

import Data.List (nub, sort, (\\))

import Data.Aeson (decode)
import Data.Maybe (fromJust, listToMaybe)

import HasHub.Object.Object.Data

import HasHub.Client (Client, postGitHub, getZenHub, postZenHub, putZenHub, asRepositoryId, Resource)

import HasHub.Object.Milestone.Data as M
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import HasHub.Object.Pipeline.Data as P hiding (parseInList)

import HasHub.Object.FixMe (FixMe(..), areAllContains, mergeAll)


type RowNum = Int
type Definition = (RowNum, EpicLinkNumber)
type Parent = (RowNum, ParentEpicNumber)


validateLinking :: [Definition] -> [Parent] -> Validation [String] [()]
validateLinking definitions parents = mergeAll (map validate parents) []
  where
    filterBy :: [Definition] -> Parent -> Maybe Definition
    filterBy definitions parent = listToMaybe $ filter (cond parent) definitions
      where
        cond :: Parent -> Definition -> Bool
        cond (_, QuestionEpicNumber(qen)) (_, EpicLinkNumber(eln)) = qen == eln
        cond _ _ = False

    validate :: Parent -> Validation [String] [()]
    validate (pn, eln) = case filterBy definitions (pn, eln) of
      Just (dn, _) -> if dn < pn then Success [()] else Failure ["invalid order"]
      Nothing -> Failure ["no link"]


epicLinkNumberFormat :: [EpicLinkNumber] -> Validation [String] [()]
epicLinkNumberFormat elns = mergeAll (map validate elns) []
  where
    validate :: EpicLinkNumber -> Validation [String] [()]
    validate (EpicLinkNumber s) = if s `isNumberedBy` '?' then Success [()] else Failure ["format error"] -- todo (FormatError, ?1)


parentEpicNumberFormat :: [ParentEpicNumber] -> Validation [String] [()]
parentEpicNumberFormat pens = mergeAll (map validate pens) []
  where
    validate :: ParentEpicNumber -> Validation [String] [()]
    validate (SharpEpicNumber s) = if s `isNumberedBy` '#' then Success [()] else Failure ["format error"] -- todo (FormatError, ?1)
    validate (QuestionEpicNumber s) = if s `isNumberedBy` '?' then Success [()] else Failure ["format error"] -- todo (FormatError, ?1)


validateNoDuplicate :: [EpicLinkNumber] -> Validation [FixMe] ()
validateNoDuplicate xs = if dups == [] then Success () else Failure [FixMe [""] (map show dups)]
  where
    dups = sort xs \\ (nub . sort) xs


validateAllExists :: Client -> [ParentEpicNumber] -> IO (Validation [FixMe] [EpicNumber])
validateAllExists client needles = do
  putStrLn "\nvalidate ParentEpicNumbers"

  haystacks <- getAll client

  return $ haystacks `areAllContains` (concatMap toEpicNumberIfSharp needles)

    where
      toEpicNumberIfSharp :: ParentEpicNumber -> [EpicNumber]
      toEpicNumberIfSharp (SharpEpicNumber s) = [EpicNumber $ (read . tail) s]
      toEpicNumberIfSharp (QuestionEpicNumber _) = []



getAll :: Client -> IO [EpicNumber]
getAll client = do
  putStrLn "  fetch all EpicNumbers"

  json <- getZenHub client "/epics"

  return . fromJust $ (parseInList json)


createEpic :: Client -> [Milestone] -> [Pipeline] -> [LinkedEpic] -> YamlObject -> IO LinkedEpic
createEpic client milestones pipelines links (EpicYamlObject eln title body epicLinkNumbers estimate milestoneTitle labels collaborators pipelineName) = do
  let milestone = M.intersect milestones milestoneTitle
  let pipeline = P.intersect pipelines pipelineName
  let epicNumbers = []
  -- let epicNumbers = fixEpicNumbers links epicLinkNumbers

  number <- createIssue client title body milestone collaborators labels epicNumbers pipeline estimate

  convertToEpic client number

  let (IssueNumber n) = number
  return $ LinkedEpic eln (EpicNumber n)


createIssue :: Client -> Title -> Body -> (Maybe Milestone) -> [Collaborator] -> [Label] -> [EpicNumber] -> (Maybe Pipeline) -> (Maybe Estimate) -> IO IssueNumber
createIssue client title body milestone collaborators labels epicNumbers pipeline estimate = do
  printf "create issue(%s) -> " (show title)

  json <- postGitHub client "/issues" $ CreateIssueInput title body milestone collaborators labels
  let number = fromJust $ (decode json :: Maybe IssueNumber)

  printf "%s\n" (show number)

  mapM_ (setEpic client number) epicNumbers           -- todo order by g -> z
  mapM_ (setPipeline client number) pipeline
  mapM_ (setEstimate client number) estimate

  return number


setEpic :: Client -> IssueNumber -> EpicNumber -> IO ()
setEpic client iNumber eNumber = do
   printf "[%s] set parent epic(%s)\n" (show iNumber) (show eNumber)

   json <- postZenHub client (toResource eNumber) $ SetEpicInput iNumber (asRepositoryId client)

   return ()

     where
       toResource :: EpicNumber -> Resource
       toResource (EpicNumber n) = "/epics/" ++ (show $ n) ++ "/update_issues"



setPipeline :: Client -> IssueNumber -> Pipeline -> IO ()
setPipeline client number pipeline = do
  printf "[%s] move to pipeline(%s)\n" (show number) (show pipeline)

  json <- postZenHub client (toResource number) $ SetPipelineInput pipeline

  return ()

    where
      toResource :: IssueNumber -> Resource
      toResource (IssueNumber number) = "/issues/" ++ (show $ number) ++ "/moves"


setEstimate :: Client -> IssueNumber -> Estimate -> IO ()
setEstimate client number estimate = do
  printf "[%s] set estimate(%s)\n" (show number) (show estimate)

  json <- putZenHub client (toResource number) $ SetEstimateInput estimate

  return ()

    where
      toResource :: IssueNumber -> Resource
      toResource (IssueNumber n) = "/issues/" ++ (show $ n) ++ "/estimate"


convertToEpic :: Client -> IssueNumber -> IO ()
convertToEpic client number = do
  printf "[%s] convert to epic\n" (show number)

  json <- postZenHub client (toResource number) $ ConvertToEpicInput

  return ()

    where
      toResource :: IssueNumber -> Resource
      toResource (IssueNumber number) = "/issues/" ++ (show $ number) ++ "/convert_to_epic"
