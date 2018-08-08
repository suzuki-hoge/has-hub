module HasHub.Object.Object
(
  createEpic
, createIssue
, module HasHub.Object.Object.Data
)
where


import Text.Printf (printf)

import Data.Aeson (decode)
import Data.Maybe (fromJust)

import HasHub.Object.Object.Data

import HasHub.Client (Client, postGitHub, postZenHub, putZenHub, asRepositoryId, Resource)

import HasHub.Object.Milestone.Data as M
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import HasHub.Object.Pipeline.Data as P


createEpic :: Client -> [Milestone] -> [Pipeline] -> YamlObject -> IO Epic
createEpic client milestones pipelines (EpicYamlObject eln title body epicNumbers estimate milestoneTitle labels collaborators pipelineName) = do
  let milestone = milestones `M.filterBy` milestoneTitle
  let pipeline = pipelines `P.filterBy` pipelineName

  number <- createIssue client title body milestone collaborators labels epicNumbers pipeline estimate

  convertToEpic client number

  let (IssueNumber n) = number
  return $ Epic (EpicNumber n) title


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
