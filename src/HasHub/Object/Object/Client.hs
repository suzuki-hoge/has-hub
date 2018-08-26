module HasHub.Object.Object.Client
(
  referAll
, createEpic
, createIssue
, module HasHub.Object.Object.Type
)
where


import Text.Printf (printf)

import HasHub.Object.Object.IOType
import HasHub.Object.Object.Type

import HasHub.Object.Pipeline.Type
import HasHub.Object.Label.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Milestone.Type

import HasHub.Connection.Connector (getGitHub, getZenHub, postGitHub, postZenHub_, postZenHub'_, putZenHub_)



referAll :: IO [Epic]
referAll = do
  putStrLn "  refer all Epics"

  getGitHub ReferEpicsInput asEpics


createEpic :: EpicLinkNumber -> Title -> Body -> Maybe Pipeline -> [Label] -> [Collaborator] -> Maybe Milestone -> Maybe Estimate -> [Epic] -> IO LinkedEpic
createEpic epicLinkNumber title body pipeline labels collaborators milestone estimate epics = do
  issueNumber <- createIssue title body pipeline labels collaborators milestone estimate epics

  epicNumber <- convertToEpic issueNumber

  return $ LinkedEpic epicLinkNumber (Epic epicNumber title)


createIssue :: Title -> Body -> Maybe Pipeline -> [Label] -> [Collaborator] -> Maybe Milestone -> Maybe Estimate -> [Epic] -> IO IssueNumber
createIssue title@(Title t) body pipeline labels collaborators milestone estimate epics = do
  printf "  create Issue(%s) -> " t

  number <- postGitHub (CreateIssueInput title body labels collaborators milestone) >>= asIssueNumber

  printf "[%s]\n" (show number)

  mapM_ (setPipeline number) pipeline
  mapM_ (setEstimate number) estimate
  mapM_ (setEpic number) epics

  return number


setPipeline :: IssueNumber -> Pipeline -> IO ()
setPipeline number pipeline@(Pipeline _ (PipelineName n)) = do
  printf "  [%s] set Pipeline(%s)\n" (show number) n

  postZenHub_ $ SetPipelineInput number pipeline


setEstimate :: IssueNumber -> Estimate -> IO ()
setEstimate number estimate = do
  printf "  [%s] set Estimate(%s)\n" (show number) (show estimate)

  putZenHub_ $ SetEstimateInput number estimate


setEpic :: IssueNumber -> Epic -> IO ()
setEpic issueNumber (Epic epicNumber (Title t)) = do
  printf "  [%s] set Epic(%s %s)\n" (show issueNumber) (show epicNumber) t

  postZenHub'_ $ SetEpicInput issueNumber epicNumber


convertToEpic :: IssueNumber -> IO EpicNumber
convertToEpic number = do
  printf "  [%s] convert to Epic\n" (show number)

  postZenHub_ $ ConvertToEpicInput number

  return $ _epicNumber number
