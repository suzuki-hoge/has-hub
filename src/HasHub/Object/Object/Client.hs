module HasHub.Object.Object.Client
(
  referAll
, create
, module HasHub.Object.Object.Type
)
where


import HasHub.Object.Object.Parser
import HasHub.Object.Object.Type
import HasHub.Object.Pipeline.Type hiding (decodeJust)
import HasHub.Object.Label.Type hiding (decodeJust)
import HasHub.Object.Collaborator.Type hiding (decodeJust)
import HasHub.Object.Milestone.Type hiding (decodeJust)

import HasHub.Connection.Connector (getZenHub, postGitHub, postZenHub_, postZenHub'_, putZenHub_)


referAll :: IO [EpicNumber]
referAll = decodeJust <$> getZenHub ReferEpicResourcer


create :: YamlObject -> [Milestone] -> [Pipeline] -> [LinkedEpic] -> IO (Maybe LinkedEpic)
create (EpicYamlObject epicLinkNumber title body pipelineName labels collaborators milestoneTitles estimate parentEpicNumbers) milestones pipelines linkedEpics = do
  let milestone = Nothing -- todo integration
  let pipeline = Nothing  -- todo integration
  let epicNumbers = []    -- todo integration
  Just <$> createEpic epicLinkNumber title body pipeline labels collaborators milestone estimate epicNumbers
create (IssueYamlObject title body pipelineName labels collaborators milestoneTitles estimate parentEpicNumbers) milestones pipelines linkedEpics = do
  let milestone = Nothing -- todo integration
  let pipeline = Nothing  -- todo integration
  let epicNumbers = []    -- todo integration
  const Nothing <$> createIssue title body pipeline labels collaborators milestone estimate epicNumbers


createEpic :: EpicLinkNumber -> Title -> Body -> Maybe Pipeline -> [Label] -> [Collaborator] -> Maybe Milestone -> Maybe Estimate -> [EpicNumber] -> IO LinkedEpic
createEpic epicLinkNumber title body pipeline labels collaborators milestone estimate epicNumbers = do
  issueNumber <- createIssue title body pipeline labels collaborators milestone estimate epicNumbers

  epicNumber <- convertToEpic issueNumber

  return $ LinkedEpic epicLinkNumber epicNumber


createIssue :: Title -> Body -> Maybe Pipeline -> [Label] -> [Collaborator] -> Maybe Milestone -> Maybe Estimate -> [EpicNumber] -> IO IssueNumber
createIssue title body pipeline labels collaborators milestone estimate epicNumbers = do
  number <- decodeJust' <$> postGitHub (CreateIssueInput title body labels collaborators milestone)

  mapM_ (setPipeline number) pipeline
  mapM_ (setEstimate number) estimate
  mapM_ (setEpic number) epicNumbers

  return number


setPipeline :: IssueNumber -> Pipeline -> IO ()
setPipeline number pipeline = postZenHub_ $ SetPipelineInput number pipeline


setEstimate :: IssueNumber -> Estimate -> IO ()
setEstimate number estimate = putZenHub_ $ SetEstimateInput number estimate


setEpic :: IssueNumber -> EpicNumber -> IO ()
setEpic issueNumber epicNumber = postZenHub'_ $ SetEpicInput issueNumber epicNumber


convertToEpic :: IssueNumber -> IO EpicNumber
convertToEpic number = do
  postZenHub_ $ ConvertToEpicInput number

  return $ _epicNumber number
