module HasHub.Object.Object.Client
(
  referAll
, create
, module HasHub.Object.Object.Type
)
where


import HasHub.Object.Object.Parser
import HasHub.Object.Object.IOType
import HasHub.Object.Object.Type

import HasHub.Connection.Connector (getGitHub, getZenHub, postGitHub, postZenHub_, postZenHub'_, putZenHub_)


referAll :: IO [Epic]
referAll = do
  outputs <- asIssueOutputs <$> getGitHub ReferIssueInput
  epicNumbers <- asEpicNumbers <$> getZenHub ReferEpicInput

  return . map _epic . filter (isEpic epicNumbers) $ outputs


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
  number <- asIssueNumber <$> postGitHub (CreateIssueInput title body labels collaborators milestone)

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
