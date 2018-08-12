module HasHub.Object.Object.Client
(
  referAll
, create
, module HasHub.Object.Object.Type
)
where


import HasHub.Connection.Connector (getZenHub, postGitHub, postZenHub_, postZenHub'_, putZenHub_)

import HasHub.Object.Object.Parser
import HasHub.Object.Object.Type
import HasHub.Object.Pipeline.Type hiding (decodeJust)
import HasHub.Object.Label.Type hiding (decodeJust)
import HasHub.Object.Collaborator.Type hiding (decodeJust)
import HasHub.Object.Milestone.Type hiding (decodeJust)


referAll :: IO [EpicNumber2]
referAll = decodeJust <$> getZenHub "/epics"


create :: YamlObject2 -> [Milestone2] -> [Pipeline2] -> [LinkedEpic2] -> IO [LinkedEpic2]
create (EpicYamlObject2 epicLinkNumber title body pipelineName labels collaborators milestoneTitles estimate parentEpicNumbers) milestones pipelines linkedEpics = do
  let milestone = Nothing -- todo integration
  let pipeline = Nothing  -- todo integration
  let epicNumbers = []    -- todo integration
  (\x -> [x]) <$> createEpic epicLinkNumber title body pipeline labels collaborators milestone estimate epicNumbers
create (IssueYamlObject2 title body pipelineName labels collaborators milestoneTitles estimate parentEpicNumbers) milestones pipelines linkedEpics = do
  let milestone = Nothing -- todo integration
  let pipeline = Nothing  -- todo integration
  let epicNumbers = []    -- todo integration
  (\_ -> []) <$> createIssue title body pipeline labels collaborators milestone estimate epicNumbers


createEpic :: EpicLinkNumber2 -> Title2 -> Body2 -> (Maybe Pipeline2) -> [Label2] -> [Collaborator2] -> (Maybe Milestone2) -> (Maybe Estimate2) -> [EpicNumber2] -> IO LinkedEpic2
createEpic epicLinkNumber title body pipeline labels collaborators milestone estimate epicNumbers = do
  issueNumber <- createIssue title body pipeline labels collaborators milestone estimate epicNumbers

  epicNumber <- convertToEpic issueNumber

  return $ LinkedEpic2 epicLinkNumber epicNumber


createIssue :: Title2 -> Body2 -> (Maybe Pipeline2) -> [Label2] -> [Collaborator2] -> (Maybe Milestone2) -> (Maybe Estimate2) -> [EpicNumber2] -> IO IssueNumber2
createIssue title body pipeline labels collaborators milestone estimate epicNumbers = do
  number <- decodeJust' <$> (postGitHub "/issues" $ CreateIssueInput2 title body milestone collaborators labels) -- todo order

  mapM_ (setPipeline number) pipeline
  mapM_ (setEstimate number) estimate
  mapM_ (setEpic number) epicNumbers

  return number


setPipeline :: IssueNumber2 -> Pipeline2 -> IO ()
setPipeline number pipeline = do
  postZenHub_ (toResource number) $ SetPipelineInput2 pipeline
    where
      toResource :: IssueNumber2 -> String                                              -- todo resource interface
      toResource (IssueNumber2 n) = "/issues/" ++ (show $ n) ++ "/moves"


setEstimate :: IssueNumber2 -> Estimate2 -> IO ()
setEstimate number estimate = do
  putZenHub_ (toResource number) $ SetEstimateInput2 estimate
    where
      toResource :: IssueNumber2 -> String                                              -- todo resource interface
      toResource (IssueNumber2 n) = "/issues/" ++ (show $ n) ++ "/estimate"


setEpic :: IssueNumber2 -> EpicNumber2 -> IO ()
setEpic issueNumber epicNumber = do
   postZenHub'_ (toResource epicNumber) $ SetEpicInput2 issueNumber
     where
       toResource :: EpicNumber2 -> String                                              -- todo resource interface
       toResource (EpicNumber2 n) = "/epics/" ++ (show $ n) ++ "/update_issues"


convertToEpic :: IssueNumber2 -> IO EpicNumber2
convertToEpic number = do
  postZenHub_ (toResource number) $ ConvertToEpicInput2

  return $ convert number
    where
      toResource :: IssueNumber2 -> String                                              -- todo resource interface
      toResource (IssueNumber2 n) = "/issues/" ++ (show $ n) ++ "/convert_to_epic"

      convert (IssueNumber2 n) = EpicNumber2 n
