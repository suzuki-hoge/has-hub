module Fixture where


import HasHub.Connection.Config.Type (RepositoryId)

import HasHub.Object.Object.Type
import HasHub.Object.Object.IOType
import HasHub.Object.Pipeline.Type
import HasHub.Object.Pipeline.IOType
import HasHub.Object.Label.Type
import HasHub.Object.Label.IOType
import HasHub.Object.Collaborator.Type
import HasHub.Object.Collaborator.IOType
import HasHub.Object.Milestone.Type
import HasHub.Object.Milestone.IOType


repositoryId = 131509978 :: RepositoryId

owner = "suzuki-hoge"

repository = "has-hub-workspace"

cursor = Just "abcd=="


epicNumber1 = EpicNumber 1
epicNumber2 = EpicNumber 2


issueNumber = IssueNumber 2


epicLinkNumber = EpicLinkNumber "?1"


title1 = Title "registration module"
title2 = Title "user registration api"


epic1 = Epic epicNumber1 title1
epic2 = Epic epicNumber2 title2


body1 = Body "post user data and write record."
body2 = Body "write record."

emptyBody = Body ""


pipelineId1 = PipelineId "abc123"
pipelineId2 = PipelineId "xyz789"

pipelineName1 = PipelineName "backlog"
pipelineName2 = PipelineName "sprint backlog"

pipeline1 = Pipeline pipelineId1 pipelineName1
pipeline2 = Pipeline pipelineId2 pipelineName2


label1 = Label "実装"
label2 = Label "dev"


collaborator = Collaborator "suzuki-hoge"


milestoneNumber1 = MilestoneNumber 1
milestoneNumber2 = MilestoneNumber 2

milestoneTitle1 = MilestoneTitle "sprint 1"
milestoneTitle2 = MilestoneTitle "sprint 2"

startOn1 = Just $ StartOn "2018-04-01T00:00:00Z"
startOn2 = Nothing

dueOn1 = Just $ DueOn "2018-04-30T23:59:59Z"
dueOn2 = Nothing

referGitHubMilestonesOutput1 = ReferGitHubMilestonesOutput milestoneNumber1 milestoneTitle1 dueOn1
referGitHubMilestonesOutput2 = ReferGitHubMilestonesOutput milestoneNumber2 milestoneTitle2 dueOn2

createGitHubMilestoneInput1 = CreateGitHubMilestoneInput milestoneTitle1 dueOn1
createGitHubMilestoneInput2 = CreateGitHubMilestoneInput milestoneTitle2 dueOn2

createStartOnInput1 = CreateStartOnInput milestoneNumber1 (StartOn "2018-04-01T00:00:00Z")

milestone1 = Milestone milestoneNumber1 milestoneTitle1 startOn1 dueOn1
milestone2 = Milestone milestoneNumber2 milestoneTitle2 startOn2 dueOn2


estimate1 = Estimate 3.0
estimate2 = Estimate 0.5


sharpEpicNumber = SharpEpicNumber "#1"
questionEpicNumber = QuestionEpicNumber "?1"
