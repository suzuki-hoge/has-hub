module Fixture where


import HasHub.Connection.Type (RepositoryId)

import HasHub.Object.Object.Type
import HasHub.Object.Milestone.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Label.Type
import HasHub.Object.Pipeline.Type


epicNumber1 = EpicNumber 1
epicNumber2 = EpicNumber 2


issueNumber = IssueNumber 2


epicLinkNumber = EpicLinkNumber "?1"


title1 = Title "registration module"
title2 = Title "user registration api"


body1 = Body "post user data and write record."
body2 = Body "write record."

emptyBody = Body ""


pipelineId1 = PipelineId "5b02c59d2133e10681389873"
pipelineName1 = PipelineName "backlog"
pipeline1 = Pipeline pipelineId1 pipelineName1

pipelineId2 = PipelineId "5b0577fa2133e1068138aabc"
pipelineName2 = PipelineName "sprint backlog"
pipeline2 = Pipeline pipelineId2 pipelineName2


label1 = Label "実装"
label2 = Label "dev"


collaborator = Collaborator "suzuki-hoge"


milestoneNumber1 = MilestoneNumber 1
milestoneTitle1 = MilestoneTitle "sprint 1"

startOn1 = Just $ StartOn "2018-04-01T00:00:00Z"
dueOn1 = Just $ DueOn "2018-04-30T23:59:59Z"

referGitHubOutput1 = ReferGitHubOutput milestoneNumber1 milestoneTitle1 dueOn1

--createMilestoneInput1 = CreateMilestoneInput milestoneTitle1 dueOn1

milestone1 = Milestone milestoneNumber1 milestoneTitle1 startOn1 dueOn1

milestoneNumber2 = MilestoneNumber 2
milestoneTitle2 = MilestoneTitle "sprint 2"

startOn2 = Nothing
dueOn2 = Nothing

referGitHubOutput2 = ReferGitHubOutput milestoneNumber2 milestoneTitle2 dueOn2

--createMilestoneInput2 = CreateMilestoneInput milestoneTitle2 dueOn2

milestone2 = Milestone milestoneNumber2 milestoneTitle2 startOn2 dueOn2


estimate1 = Estimate 3.0
estimate2 = Estimate 0.5


sharpEpicNumber = SharpEpicNumber "#1"
questionEpicNumber = QuestionEpicNumber "?1"


repositoryId = 131509978 :: RepositoryId
