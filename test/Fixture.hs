module Fixture where


import HasHub.Connection.Type (RepositoryId)

import HasHub.Object.Object.Type
import HasHub.Object.Milestone.Type
import HasHub.Object.Collaborator.Type
import HasHub.Object.Label.Type
import HasHub.Object.Pipeline.Type


-- todo order

--title = Title "post module"
--
--title2 = Title "user api"
--
--
--body = Body "post user data and write record."
--body2 = Body "write record."
--emptyBody = Body ""
--
--
--epicLinkNumber = EpicLinkNumber "?2"
--parentEpicNumber = QuestionEpicNumber "?1"
--
--
--parentEpicNumbers = [parentEpicNumber]
--noEpicLinkNumbers = []
--
--
--milestoneTitle = MilestoneTitle "sprint 1"
--justMilestoneTitle = Just milestoneTitle
--noMilestoneTitle = Nothing
--
--milestone = Milestone (MilestoneNumber 1) milestoneTitle (Just $ StartOn "2018-01-01T00:00:00Z") (Just $ DueOn "2018-01-31T23:59:59Z")
--justMilestone = Just milestone
--noMilestone = Nothing
--
--
collaborator1 = Collaborator "suzuki-hoge"


label1 = Label "setup"
label2 = Label "実装"


pipelineId1 = PipelineId "5b02c59d2133e10681389873"
pipelineName1 = PipelineName "backlog"
pipeline1 = Pipeline pipelineId1 pipelineName1

pipelineId2 = PipelineId "5b0577fa2133e1068138aabc"
pipelineName2 = PipelineName "sprint backlog"
pipeline2 = Pipeline pipelineId2 pipelineName2


milestoneNumber1 = MilestoneNumber 1
milestoneTitle1 = MilestoneTitle "sprint 1"
startOn1 = Just $ StartOn "2018-04-01T00:00:00Z"
dueOn1 = Just $ DueOn "2018-04-30T23:59:59Z"
referMilestoneOutput1 = ReferMilestoneOutput milestoneNumber1 milestoneTitle1 dueOn1
--createMilestoneInput1 = CreateMilestoneInput milestoneTitle1 dueOn1
milestone1 = Milestone milestoneNumber1 milestoneTitle1 startOn1 dueOn1

milestoneNumber2 = MilestoneNumber 2
milestoneTitle2 = MilestoneTitle "sprint 2"
startOn2 = Nothing
dueOn2 = Nothing
referMilestoneOutput2 = ReferMilestoneOutput milestoneNumber2 milestoneTitle2 dueOn2
--createMilestoneInput2 = CreateMilestoneInput milestoneTitle2 dueOn2
milestone2 = Milestone milestoneNumber2 milestoneTitle2 startOn2 dueOn2


issueNumber1 = IssueNumber 1

epicNumber1 = EpicNumber 1
epicNumber2 = EpicNumber 2

sharpEpicNumber1 = SharpEpicNumber "#1"
sharpEpicNumber2 = SharpEpicNumber "#2"
questionEpicNumber1 = QuestionEpicNumber "?1"
questionEpicNumber2 = QuestionEpicNumber "?2"

epicLinkNumber1 = EpicLinkNumber "?1"


title1 = Title "post module"
title2 = Title "user api"


body1 = Body "post user data and write record."
body2 = Body "write record."

emptyBody = Body ""


estimate1 = Estimate 3.0
estimate2 = Estimate 0.5


repositoryId = 131509978 :: RepositoryId

--
--
--pipelineName = PipelineName "sprint backlog"
--justPipelineName = Just pipelineName
--noPipelineName = Nothing
--
--
--pipeline = Pipeline "12345678" "sprint backlog"
--justPipeline = Just pipeline
--noPipeline = Nothing
--
--
--estimate = Estimate 3.0
--justEstimate = Just estimate
--estimate2 = Estimate 0.5
--justEstimate = Just estimate2
--noEstimate = Nothing
