module HasHub.Fixture where


import HasHub.Object.Object.Data
import HasHub.Object.Milestone.Data
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import HasHub.Object.Pipeline.Data


-- todo order

title = Title "post module"

title2 = Title "user api"


body = Body "post user data and write record."
body2 = Body "write record."
emptyBody = Body ""


ownEpicLinkNumber = EpicQuestionNumber 2
parentEpicLinkNumber = EpicQuestionNumber 1


parentEpicLinkNumbers = [parentEpicLinkNumber]
noEpicLinkNumbers = []


milestoneTitle = MilestoneTitle "sprint 1"
justMilestoneTitle = Just milestoneTitle
noMilestoneTitle = Nothing

milestone = Milestone (MilestoneNumber 1) milestoneTitle (Just $ StartOn "2018-01-01T00:00:00Z") (Just $ DueOn "2018-01-31T23:59:59Z")
justMilestone = Just milestone
noMilestone = Nothing


collaborators = [Collaborator "suzuki-hoge"]
noCollaborators = []


labels = [Label "setup", Label "dev"]
noLabels = []


pipelineName = PipelineName "sprint backlog"
justPipelineName = Just pipelineName
noPipelineName = Nothing


pipeline = Pipeline "12345678" "sprint backlog"
justPipeline = Just pipeline
noPipeline = Nothing


estimate = Estimate 3.0
justEstimate = Just estimate
estimate2 = Estimate 0.5
justEstimate2 = Just estimate2
noEstimate = Nothing
