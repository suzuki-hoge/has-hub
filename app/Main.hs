module Main where


import Data.Monoid
import Data.Char
import Text.Read
import Options.Applicative hiding (Success, Failure)

import qualified HasHub.Command.ReferAll as RA
import qualified HasHub.Command.GenerateObjectsSample as GOS
import qualified HasHub.Command.GenerateMilestonesSample as GMS
import qualified HasHub.Command.ValidateObjects as VO
import qualified HasHub.Command.ValidateMilestones as VM
import qualified HasHub.Command.CreateObjects as CO
import qualified HasHub.Command.CreateMilestones as CM
import qualified HasHub.Command.Configure as C

import HasHub.Connection.Config.Type

import HasHub.FixMe (printFixMes, Validation(..), isWritable)


data Options = ReferAllOptions                          Owner Repository Token Token FilePath
             | GenerateObjectsSampleOptions    FilePath
             | GenerateMilestonesSampleOptions FilePath
             | ValidateObjectsOptions          FilePath Owner Repository Token Token FilePath
             | ValidateMilestonesOptions       FilePath Owner Repository Token Token FilePath
             | CreateObjectsOptions            FilePath Owner Repository Token Token FilePath
             | CreateMilestonesOptions         FilePath Owner Repository Token Token FilePath
             deriving Show


yamlP :: Parser FilePath
yamlP = strArgument $ mconcat [
    metavar "path"
  , action "file"
  , help "input file"
  ]


ownerP :: Parser Owner
ownerP = strOption $ mconcat [
    metavar "path"
  , short 'o'
  , long "owner"
  , value ""
  , help "git hub onwer"
  ]


repositoryP :: Parser Repository
repositoryP = strOption $ mconcat [
    metavar "str"
  , short 'r'
  , long "repo"
  , value ""
  , help "git hub repository"
  ]


gitHubTokenP :: Parser Token
gitHubTokenP = strOption $ mconcat [
    metavar "str"
  , short 'g'
  , long "git-hub-token"
  , value ""
  , help "git hub personal access tokens"
  ]


zenHubTokenP :: Parser Token
zenHubTokenP = strOption $ mconcat [
    metavar "str"
  , short 'z'
  , long "zen-hub-token"
  , value ""
  , help "zen hub api token"
  ]


logPathP :: Parser FilePath
logPathP = strOption $ mconcat [
    metavar "path"
  , short 'l'
  , long "log"
  , action "file"
  , value ""
  , help "path for log"
  ]


outP :: Parser FilePath
outP = strArgument $ mconcat [
    metavar "path"
  , action "file"
  , help "output file"
  ]


referAllI :: ParserInfo Options
referAllI = info optionsP $ mconcat [header "refer all issues, epics, pipelines, labels, collaborators, milestones.", failureCode 1]
  where
    optionsP = (<*>) helper $ ReferAllOptions <$> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


generateObjectsSampleI :: ParserInfo Options
generateObjectsSampleI = info optionsP $ mconcat [header "generate objects sample yaml file.", failureCode 1]
  where
    optionsP = (<*>) helper $ GenerateObjectsSampleOptions <$> outP :: Parser Options


generateMilestonesSampleI :: ParserInfo Options
generateMilestonesSampleI = info optionsP $ mconcat [header "generate milestones sample yaml file.", failureCode 1]
  where
    optionsP = (<*>) helper $ GenerateMilestonesSampleOptions <$> outP :: Parser Options


validateObjectsI :: ParserInfo Options
validateObjectsI = info optionsP $ mconcat [header "validate objects yaml file.", failureCode 1]
  where
    optionsP = (<*>) helper $ ValidateObjectsOptions <$> yamlP <*> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


validateMilestonesI :: ParserInfo Options
validateMilestonesI = info optionsP $ mconcat [header "validate milestones yaml file.", failureCode 1]
  where
    optionsP = (<*>) helper $ ValidateMilestonesOptions <$> yamlP <*> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


createObjectsI :: ParserInfo Options
createObjectsI = info optionsP $ mconcat [header "create objects.", failureCode 1]
  where
    optionsP = (<*>) helper $ CreateObjectsOptions <$> yamlP <*> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


createMilestonesI :: ParserInfo Options
createMilestonesI = info optionsP $ mconcat [header "create milestones.", failureCode 1]
  where
    optionsP = (<*>) helper $ CreateMilestonesOptions <$> yamlP <*> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


optionsP :: Parser Options
optionsP = (<*>) helper $ subparser $ mconcat [
    command "refer-all"                  referAllI
  , command "generate-objects-sample"    generateObjectsSampleI
  , command "generate-milestones-sample" generateMilestonesSampleI
  , command "validate-objects"           validateObjectsI
  , command "validate-milestones"        validateMilestonesI
  , command "create-objects"             createObjectsI
  , command "create-milestones"          createMilestonesI
  ]


optionsInfo :: ParserInfo Options
optionsInfo = info optionsP $ mconcat [
    header "operate git hub and zen hub with yaml file."
  , failureCode 1
  ]


main :: IO ()
main = customExecParser (prefs showHelpOnError) optionsInfo >>= execute


execute :: Options -> IO ()
execute (ReferAllOptions                             owner repository gitHubToken zenHubToken logPath) = executeWithConnection   RA.execute        owner repository gitHubToken zenHubToken logPath
execute (GenerateObjectsSampleOptions    output                                                      ) = executeWithOutput      GOS.execute output
execute (GenerateMilestonesSampleOptions output                                                      ) = executeWithOutput      GMS.execute output
execute (ValidateObjectsOptions                 yaml owner repository gitHubToken zenHubToken logPath) = executeWithConnection  (VO.execute yaml)  owner repository gitHubToken zenHubToken logPath
execute (ValidateMilestonesOptions              yaml owner repository gitHubToken zenHubToken logPath) = executeWithConnection  (VM.execute yaml)  owner repository gitHubToken zenHubToken logPath
execute (CreateObjectsOptions                   yaml owner repository gitHubToken zenHubToken logPath) = executeWithConnection  (CO.execute yaml)  owner repository gitHubToken zenHubToken logPath
execute (CreateMilestonesOptions                yaml owner repository gitHubToken zenHubToken logPath) = executeWithConnection  (CM.execute yaml)  owner repository gitHubToken zenHubToken logPath


executeWithConnection :: IO () -> Owner -> Repository -> Token -> Token -> FilePath -> IO ()
executeWithConnection executor owner repository gitHubToken zenHubToken logPath = do
  initialized <- C.initialize (mb owner) (mb repository) (mb gitHubToken) (mb zenHubToken) (mb logPath)

  case initialized of
    Success lp  -> do
      writable <- isWritable lp

      case writable of
        Success () -> do
          C.fetchRepositoryId
          executor
        Failure fms -> printFixMes fms

    Failure fms -> printFixMes fms

    where
      mb :: String -> Maybe String
      mb "" = Nothing
      mb s  = Just s


executeWithOutput :: (FilePath -> IO ()) -> FilePath -> IO ()
executeWithOutput executor output = do
  writable <- isWritable output

  case writable of
    Success ()  -> executor output
    Failure fms -> printFixMes fms
