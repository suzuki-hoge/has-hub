module Main where


import Data.Monoid
import Data.Char
import Text.Read
import Options.Applicative hiding (Success, Failure)

import qualified HasHub.Command.ReferAll as RA
import qualified HasHub.Command.GenerateObjectsSample as GOS
import qualified HasHub.Command.GenerateMilestonesSample as GMS
import qualified HasHub.Command.CreateObjects as CO
import qualified HasHub.Command.CreateMilestones as CM
import qualified HasHub.Command.Configure as C

import HasHub.Connection.Config.Type

import HasHub.FixMe (printFixMes, Validation(..), isWritable)


data Options = ReferAllOptions                          Owner Repository Token Token FilePath
             | GenerateObjectsSampleOptions    FilePath
             | GenerateMilestonesSampleOptions FilePath
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


referOptionsP :: Parser Options
referOptionsP = (<*>) helper $ ReferAllOptions <$> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


referOptionsInfo :: ParserInfo Options
referOptionsInfo = info referOptionsP $ mconcat [
    header "refer all objects"
  , failureCode 1
  ]


createObjectsOptionsP :: Parser Options
createObjectsOptionsP = (<*>) helper $ CreateObjectsOptions <$> yamlP <*> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


createObjectsOptionsInfo :: ParserInfo Options
createObjectsOptionsInfo = info createObjectsOptionsP $ mconcat [
    header "create epics and issues"
  , failureCode 1
  ]


createMilestonesOptionsP :: Parser Options
createMilestonesOptionsP = (<*>) helper $ CreateMilestonesOptions <$> yamlP <*> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP


createMilestonesOptionsInfo :: ParserInfo Options
createMilestonesOptionsInfo = info createMilestonesOptionsP $ mconcat [
    header "create milestones"
  , failureCode 1
  ]


generateObjectsSampleOptionsP :: Parser Options
generateObjectsSampleOptionsP = (<*>) helper $ GenerateObjectsSampleOptions <$> outP


generateObjectsSampleOptionsInfo :: ParserInfo Options
generateObjectsSampleOptionsInfo = info generateObjectsSampleOptionsP $ mconcat [
    header "generate sample yaml"
  , failureCode 1
  ]


generateMilestonesSampleOptionsP :: Parser Options
generateMilestonesSampleOptionsP = (<*>) helper $ GenerateMilestonesSampleOptions <$> outP


generateMilestonesSampleOptionsInfo :: ParserInfo Options
generateMilestonesSampleOptionsInfo = info generateMilestonesSampleOptionsP $ mconcat [
    header "generate sample yaml"
  , failureCode 1
  ]


optionsP :: Parser Options
optionsP = (<*>) helper $ subparser $ mconcat [
    command "refer-all"                  referOptionsInfo
  , command "create-objects"             createObjectsOptionsInfo
  , command "create-milestones"          createMilestonesOptionsInfo
  , command "generate-objects-sample"    generateObjectsSampleOptionsInfo
  , command "generate-milestones-sample" generateMilestonesSampleOptionsInfo
  ]


optionsInfo :: ParserInfo Options
optionsInfo = info optionsP $ mconcat [
    header "operate git hub and zen hub with yaml"
  , failureCode 1
  ]


main :: IO ()
main = customExecParser (prefs showHelpOnError) optionsInfo >>= execute


execute :: Options -> IO ()
execute (ReferAllOptions                             owner repository gitHubToken zenHubToken logPath) = executeWithConnection   RA.execute        owner repository gitHubToken zenHubToken logPath
execute (GenerateObjectsSampleOptions    output                                                      ) = executeWithOutput      GOS.execute output
execute (GenerateMilestonesSampleOptions output                                                      ) = executeWithOutput      GMS.execute output
execute (CreateObjectsOptions                   yaml owner repository gitHubToken zenHubToken logPath) = executeWithConnection  (CO.execute yaml)  owner repository gitHubToken zenHubToken logPath
execute (CreateMilestonesOptions                yaml owner repository gitHubToken zenHubToken logPath) = executeWithConnection  (CM.execute yaml)  owner repository gitHubToken zenHubToken logPath


executeWithOutput :: (FilePath -> IO ()) -> FilePath -> IO ()
executeWithOutput executor output = do
  writable <- isWritable output

  case writable of
    Success ()  -> executor output
    Failure fms -> printFixMes fms


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
