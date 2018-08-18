module Main where


import Data.Monoid
import Data.Char
import Text.Read
import Options.Applicative

import Control.Monad

import HasHub.Connection.Config.Type


data Options = CreateOptions FilePath Owner Repository Token Token FilePath Proxy
             | ReferOptions Owner Repository Token Token FilePath Proxy
             | GenerateOptions FilePath
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


proxyP :: Parser FilePath
proxyP = strOption $ mconcat [
    metavar "str"
  , short 'p'
  , long "proxy"
  , value ""
  , help "like xxx.xxx.xxx.xxx:xxxx"
  ]


outP :: Parser FilePath
outP = strArgument $ mconcat [
    metavar "path"
  , action "file"
  , help "output file"
  ]


referOptionsP :: Parser Options
referOptionsP = (<*>) helper $ ReferOptions <$> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP <*> proxyP


referOptionsInfo :: ParserInfo Options
referOptionsInfo = info referOptionsP $ mconcat [
    header "refer all objects"
  , failureCode 1
  ]


createOptionsP :: Parser Options
createOptionsP = (<*>) helper $ CreateOptions <$> yamlP <*> ownerP <*> repositoryP <*> gitHubTokenP <*> zenHubTokenP <*> logPathP <*> proxyP


createOptionsInfo :: ParserInfo Options
createOptionsInfo = info createOptionsP $ mconcat [
    header "create epics and issues"
  , failureCode 1
  ]


generateOptionsP :: Parser Options
generateOptionsP = (<*>) helper $ GenerateOptions <$> outP


generateOptionsInfo :: ParserInfo Options
generateOptionsInfo = info generateOptionsP $ mconcat [
    header "generate sample yaml"
  , failureCode 1
  ]


optionsP :: Parser Options
optionsP = (<*>) helper $ subparser $ mconcat [
    command "refer-all"                  referOptionsInfo
  , command "create-objects"             createOptionsInfo
  , command "create-milestones"          createOptionsInfo
  , command "generate-objects-sample"    generateOptionsInfo
  , command "generate-milestones-sample" generateOptionsInfo
  ]


optionsInfo :: ParserInfo Options
optionsInfo = info optionsP $ mconcat [
    header "operate git hub and zen hub with yaml"
  , failureCode 1
  ]


main :: IO ()
main = do
  options <- customExecParser (prefs showHelpOnError) optionsInfo

  print options
