module Main where

import           Options.Applicative
import           HubBoard.Command.Init         as I
import           HubBoard.Command.NewWorkspace as NW
import           HubBoard.Command.Post         as P
import           HubBoard.Command.Desc         as D

data Options = InitOptions
             | NewWorkspaceOptions
             | PostOptions         FilePath
             | DescOptions

optionsI :: ParserInfo Options
optionsI = info optionsP $ mconcat [header "create epics and issues.", failureCode 1]
  where
    optionsP :: Parser Options
    optionsP = (<*>) helper $ subparser $ mconcat [
        command "init"          $ info ((<*>) helper $ pure InitOptions)         $ mconcat [header "initialize hub-board. put config file to home dir with git-hub-token and zen-hub-token attributes. this command use only one time after install.", failureCode 1]
      , command "new-workspace" $ info ((<*>) helper $ pure NewWorkspaceOptions) $ mconcat [header "put config file to new workspace dir with owner and repository attributes. this command use every time the destination board increased.", failureCode 1]
      , command "post"          $ info ((<*>) helper $ PostOptions <$> yamlP)    $ mconcat [header "post to github and zenhub according yaml.", failureCode 1]
      , command "desc"          $ info ((<*>) helper $ pure DescOptions)         $ mconcat [header "show yaml description url.", failureCode 1]
      ]

    yamlP :: Parser FilePath
    yamlP = strArgument $ mconcat [metavar "yaml", action "file", help "yaml path"]

main :: IO ()
main = customExecParser (prefs showHelpOnError) optionsI >>= execute
  where
    execute :: Options -> IO ()
    execute InitOptions = I.exec
    execute NewWorkspaceOptions = NW.exec
    execute (PostOptions yaml) = P.exec yaml
    execute DescOptions = D.exec
