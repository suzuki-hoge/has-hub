module Main where

import           Options.Applicative
import           HubBoard.Command.Init as I
import           HubBoard.Command.New  as N
import           HubBoard.Command.Post as P
import           HubBoard.Command.Desc as D

data Options = InitOptions
             | NewOptions String
             | PostOptions FilePath Bool
             | DescOptions

optionsI :: ParserInfo Options
optionsI = info (optionsP <**> versionP) $ mconcat [header "create epics and issues.", failureCode 1]
  where
    optionsP :: Parser Options
    optionsP = (<*>) helper $ subparser $ mconcat [
        command "init" $ info ((<*>) helper $ pure InitOptions)               $ mconcat [header "put \".hub-board-config.yaml\" to home dir with \"git-hub-token\" and \"zen-hub-token\" attributes. this command use only one time after install.", failureCode 1]
      , command "new"  $ info ((<*>) helper $ NewOptions <$> nameP)           $ mconcat [header "put \".hub-board-config.yaml\" to new workspace dir with \"owner\" and \"repository\" attributes. this command use every time the destination board increased.", failureCode 1]
      , command "post" $ info ((<*>) helper $ PostOptions <$> yamlP <*> dryP) $ mconcat [header "post to github and zenhub according yaml.", failureCode 1]
      , command "desc" $ info ((<*>) helper $ pure DescOptions)               $ mconcat [header "show yaml description url.", failureCode 1]
      ]

    yamlP :: Parser FilePath
    yamlP = strArgument $ mconcat [metavar "yaml", action "file", help "yaml path"]

    nameP :: Parser String
    nameP = strArgument $ mconcat [metavar "workspace-name", help "workspace directory name"]

    dryP :: Parser Bool
    dryP = switch $ mconcat [long "dry", help "dry run"]

    versionP :: Parser (a -> a)
    versionP = infoOption "2.2.0" $ mconcat [short 'v', long "version", help "Show version"]

main :: IO ()
main = execParser optionsI >>= execute
  where
    execute :: Options -> IO ()
    execute InitOptions = I.exec
    execute (NewOptions name) = N.exec name
    execute (PostOptions yaml dry) = P.exec yaml (if dry then P.Dry else P.Execute)
    execute DescOptions = D.exec
