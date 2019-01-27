module Lib
    ( someFunc
    )
where

import qualified HubBoard.Object.Epic.Mapper   as E
import qualified HubBoard.Object.Label.Mapper  as L
import qualified HubBoard.Object.Milestone.Mapper
                                               as M
import qualified HubBoard.Object.Collaborator.Mapper
                                               as C
import qualified HubBoard.Object.Pipeline.Mapper
                                               as P
import           HubBoard.Transfer.GitHubV4.Transfer
import           HubBoard.Transfer.ZenHub.Transfer
import           HubBoard.Transfer.Config

import           System.Directory

someFunc :: IO ()
someFunc = do
    validation <- validate
    print validation

    getRepositoryId >>= setRepositoryIdToEnv

    es <- getGitHub E.refer
    print es

    ls <- getGitHub L.refer
    print ls

    ms <- getGitHub M.refer
    print ms

    cs <- getGitHub C.refer
    print cs

    ps <- getZenHub P.refer
    print ps
