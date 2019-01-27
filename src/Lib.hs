module Lib
    ( someFunc
    )
where

import qualified HubBoard.Object.Epic.Mapper   as E
import qualified HubBoard.Object.Epic.Type as E
import qualified HubBoard.Object.Label.Mapper  as L
import qualified HubBoard.Object.Label.Type  as L
import qualified HubBoard.Object.Milestone.Type
                                               as M
import qualified HubBoard.Object.Milestone.Mapper
                                               as M
import qualified HubBoard.Object.Issue.Mapper
                                               as I
import qualified HubBoard.Object.Issue.Type
                                               as I
import qualified HubBoard.Object.Collaborator.Type
                                               as C
import qualified HubBoard.Object.Collaborator.Mapper
                                               as C
import qualified HubBoard.Object.Pipeline.Mapper
                                               as P
import           HubBoard.Transfer.GitHubV4.Transfer
import           HubBoard.Transfer.GitHubV3.Transfer
import           HubBoard.Transfer.ZenHub.Transfer
import           HubBoard.Transfer.Config

import           System.Directory

someFunc :: IO ()
someFunc = do
    -- validation <- validate
    -- print validation

    getRepositoryId >>= setRepositoryIdToEnv

    -- es <- getGitHub E.refer
    -- print es

    -- ls <- getGitHub L.refer
    -- print ls

    -- let m = M.Milestone "sprint x"
    --                     "2019-01-01T00:00:00Z"
    --                     "2019-01-31T23:59:59Z"
    -- number <- postGitHub (M.create m)
    -- ms <- getGitHub M.refer
    -- print ms

    let i = I.Issue "created" "+ [ ] task 1" [L.Label "dev"] [C.Collaborator "suzuki-hoge"] (Just $ M.MilestoneNumber "4")
    (I.IssueNumber n) <- postGitHub (I.create i)
    print n

    -- cs <- getGitHub C.refer
    -- print cs

    -- ps <- getZenHub P.refer
    -- print ps
