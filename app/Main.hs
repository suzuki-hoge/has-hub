{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HubBoard.Fetcher             as F
import           HubBoard.Object.Collaborator as C
import           HubBoard.Object.Epic         as E
import           HubBoard.Object.Issue        as I
import           HubBoard.Object.Label        as L
import           HubBoard.Object.Milestone    as M
import           HubBoard.Object.Pipeline     as P

main :: IO ()
main = do
    F.initialize
    L.refer >>= print
    M.refer >>= print
    C.refer >>= print
    P.refer >>= print
    E.refer >>= print
    let ls = [Label "dev"]
    let cs = [Collaborator "suzuki-hoge"]
    let mm = Just (MilestoneNumber 1)
    let p = Pipeline "5b0577fa2133e1068138aabc" "sprint backlog"
    let me = Just (EpicNumber 74)
    let e = 3
    I.create (Issue "issue-title" "issue-body" ls cs mm p me e)

    E.create (Epic "epic-title" "epic-body" ls mm p e)
    return ()
