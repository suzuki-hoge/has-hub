{-# LANGUAGE OverloadedStrings #-}


module Main where


import HasHub.Connection.Connector (set) -- todo

import HasHub.Object.Label.Client as LC
import HasHub.Object.Collaborator.Client as CC
import qualified HasHub.Object.Label.Validator as LV
import qualified HasHub.Object.Collaborator.Validator as CV

la :: IO ()
la = do
  ls <- LC.referAll
  print $ [Label2 "setup"] `LV.areAllIn` ls
  print $ [Label2 "unknown"] `LV.areAllIn` ls


ca :: IO ()
ca = do
  cs <- CC.referAll
  print $ [Collaborator2 "suzuki-hoge"] `CV.areAllIn` cs
  print $ [Collaborator2 "unknown"] `CV.areAllIn` cs


main = do
  set
  la
  ca
