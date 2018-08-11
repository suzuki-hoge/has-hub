{-# LANGUAGE OverloadedStrings #-}


module Main where


import HasHub.Connection.Connector (set) -- todo
import HasHub.Object.Label.Client as LC
import qualified HasHub.Object.Label.Validator as LV

la :: IO ()
la = do
  ls <- LC.referAll
  print $ [] `LV.areAllIn` ls
  print $ [Label2 "setup"] `LV.areAllIn` ls
  print $ [Label2 "setup", Label2 "unknown", Label2 "dev"] `LV.areAllIn` ls


main = do
  set
  la
