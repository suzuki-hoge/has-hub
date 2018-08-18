module Main where


import HasHub.Connection.Config.LocalConfig

import qualified HasHub.Command.CreateObjects as CO
import qualified HasHub.Command.Configure as C

import HasHub.FixMe


main = do
  initialized <- C.initialize Nothing (Just "has-hub-workspace") Nothing Nothing (Just "has-hub.log") Nothing
  case initialized of
    Success ()  -> CO.execute
    Failure fms -> printFixMes fms

  C.deinitialize
