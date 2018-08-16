module Main where


import qualified HasHub.Command.CreateObjects as CO

import HasHub.Connection.Connector (set) -- todo


setup :: String -> String -> IO ()
setup = set


main = CO.execute
