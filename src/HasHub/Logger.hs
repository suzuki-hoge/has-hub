module HasHub.Logger where


logging :: String -> FilePath -> String -> IO ()
logging cid path s = do
  let line = cid ++ ": " ++ s ++ "\n"
  appendFile path line
