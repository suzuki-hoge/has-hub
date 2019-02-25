{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Command.Post (
    exec
) where

import           Data.List.Utils (replace)
import           HubBoard.Fetcher     as F
import           HubBoard.Yaml.Parser
import           HubBoard.Object.Epic as E

type IsDry = Bool

exec :: FilePath -> IsDry -> IO ()
exec fp isDry = do
    configErrors <- F.initialize

    case configErrors of
        [] -> do
            validated <- parse fp isDry

            case validated of
                (Right epics) -> do
                    putStrLn "\nfound"
                    mapM_ (putStrLn . found) epics

                    case isDry of
                        False -> do
                            putStrLn "\ncreating..."
                            mapM_ E.create epics
                        True -> putStrLn "\ncreating ( dry )..."

                    putStrLn "\ncompleted.\n"
                (Left es) -> do
                    putStrLn "\nfix following validation errors!!!"
                    ps es
                    putStrLn "\naboted.\n"
        es -> do
            putStrLn "\nfix following configure errors!!!"
            ps es
            putStrLn "\naboted.\n"

    putStrLn $ "show " ++ F.logFilePath ++ "\n"
  where
    ps = mapM_ (putStrLn . ("  " ++))

    found :: Epic -> String
    found (NewEpic title _ _ _ _ _ _ issues) = printf "  NewEpic ( %s ): %s" title (found' issues)
    found (ExistingEpic (EpicNumber number) issues) = printf "  ExistingEpic ( #%d ): %s" number (found' issues)
    found (NoEpic issues) = printf "  NoEpic: %s" (found' issues)

    found' :: [Issue] -> String
    found' is = printf "issue count is %d, total estimate of issue is %s" (length is) (total is)
      where
        total :: [Issue] -> String
        total = replace ".0" "" . take 3 . show . sum . map (\(Issue _ _ _ _ _ _ e) -> e)