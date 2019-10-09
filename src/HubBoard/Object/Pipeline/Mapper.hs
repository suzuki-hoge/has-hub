{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Object.Pipeline.Mapper (
    refer
) where

import           HubBoard.Object.Pipeline.Type
import           HubBoard.Fetcher

instance FromJSON Pipeline where
    parseJSON (Object v) = Pipeline <$> (v .: "id") <*> (v .: "name")

--refer :: IO [Pipeline]
--refer = getFromZenHub toResource parse
--  where
--    toResource = printf "%s/board"
--    parse = fromJust . (decode >=> parseMaybe (.: "pipelines"))
refer :: IO [Pipeline]
refer = return [Pipeline "5bd92a1cd643eb36430c27c6" "超先のバックログ", Pipeline "5c3401ab32f2e5319a433524" "バックログ", Pipeline "5be11beb4590dc110b6dcf91" "カレンダーの予定", Pipeline "5bd92a1cd643eb36430c27c9" "スプリントバックログ", Pipeline "5c9c29509a9dfd529273578c" "今日中", Pipeline "5bd92a1cd643eb36430c27ca" "進行中", Pipeline "5bdfe50a4590dc110b6dc19c" "レビュー待ち"]
