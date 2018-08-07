{-# LANGUAGE OverloadedStrings #-}


module HasHub.Yaml.Parser.Data where


import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?))


data Object = EpicObject
                  String           -- epic-link-number
                  String           -- title
                  String           -- body
                  [String]         -- epics
                  (Maybe Double)   -- estimate
                  (Maybe String)   -- milestone
                  [String]         -- labels
                  [String]         -- assignees
                  (Maybe String)   -- pipeline
            | IssueObject

                  String           -- title
                  String           -- body
                  [String]         -- epics
                  (Maybe Double)   -- estimate
                  (Maybe String)   -- milestone
                  [String]         -- labels
                  [String]         -- assignees
                  (Maybe String)   -- pipeline
            deriving (Eq, Show)


data RawObject = RawObject
                  (Maybe String)   -- epic-link-number
                  String           -- title
                  (Maybe String)   -- body
                  (Maybe [String]) -- epics
                  (Maybe Double)   -- estimate
                  (Maybe String)   -- milestone
                  (Maybe [String]) -- labels
                  (Maybe [String]) -- assignees
                  (Maybe String)   -- pipeline
               deriving Show
instance FromJSON RawObject where
  parseJSON (Object v) = RawObject <$> (v .:? "epic-link-number") <*> (v .: "title") <*> (v .:? "body") <*> (v .:? "epics") <*> (v .:? "estimate") <*> (v .:? "milestone") <*> (v .:? "labels") <*> (v .:? "assignees") <*> (v .:? "pipeline")


rawToEpicOrIssue :: RawObject -> Object
rawToEpicOrIssue (RawObject epicLinkNumber title body epics estimate milestone labels assignees pipeline) = createEither epicLinkNumber title ((?) body) ((??) epics) estimate milestone ((??) labels) ((??) assignees) pipeline

  where
    createEither (Just eln) = EpicObject eln
    createEither Nothing    = IssueObject

    (?) :: Maybe String -> String
    (?) Nothing = ""
    (?) (Just xs) = xs


    (??) :: Maybe [a] -> [a]
    (??) Nothing = []
    (??) (Just xs) = xs


data Milestone = Milestone
                  String         -- title
                  (Maybe String) -- start_on
                  (Maybe String) -- due_on
               deriving (Eq, Show)
instance FromJSON Milestone where
  parseJSON (Object v) = Milestone <$> (v .: "title") <*> (v .:? "start_on") <*> (v .:? "due_on")
