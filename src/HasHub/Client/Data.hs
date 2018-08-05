module HasHub.Client.Data where


import Network.HTTP.Types (RequestHeaders)

import Data.ByteString (ByteString)

import Data.Aeson (Value(Object))
import Data.Aeson.Types (Parser)

import HasHub.Client.Fetcher (Fetcher)


-- Client


type Owner = String
type RepositoryName = String

type GitHubToken = String
type GitHubHeaders = RequestHeaders
type GitHubEndpoint = String

data GitHubConnector = GitHubConnector RequestHeaders GitHubEndpoint deriving Show

type ZenHubToken = String
type ZenHubHeaders = RequestHeaders
type ZenHubEndpoint = String
type RepositoryId = Int

data ZenHubConnector = ZenHubConnector ZenHubHeaders ZenHubEndpoint RepositoryId deriving Show

data Client = Client Fetcher GitHubConnector ZenHubConnector deriving Show


asRepositoryId :: Client -> RepositoryId
asRepositoryId (Client _ _ (ZenHubConnector _ _ rid)) = rid