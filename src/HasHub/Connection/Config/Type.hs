module HasHub.Connection.Config.Type where


type RequestId = String


type Owner = String


type Repository = String


type Token = String


type Endpoint = String


type RepositoryId = Int


type Proxy = String


type Resource = String


data Configs = Configs Owner Repository Token Token FilePath (Maybe Proxy) deriving (Show)


class ToResource a where
  toResource :: a -> Resource


data RepositoryIdInput = RepositoryIdInput
instance ToResource RepositoryIdInput where
  toResource _ = ""
