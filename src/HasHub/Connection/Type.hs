module HasHub.Connection.Type where


type RequestId = String


type Token = String


type Owner = String


type Repository = String


type RepositoryId = Int


type Endpoint = String


type Resource = String


class ToResource a where
  toResource :: a -> Resource


data RepositoryIdInput = RepositoryIdInput
instance ToResource RepositoryIdInput where
  toResource _ = ""
