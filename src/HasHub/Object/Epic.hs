module HasHub.Object.Epic
(
  validate
, create
, module HasHub.Object.Epic.Data
)
where


import Data.Either.Validation (Validation(..))

import Text.Printf (printf)

import Data.Aeson (decode)
import Data.Maybe (fromJust)

import qualified HasHub.Client as C (Client, postGitHub, getZenHub, postZenHub, Resource)

import HasHub.Object.Epic.Data

import qualified HasHub.Object.Issue as I (create)

import qualified HasHub.Object.Milestone.Data as M
import HasHub.Object.Collaborator.Data
import HasHub.Object.Label.Data
import qualified HasHub.Object.Issue.Data as I
import HasHub.Object.FixMe (FixMe, areAllContains)


validate :: C.Client -> [Number] -> IO (Validation [FixMe] [Number])
validate client needles = do
  putStrLn "\nvalidate EpicNumbers"

  haystacks <- getAll client

  return $ haystacks `areAllContains` needles


getAll :: C.Client -> IO [Number]
getAll client = do
  putStrLn "  fetch all Epics"

  json <- C.getZenHub client "/epics"

  return . fromJust $ (parseInList json)


create :: C.Client -> I.Title -> I.Body -> Maybe M.Number -> [Collaborator] -> [Label] -> [Epic] -> Maybe I.Pipeline -> Maybe I.Estimate -> IO Epic
create client title body mmn cs ls es mp me = do
  number <- asEpicNumber <$> I.create client title body mmn cs ls es mp me

  convertToEpic client number

  return $ Epic number (asEpicTitle title)

    where
      asEpicNumber :: I.Number -> Number
      asEpicNumber (I.Number n) = Number n

      asEpicTitle :: I.Title -> Title
      asEpicTitle (I.Title t) = Title t


convertToEpic :: C.Client -> Number -> IO ()
convertToEpic client number = do
  printf "[%s] convert to epic\n" (show number)

  json <- C.postZenHub client (toResource number) $ EpicConversionInput

  return ()

    where
      toResource :: Number -> C.Resource
      toResource (Number n) = "/issues/" ++ (show $ n) ++ "/convert_to_epic"
