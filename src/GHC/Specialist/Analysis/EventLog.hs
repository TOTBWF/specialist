-- | Functions for extracting specialist plugin output from a GHC event log.
module GHC.Specialist.Analysis.EventLog where

import GHC.Specialist.Plugin.Types

import Data.Binary qualified as Bin
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Base64 qualified as LBS
import Data.Text.Encoding qualified as T

import GHC.RTS.Events

-- | Extract a 'SpecialistNote' from an 'Event'
specialistNoteFromEvent :: Event -> Maybe (Either String SpecialistNote)
specialistNoteFromEvent Event{..} =
    case evSpec of
      UserMessage msg ->
        case Bin.decodeOrFail $ LBS.decodeBase64Lenient $ LBS.fromStrict $ T.encodeUtf8 msg of
          Left (_, _, err) -> Just $! Left err
          Right (_, _, a) -> Just $! Right a
      UnknownEvent typeNum ->
        Just $! Left $ "Unknown event type " <> show typeNum
      _ -> Nothing

specialistNotesFromEventLogFile :: FilePath -> IO [SpecialistNote]
specialistNotesFromEventLogFile eventLogFile =
    readEventLogFromFile eventLogFile >>=
      \case
        Right eventLog ->
          return $ go (eventLogEvents eventLog)
        Left msg ->
          fail msg
  where
    go :: [Event] -> [SpecialistNote]
    go [] = []
    go (ev:evs) =
        case specialistNoteFromEvent ev of
          Just (Left err) -> fail err
          Just (Right note) -> note : go evs
          Nothing -> go evs

eventLogEvents :: EventLog -> [Event]
eventLogEvents (EventLog _ (Data evs)) = evs
