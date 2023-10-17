{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( updateQueue,
    headQueue,
    removeHead,
    emptyQueue,
    Queue,
    QItem (..),
    Status (..),
    BotState (..),
  )
where

import Data.Aeson (FromJSON, decodeFileStrict)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.Directory (listDirectory, removeFile)
import TD.Lib

data Status
  = Ready
  | WaitingChat Extra
  | WaitingMessageACK Extra
  | WaitingMessageSending Int
  deriving (Eq)

data BotState = BotState
  { client :: Client,
    status :: Status,
    queue :: Queue,
    chats :: [Int],
    myId :: Maybe Int,
    apiid :: Int,
    apihash :: T.Text
  }

data QItem = QItem
  { method :: String,
    caption :: Maybe String,
    chat_id :: Int,
    file :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON QItem

newtype Queue = Queue [(QItem, FilePath)]

queueDir :: FilePath
queueDir = "./queue/"

emptyQueue :: Queue
emptyQueue = Queue []

headQueue :: Queue -> Maybe QItem
headQueue (Queue []) = Nothing
headQueue (Queue ((q, _) : _)) = Just q

removeHead :: Queue -> IO Queue
removeHead (Queue []) = pure $ Queue []
removeHead (Queue ((_, f) : xs)) = do
  removeFile f
  pure $ Queue xs

updateQueue :: IO Queue
updateQueue = listDirectory queueDir >>= fmap (Queue . catMaybes) . mapM r
  where
    r :: FilePath -> IO (Maybe (QItem, FilePath))
    r f =
      decodeFileStrict (path f) >>= \case
        (Just res) -> pure (Just (res, path f))
        Nothing -> pure Nothing

    path :: FilePath -> FilePath
    path = (queueDir <>)
