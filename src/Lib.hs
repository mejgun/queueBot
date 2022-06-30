{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( getQueue,
    Q,
    chat_id,
    caption,
    file,
    method,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Directory (listDirectory)

data Q = Q
  { method :: String,
    caption :: Maybe String,
    chat_id :: Int,
    file :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON Q

instance FromJSON Q

getQueue :: String -> IO [FilePath]
getQueue d = map (d ++) <$> listDirectory d
