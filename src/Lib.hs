{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( getQueue
  , Q
  , chat_id
  , caption
  , file
  , method
  )
where

import           System.Directory               ( listDirectory )
import           Data.Aeson
import           GHC.Generics


data Q = Q
    { method  :: String
    , caption :: String
    , chat_id :: Int
    , file    :: Maybe String
    }
    deriving (Generic, Show)

instance ToJSON Q
instance FromJSON Q

getQueue :: String -> IO ([FilePath])
getQueue d = do
  let a = listDirectory d
  map (d ++) <$> a


