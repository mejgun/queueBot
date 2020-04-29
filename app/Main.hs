{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Lib

import           API.GeneralResult
import           Defaults
import           TDLib

import           API.Functions.CheckAuthenticationBotToken
import           API.Functions.CheckDatabaseEncryptionKey
import qualified API.Functions.SendMessage                 as SM
import           API.Functions.SetLogVerbosityLevel
import           API.Functions.SetTdlibParameters

import           API.AuthorizationState
import qualified API.FormattedText                         as FT
import qualified API.InputMessageContent                   as IMC
import qualified API.Message                               as M
import qualified API.Update                                as U


import           Control.Monad                             (join)
import           System.Directory                          (removeFile)


data State = State
    { currentSendingExtra   :: Maybe String
    , currentSendingMessage :: Maybe M.Message
    , queue                 :: [FilePath]
    , currentItem           :: Maybe FilePath
    }
    deriving (Show)

queueDir :: String
queueDir = "./queue/"

main :: IO ()
main = do
  client <- create
  send client SetLogVerbosityLevel { new_verbosity_level = Just 2 }
  mainLoop client emptyState

mainLoop :: Client -> State -> IO ()
mainLoop c st = do
  r <- receive c
  case r of
    -- no msg from tdlib
    Nothing ->
      if currentSendingExtra st == Nothing && currentSendingMessage st == Nothing
      then
        case queue st of
          [] -> do
            q <- getQueue queueDir
            let newSt = newQueueState st q in mainLoop c newSt
          (q:qs) -> do
            j <- decodeFileStrict q :: IO (Maybe Q)
            case j of
              Nothing -> let newSt = newQueueState st qs in mainLoop c newSt
              Just msg -> do
                extra <- handleQeueueMessage msg c
                let n = messageCreatingState st extra q
                    newSt = newQueueState n qs in mainLoop c newSt
      else mainLoop c st
    -- new message from tdlib
    Just (ResultWithExtra res extra) -> do
      print res
      -- have extra and it is eq to currentSendingExtra
      if currentSendingExtra st /= Nothing && extra == currentSendingExtra st
        then case res of
          Message m -> let newSt = messageSendingState st m in mainLoop c newSt
          p ->
            let newSt = messageSendFailedState st                  
            in  do
                  putStrLn "ERROR:"
                  print p
                  mainLoop c newSt
        else return()
      case res of
        Update (U.UpdateAuthorizationState { U.authorization_state = s }) -> do
          handleAuthState c s
          mainLoop c st
        Update (U.UpdateMessageSendFailed { U.old_message_id = oldID }) ->
          let m = currentSendingMessage st
              mID = join $ M._id <$> m
              Just t = (==) <$> mID <*> oldID
          in  if t then let newSt = messageSendFailedState st in mainLoop c newSt else mainLoop c st
        Update (U.UpdateMessageSendSucceeded { U.old_message_id = oldID }) ->
          let m = currentSendingMessage st
              mID = join $ M._id <$> m
              t      = (==) <$> mID <*> oldID
          in  case t of
                Just True -> do
                  let Just i = currentItem st in removeFile i
                  let newSt = messageSendSuccessState st in mainLoop c newSt
                _ -> mainLoop c st
        _ -> mainLoop c st

handleAuthState :: Client -> Maybe AuthorizationState -> IO ()
handleAuthState c s = do
  case s of
    Just AuthorizationStateWaitTdlibParameters   -> send c SetTdlibParameters { parameters = Just defaultTdlibParameters }
    Just (AuthorizationStateWaitEncryptionKey _) -> send c CheckDatabaseEncryptionKey { encryption_key = Just "randomencryption" }
    Just AuthorizationStateWaitPhoneNumber       -> do
      putStrLn "Enter bot token"
      t <- getLine
      send c CheckAuthenticationBotToken { token = Just t }
    _ -> return ()

handleQeueueMessage :: Q -> Client -> IO String
handleQeueueMessage msg c = case method msg of
  "sendText" -> sendWExtra c $ sendTextMsg (chat_id msg) (caption msg)
  _ -> fail "Cannot match"

emptyState :: State
emptyState = State { currentSendingExtra = Nothing, currentSendingMessage = Nothing, queue = [], currentItem = Nothing }

messageCreatingState :: State -> String -> FilePath -> State
messageCreatingState st extra curItem = st { currentSendingMessage = Nothing, currentSendingExtra = Just extra, currentItem = Just curItem }

messageSendingState :: State -> M.Message -> State
messageSendingState st m = st { currentSendingMessage = Just m, currentSendingExtra = Nothing }

newQueueState :: State -> [FilePath] -> State
newQueueState st q = st { queue = q }

messageSendFailedState :: State -> State
messageSendFailedState st = st { currentSendingMessage = Nothing, currentSendingExtra = Nothing, currentItem = Nothing }

messageSendSuccessState :: State -> State
messageSendSuccessState st = st { currentSendingMessage = Nothing, currentSendingExtra = Nothing, currentItem = Nothing }

sendTextMsg :: Int -> String -> SM.SendMessage
sendTextMsg cID text = SM.SendMessage
  { SM.chat_id               = Just cID
  , SM.reply_to_message_id   = Nothing
  , SM.reply_markup          = Nothing
  , SM.options               = Nothing
  , SM.input_message_content = Just IMC.InputMessageText
                                 { IMC.clear_draft = Nothing
                                 , IMC.disable_web_page_preview = Nothing
                                 , IMC.text = Just FT.FormattedText { FT.text = Just text, FT.entities = Nothing }
                                 }
  }
