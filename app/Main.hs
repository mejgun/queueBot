{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import API.AuthorizationState
import qualified API.FormattedText as FT
import API.Functions.CheckAuthenticationBotToken
import API.Functions.CheckDatabaseEncryptionKey
import qualified API.Functions.SendMessage as SM
import API.Functions.SetLogVerbosityLevel
import API.Functions.SetTdlibParameters
import API.GeneralResult
import qualified API.InputMessageContent as IMC
import qualified API.Message as M
import qualified API.Update as U
import Data.Aeson
import Data.Maybe
  ( fromJust,
    isJust,
    isNothing,
  )
import Defaults
import Lib
import System.Directory (removeFile)
import TDLib

data State = State
  { currentSendingExtra :: Maybe String,
    currentSendingMessage :: Maybe M.Message,
    queue :: [FilePath],
    currentItem :: Maybe FilePath
  }
  deriving (Show)

queueDir :: String
queueDir = "./queue/"

main :: IO ()
main = do
  client <- create
  send client SetLogVerbosityLevel {new_verbosity_level = Just 2}
  mainLoop client emptyState

mainLoop :: Client -> State -> IO ()
mainLoop c st = do
  r <- receive c
  case r of
    -- no msg from tdlib. check if we sending someting already
    -- if not, update queue or send first message
    Nothing ->
      if isNothing (currentSendingExtra st)
        && isNothing (currentSendingMessage st)
        then case queue st of
          [] -> do
            q <- getQueue queueDir
            mainLoop c $ newQueueState st q
          (q : qs) -> do
            newSt <- sendMsg c q st
            mainLoop c $ newQueueState newSt qs
        else mainLoop c st
    -- new message from tdlib
    Just (ResultWithExtra res extra) -> do
      print res
      -- have extra and it is eq to currentSendingExtra
      if isJust (currentSendingExtra st) && extra == currentSendingExtra st
        then case res of
          Message m -> mainLoop c $ messageSendingState st m
          p -> do
            printError p
            mainLoop c $ messageSendFailedState st
        else -- ignore extra, just handle result
        do
          newSt <- handleResult res c st
          mainLoop c newSt

sendMsg :: Client -> FilePath -> State -> IO State
sendMsg c q st = do
  j <- decodeFileStrict q :: IO (Maybe Q)
  case j of
    Nothing -> return st
    Just msg -> do
      extra <- handleQeueueMessage msg c
      return $ messageCreatingState st extra q

printError :: (Show a) => a -> IO ()
printError e = putStrLn "ERROR:" >> print e

handleResult :: GeneralResult -> Client -> State -> IO State
handleResult (Update (U.UpdateAuthorizationState {U.authorization_state = s})) c st =
  handleAuthState c s >> return st
handleResult (Update (U.UpdateMessageSendFailed {U.old_message_id = oldID})) _ st =
  return $
    if isMessageIdEqualToId (currentSendingMessage st) oldID
      then messageSendFailedState st
      else st
handleResult (Update (U.UpdateMessageSendSucceeded {U.old_message_id = oldID})) _ st =
  if isMessageIdEqualToId (currentSendingMessage st) oldID
    then
      removeFile (fromJust (currentItem st))
        >> return (messageSendSuccessState st)
    else return st
handleResult _ _ st = return st

isMessageIdEqualToId :: Maybe M.Message -> Maybe Int -> Bool
isMessageIdEqualToId Nothing _ = False
isMessageIdEqualToId m mid =
  let mID = M._id =<< m
      Just t = (==) <$> mID <*> mid
   in t

handleAuthState :: Client -> Maybe AuthorizationState -> IO ()
handleAuthState c s = do
  case s of
    Just AuthorizationStateWaitTdlibParameters ->
      send c SetTdlibParameters {parameters = Just defaultTdlibParameters}
    Just (AuthorizationStateWaitEncryptionKey _) ->
      send
        c
        CheckDatabaseEncryptionKey {encryption_key = Just "randomencryption"}
    Just AuthorizationStateWaitPhoneNumber -> do
      putStrLn "Enter bot token"
      t <- getLine
      send c CheckAuthenticationBotToken {token = Just t}
    _ -> return ()

handleQeueueMessage :: Q -> Client -> IO String
handleQeueueMessage msg c = case method msg of
  "sendText" -> sendWExtra c $ sendTextMsg (chat_id msg) (caption msg)
  _ -> fail "Cannot match"

emptyState :: State
emptyState =
  State
    { currentSendingExtra = Nothing,
      currentSendingMessage = Nothing,
      queue = [],
      currentItem = Nothing
    }

messageCreatingState :: State -> String -> FilePath -> State
messageCreatingState st extra curItem =
  st
    { currentSendingMessage = Nothing,
      currentSendingExtra = Just extra,
      currentItem = Just curItem
    }

messageSendingState :: State -> M.Message -> State
messageSendingState st m =
  st {currentSendingMessage = Just m, currentSendingExtra = Nothing}

newQueueState :: State -> [FilePath] -> State
newQueueState st q = st {queue = q}

messageSendFailedState :: State -> State
messageSendFailedState st =
  st
    { currentSendingMessage = Nothing,
      currentSendingExtra = Nothing,
      currentItem = Nothing
    }

messageSendSuccessState :: State -> State
messageSendSuccessState st =
  st
    { currentSendingMessage = Nothing,
      currentSendingExtra = Nothing,
      currentItem = Nothing
    }

sendTextMsg :: Int -> String -> SM.SendMessage
sendTextMsg cID text =
  SM.SendMessage
    { SM.chat_id = Just cID,
      SM.reply_to_message_id = Nothing,
      SM.reply_markup = Nothing,
      SM.options = Nothing,
      SM.message_thread_id = Nothing,
      SM.input_message_content =
        Just
          IMC.InputMessageText
            { IMC.clear_draft = Nothing,
              IMC.disable_web_page_preview = Nothing,
              IMC.text =
                Just
                  FT.FormattedText
                    { FT.text = Just text,
                      FT.entities = Nothing
                    }
            }
    }
