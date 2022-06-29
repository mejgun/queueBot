{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -W -Wall -Werror #-}

module Main (main) where

import Data.Aeson (decodeFileStrict)
import Lib
import System.Directory (removeFile)
import TD.Data.AuthorizationState
import qualified TD.Data.FormattedText as FT
import TD.Data.GeneralResult
import qualified TD.Data.InputMessageContent as IMC
import qualified TD.Data.Message as M
import qualified TD.Data.Update as U
import TD.Defaults (defaultTdlibParameters)
import TD.Lib
import TD.Query.CheckAuthenticationBotToken
import TD.Query.CheckDatabaseEncryptionKey
import qualified TD.Query.SendMessage as SM
import TD.Query.SetLogVerbosityLevel
import TD.Query.SetTdlibParameters

type Extra = String

data Status
  = Empty
  | WaitMsg Extra FilePath
  | WaitMsgSending M.Message FilePath
  deriving (Show, Eq)

data BotState = BotState
  { client :: Client,
    status :: Status,
    queue :: [FilePath]
  }
  deriving (Show)

queueDir :: String
queueDir = "./queue/"

main :: IO ()
main = do
  cl <- create
  send cl SetLogVerbosityLevel {new_verbosity_level = Just 2}
  mainLoop $ BotState {client = cl, queue = [], status = Empty}

mainLoop :: BotState -> IO ()
mainLoop st = do
  r <- receive $client st
  case r of
    -- no msg from tdlib. check if we sending someting already
    -- if not, update queue or send first message
    Nothing ->
      if status st == Empty
        then handleQueue (queue st)
        else mainLoop st
    -- new message from tdlib
    Just (ResultWithExtra res extra) -> do
      print res
      newSt <- handleResultAndExtra res extra st
      mainLoop newSt
  where
    handleQueue :: [FilePath] -> IO ()
    handleQueue [] = do
      q <- getQueue queueDir
      mainLoop $ newQueueState st q
    handleQueue (q : qs) = do
      newSt <- handleMsg q
      mainLoop $ newQueueState newSt qs

    handleMsg :: FilePath -> IO BotState
    handleMsg f = do
      j <- decodeFileStrict f :: IO (Maybe Q)
      case j of
        Nothing -> return st
        Just msg -> handleQeueueMessage f msg

    handleQeueueMessage :: FilePath -> Q -> IO BotState
    handleQeueueMessage f msg = case method msg of
      "sendText" -> do
        xtr <- sendWExtra (client st) $ sendTextMsg (chat_id msg) (caption msg)
        pure $ st {status = WaitMsg xtr f}
      _ -> do
        printError ("cannot parse" :: String)
        printError msg
        pure st

printError :: (Show a) => a -> IO ()
printError e = putStrLn "ERROR:" >> print e

handleResultAndExtra :: GeneralResult -> Maybe Extra -> BotState -> IO BotState
-- auth msg
handleResultAndExtra
  (Update U.UpdateAuthorizationState {U.authorization_state = s})
  _
  st =
    handleAuthState (client st) s >> pure st
-- message sending start
handleResultAndExtra
  (Message m)
  (Just extra1)
  st@(BotState _ (WaitMsg extra2 f) _)
    | extra1 == extra2 =
      pure $ st {status = WaitMsgSending m f}
-- message sending failed
handleResultAndExtra
  (Update U.UpdateMessageSendFailed {U.old_message_id = oldID})
  _
  st@(BotState _ (WaitMsgSending M.Message {M._id = mId} f) _)
    | oldID == mId = pure $ st {status = Empty, queue = queue st ++ [f]}
-- message sending succeeded
handleResultAndExtra
  (Update U.UpdateMessageSendSucceeded {U.old_message_id = oldID})
  _
  st@(BotState _ (WaitMsgSending M.Message {M._id = mId} f) _)
    | oldID == mId = do
      removeFile f
      pure $ st {status = Empty}
-- uknown msg. ignoring
handleResultAndExtra _ _ st = pure st

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

newQueueState :: BotState -> [FilePath] -> BotState
newQueueState st q = st {queue = q}

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
