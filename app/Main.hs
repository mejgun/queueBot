-- {-# OPTIONS_GHC -W -Wall -Werror #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Handler.Auth qualified as Auth
import Handler.Chat qualified as Chat
import Handler.Error qualified as Error
import Handler.Message qualified as Message
import Handler.Option qualified as Option
import Lib
  ( BotState (..),
    QItem (caption, chat_id, method),
    Queue,
    Status (Ready, WaitingChat, WaitingMessageACK),
    emptyQueue,
    headQueue,
    updateQueue,
  )
import System.Directory.Internal.Prelude (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import TD.Data.FormattedText qualified as FT
import TD.Data.InputMessageContent (InputMessageContent (link_preview_options))
import TD.Data.InputMessageContent qualified as IMC
import TD.Data.Update (Update (UpdateNewChat))
import TD.Data.Update qualified as U
import TD.GeneralResult
  ( GeneralResult (Chat, Error, Message, Update),
  )
import TD.Lib (Extra, ShortShow (..), create, receive, send, sendWExtra)
import TD.Query.GetChat (GetChat (..))
import TD.Query.SendMessage qualified as SM
import TD.Query.SetLogVerbosityLevel
  ( SetLogVerbosityLevel (SetLogVerbosityLevel, new_verbosity_level),
  )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  getArgs >>= \case
    [apiid, apihash] -> do
      cl <- create
      send cl SetLogVerbosityLevel {new_verbosity_level = Just 2}
      mainLoop $
        BotState
          { client = cl,
            queue = emptyQueue,
            status = Ready,
            chats = [],
            myId = Nothing,
            apiid = read apiid,
            apihash = T.pack apihash
          }
    _ -> putStrLn "run as ./app api_id api_hash"

mainLoop :: BotState -> IO ()
mainLoop st = do
  r <- receive st.client
  case r of
    -- no msg from tdlib. check if we sending someting already
    -- if not, update queue or send first message
    Nothing ->
      if st.status == Ready
        then handleQueue st.queue
        else mainLoop st
    -- new message from tdlib
    Just (res, extra) -> do
      putStrLn $ shortShow res
      newSt <- handleAnswer res extra st
      mainLoop newSt
  where
    handleQueue :: Queue -> IO ()
    handleQueue q =
      case headQueue q of
        (Just m) -> handleQueueMessage st m >>= mainLoop
        Nothing -> updateQueue >>= \xs -> mainLoop $ st {queue = xs}

handleQueueMessage :: BotState -> QItem -> IO BotState
handleQueueMessage st msg = case msg.method of
  "sendText" -> do
    if msg.chat_id `elem` st.chats
      then do
        xtra <-
          sendWExtra st.client $
            sendTextMsg msg.chat_id $
              fromMaybe "defaultText" msg.caption
        pure $ st {status = WaitingMessageACK xtra}
      else do
        xtra <- sendWExtra st.client $ getChat msg.chat_id
        pure $ st {status = WaitingChat xtra}
  _ ->
    putStrLn ("cannot parse" <> show msg)
      >> pure st

handleAnswer :: GeneralResult -> Maybe Extra -> BotState -> IO BotState
handleAnswer (Update U.UpdateAuthorizationState {authorization_state = Just s}) _ st =
  Auth.handle st s >> pure st
handleAnswer (Update U.UpdateOption {U.name = Just k, U.value = Just v}) _ st =
  pure $ Option.handle st (k, v)
handleAnswer (Update (UpdateNewChat {chat = Just chat})) _ st =
  pure $ Chat.handlenew st chat
handleAnswer (Chat chat) xtra st =
  pure $ Chat.handle st chat xtra
handleAnswer (Error _) xtra st =
  pure $ Error.handle st xtra
handleAnswer (Update U.UpdateMessageSendFailed {old_message_id = Just m}) _ st =
  pure $ Message.handleFailed st m
handleAnswer (Update U.UpdateMessageSendSucceeded {old_message_id = Just m}) _ st =
  Message.handleSuccess st m
handleAnswer (Message m) xtra st =
  pure $ Message.handleMessage st m xtra
handleAnswer _ _ st = pure st

sendTextMsg :: Int -> String -> SM.SendMessage
sendTextMsg cID text =
  let t =
        FT.defaultFormattedText
          { FT.text = Just (T.pack text)
          }
      c =
        IMC.InputMessageText
          { text = Just t,
            link_preview_options = Nothing,
            clear_draft = Nothing
          }
   in SM.defaultSendMessage
        { SM.chat_id = Just cID,
          SM.input_message_content = Just c
        }

getChat :: Int -> GetChat
getChat cId = GetChat {chat_id = Just cId}
