module Handler.Message (handleFailed, handleSuccess, handleMessage) where

import Lib
  ( BotState (BotState, queue, status),
    Status (Ready, WaitingMessageACK, WaitingMessageSending),
    removeHead,
  )
import TD.Data.Message (Message (Message, _id))
import TD.Lib.Internal (Extra)

handleFailed :: BotState -> Int -> BotState
handleFailed st@BotState {status = WaitingMessageSending m1} m2
  | m1 == m2 = st {status = Ready}
handleFailed st _ = st

handleSuccess :: BotState -> Int -> IO BotState
handleSuccess st@BotState {status = WaitingMessageSending m1} m2
  | m1 == m2 = do
      q <- removeHead st.queue
      pure $ st {status = Ready, queue = q}
handleSuccess st _ = pure st

handleMessage :: BotState -> Message -> Maybe Extra -> BotState
handleMessage
  st@BotState {status = WaitingMessageACK xtra1}
  Message {_id = Just _id}
  (Just xtra2)
    | xtra1 == xtra2 = st {status = WaitingMessageSending _id}
handleMessage st _ _ = st