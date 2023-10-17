module Handler.Error (handle) where

import Lib (BotState (BotState, status), Status (Ready, WaitingChat, WaitingMessageACK))
import TD.Lib.Internal (Extra)

handle :: BotState -> Maybe Extra -> BotState
handle st@BotState {status = WaitingChat xtra2} (Just xtra1)
  | xtra1 == xtra2 = st {status = Ready}
handle st@BotState {status = WaitingMessageACK xtra2} (Just xtra1)
  | xtra1 == xtra2 = st {status = Ready}
handle st _ = st
