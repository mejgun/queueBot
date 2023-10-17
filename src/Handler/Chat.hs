module Handler.Chat (handle, handlenew) where

import Data.List (nub)
import Lib
  ( BotState (..),
    Status (..),
  )
import TD.Data.Chat (Chat (Chat, _id))
import TD.Lib.Internal (Extra)

update :: Int -> BotState -> BotState
update i st = st {chats = nub (i : st.chats)}

handle :: BotState -> Chat -> Maybe Extra -> BotState
handle st@BotState {status = WaitingChat xtra1} (Chat {_id = Just _id}) (Just xtra2)
  | xtra1 == xtra2 = update _id st {status = Ready}
  | otherwise = update _id st
handle st (Chat {_id = Just _id}) _ = update _id st
handle st _ _ = st

handlenew :: BotState -> Chat -> BotState
handlenew st (Chat {_id = Just _id}) = update _id st
handlenew st _ = st