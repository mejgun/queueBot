module Handler.Option (handle) where

import Data.Text qualified as T
import Lib
  ( BotState (myId),
  )
import TD.Data.OptionValue (OptionValue (OptionValueInteger, _value))

handle :: BotState -> (T.Text, OptionValue) -> BotState
handle st ("my_id", OptionValueInteger {_value = val}) = st {myId = val}
handle st _ = st
