module Handler.Auth (handle) where

import Data.Text qualified as T
import Lib
  ( BotState (apihash, apiid, client),
  )
import TD.Data.AuthorizationState
  ( AuthorizationState
      ( AuthorizationStateWaitPhoneNumber,
        AuthorizationStateWaitTdlibParameters
      ),
  )
import TD.Lib (send)
import TD.Query.CheckAuthenticationBotToken
  ( CheckAuthenticationBotToken (CheckAuthenticationBotToken, token),
  )
import TD.Query.SetTdlibParameters
  ( SetTdlibParameters (..),
    defaultSetTdlibParameters,
  )

handle :: BotState -> AuthorizationState -> IO ()
handle st AuthorizationStateWaitTdlibParameters =
  send st.client (tdlibParameters st.apiid st.apihash)
handle st AuthorizationStateWaitPhoneNumber = do
  putStrLn "Enter bot token"
  t <- getLine
  send st.client CheckAuthenticationBotToken {token = Just $ T.pack t}
handle _ _ = pure ()

tdlibParameters :: Int -> T.Text -> SetTdlibParameters
tdlibParameters apiid apihash =
  defaultSetTdlibParameters
    { ignore_file_names = Just True,
      enable_storage_optimizer = Just True,
      application_version = Just "0.1.0.0",
      system_language_code = Just "en",
      api_hash = Just apihash,
      device_model = Just "Server",
      api_id = Just apiid,
      files_directory = Just "tdlib_files",
      database_directory = Just "tdlib_db",
      database_encryption_key = Just "randomencryption"
    }