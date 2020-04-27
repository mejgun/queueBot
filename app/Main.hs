module Main where

import           Data.Aeson
import           Lib

import           API.GeneralResult
import           TDLib
import qualified API.Update                    as U

import           System.Directory               (removeFile)


data State = State
    { currentSendingExtra     :: Maybe String
    , currentSendingMessage   :: Maybe M.Message
    , queue :: [FilePath]
    , currentItem :: [FilePath]
    }
    deriving Show

queueDir = "./queue/"

main :: IO ()
main = do
  client <- create
  send client SetLogVerbosityLevel { new_verbosity_level = Just 2 }
  a <- getQueue queueDir
  mapM_
    (\f -> do
      t <- decodeFileStrict (f) :: IO (Maybe Q)
      case t of
        Just q -> do
          print (chat_id q)
          print (caption q)
          print (file q)
          print (method q)
        _ -> return ()
      putStrLn "-"
    )
    a

mainLoop :: Client -> State -> IO ()
mainLoop c st = do
  r <- receive c
  case r of
    Just (ResultWithExtra res extra) -> do
      if currentSendingExtra st /= Nothing && extra == currentSendingExtra st
        then case res of
          Message m ->
            let
              newSt = st { currentSendingMessage = Just m
                         , currentSendingExtra   = Nothing
                         }
            in  mainLoop c newSt
          p ->
            let newSt = st { queue               = (currentItem st) : (queue st)
                           , currentSendingExtra = Nothing
                           }
            in  do
                  putStrLn "ERROR:"
                  print p
                  mainLoop c newSt
        else return ()
      case res of
        Update (U.UpdateAuthorizationState { U.authorization_state = s }) -> do
          handleAuthState c s
          mainLoop c st
        Update (U.UpdateMessageSendFailed { U.old_message_id = oldID }) ->
          let Just m   = currentSendingMessage st
              Just t   = (==) <$> M._id m <*> oldID
              Just cur = currentAnsweringMessage st
          in  if t
                then
                  let newSt = st { currentSendingMessage = Nothing
                                 , currentSendingExtra = Nothing
                                 , queue = (currentItem st) : (queue st)
                                 }
                  in  mainLoop c newSt
                else mainLoop c st
        Update (U.UpdateMessageSendSucceeded { U.old_message_id = oldID }) ->
          let Just m = currentSendingMessage st
              t      = (==) <$> M._id m <*> oldID
          in  case t of
                Just True -> do
                  removeFile currentItem st
                  let newSt = st { currentSendingMessage = Nothing
                                            , currentSendingExtra = Nothing 
                                            , currentItem = Nothing                                            
                                            } in mainLoop c newSt
                _         -> mainLoop c st
        _ -> mainLoop c st

handleAuthState :: Client -> Maybe AuthorizationState -> IO ()
handleAuthState c s = do
  case s of
    Just AuthorizationStateWaitTdlibParameters ->
      send c SetTdlibParameters { parameters = Just defaultTdlibParameters }
    Just (AuthorizationStateWaitEncryptionKey _) -> send
      c
      CheckDatabaseEncryptionKey { encryption_key = Just "randomencryption" }
    Just AuthorizationStateWaitPhoneNumber -> do
      putStrLn "Enter bot token"
      token <- getLine
      send c CheckAuthenticationBotToken { token = Just token }
    _ -> return ()
