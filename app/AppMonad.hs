{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AppMonad where

import Prelude hiding (fail)
import Control.Concurrent (forkIO)
import qualified Data.Aeson as Aeson (ToJSON, encode)
import qualified Network.Wai as Wai 
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as H 

import qualified Rel.Cmd as Cmd
import qualified Rel.Log as Log
import qualified Rel.FS as FS
import qualified Rel.Git as Git
import qualified Rel.Github as Github
import Monad.Result
import Monad.RelMonad
import Util.Request

data RequestState =
    NoRequest
  | Waiting (Wai.Response -> IO Wai.ResponseReceived)
  | Completed (IO Wai.ResponseReceived)

data AppState = AppState
  { requestState :: RequestState
  , config       :: AppConfig
  }

data AppConfig = AppConfig
  { configPath   :: FilePath
  , storageRoot  :: FilePath
  , keystorePath :: FilePath
  , pidFile      :: FilePath
  , daemonMode   :: Bool
  , listenPort   :: Int
  , cmdConfig    :: Cmd.Config
  , fsConfig     :: FS.Config
  , gitConfig    :: Git.Config
  , githubConfig :: Github.Config
  , logConfig    :: Log.Config
  }

data App a = App { runApp :: AppState -> IO (AppState, Result a) }

instance Functor App where
  fmap f ma = App $ \st -> fmap2 (fmap f) $ runApp ma st

instance Applicative App where
  pure x   = App $ \st -> return (st, return x)
  ff <*> g = App $ \st -> do 
    (st',  rf) <- runApp ff st
    (st'', rx) <- runApp g st'
    return (st'', rf <*> rx)

instance Monad App where
  return x = pure x
  ma >>= f = App $ \st -> do
    (st', rx) <- runApp ma st
    runApp (unwrap $ fmap f rx) st'

instance ResultantMonad App where
  point x        = App $ \st -> fmap ((,) st) x
  mapResult f ma = App $ \st -> do
    (st', rx) <- runApp ma st
    return (st', f rx)

instance RelMonad Log.Log App where
  rPoint x = App . statePass $ Log.runLog x . logConfig . config

instance RelMonad FS.FS App where
  rPoint x = App . statePass $ FS.runFS x . fsConfig . config

instance RelMonad Git.Git App where
  rPoint x = App $ \st -> do (c, rx) <- Git.runGit x (gitConfig $ config st)
                             return $ (st { config = (config st) { gitConfig = c } }, rx)

instance RelMonad Github.Github App where
  rPoint x = App . statePass $ Github.runGithub x . githubConfig . config

instance RelMonad Cmd.Cmd App where
  rPoint x = App $ \st -> do (c, rx) <- Cmd.runCmd x (cmdConfig $ config st)
                             return $ (st { config = (config st) { cmdConfig = c } }, rx)


getStorageRoot :: App FilePath
getStorageRoot = withConfig $ return . storageRoot

getKeystorePath :: App FilePath
getKeystorePath = withConfig $ return . keystorePath

getConfigPath :: App FilePath
getConfigPath = withConfig $ return . configPath

getDaemonMode :: App Bool
getDaemonMode = withConfig $ return . daemonMode

getRespond :: App (Wai.Response -> IO Wai.ResponseReceived)
getRespond = withState $ \(AppState reqState _) ->
  case reqState of
    Waiting f   -> return f
    NoRequest   -> fail "No active request"
    Completed _ -> fail "Request already completed"

setConfigFilePath :: FilePath -> App ()
setConfigFilePath x = updateConfig $ \c -> c { configPath = x }

setDaemonMode :: Bool -> App ()
setDaemonMode x = updateConfig $ \c -> c { daemonMode = x }

setState :: AppState -> App ()
setState st = App $ \_ -> return (st, return ())

setConfig :: AppConfig -> App ()
setConfig c = App $ \(AppState r _) -> return (AppState r c, return ())

withConfig :: (AppConfig -> App a) -> App a
withConfig f = withState $ \(AppState _ c) -> f c

getConfig :: App AppConfig
getConfig = withConfig return

withState :: (AppState -> App a) -> App a
withState f = App $ \c -> runApp (f c) c

updateState :: (AppState -> AppState) -> App ()
updateState f = withState $ \st -> setState (f st)

updateConfig :: (AppConfig -> AppConfig) -> App ()
updateConfig f = withConfig $ \c -> setConfig (f c)

statePass :: (c -> IO (Result a)) -> c -> IO (c, Result a)
statePass f c = (,) c `fmap` f c

initState :: (Wai.Response -> IO Wai.ResponseReceived) -> AppConfig -> AppState
initState resp conf = AppState (Waiting resp) conf

respond :: H.Status -> H.ResponseHeaders -> BL8.ByteString -> App ()
respond status headers body = do
  Log.trace ("Sending response: " ++ (BL8.unpack body))
  withState $ \(AppState req c) ->
    case req of NoRequest   -> fail "No active request."
                Waiting f   -> setState $ AppState (Completed (f response)) c
                Completed _ -> fail "Request already completed."
  where response = Wai.responseLBS status headers body

getResponseReceived :: App Wai.ResponseReceived
getResponseReceived = withState $ \(AppState r _) ->
  case r of Completed x -> safe x
            _           -> fail "Request did not generate a response."

basicHeaders :: H.ResponseHeaders
basicHeaders = [(CI.mk $ B8.pack "Content-Type", B8.pack "text/plain")]

clientError :: String -> App ()
clientError e = do
  Log.info $ "Client error: " ++ e
  respond H.status400 basicHeaders . Aeson.encode $ Response "error" e
  fail e

serverError :: String -> App ()
serverError e = do
  Log.info $ "Server error: " ++ e
  respond H.status500 basicHeaders . Aeson.encode $ Response "error" e
  fail e

ok :: Aeson.ToJSON a => a -> App ()
ok = respond H.status200 basicHeaders . Aeson.encode

-- Assert a condition, respond to client with error upon failure
mandatory :: App Bool -> String -> App ()
mandatory k = recover' clientError . assert k

fork :: App a -> App ()
fork f = withState $ \st -> safe $ forkIO ((runApp f st) >> return ()) >> return ()

