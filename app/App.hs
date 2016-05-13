{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

import Prelude hiding (fail, lookup)
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai 
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Posix.Daemonize as Daemonize
import qualified System.Posix.Process as Proc
import qualified Data.Streaming.Network as NetStream
import qualified Network.Socket as Socket
import qualified System.Posix.Signals as Signals

import qualified Rel.FS as FS
import qualified Rel.Log as Log
import qualified Push
import qualified Keys
import Monad.Result
import Util.Request (Request(Request), readRequest, withPath, errorResponse)
import AppMonad
import Config


processRequest :: Request -> App Wai.ResponseReceived
processRequest req@(Request path _ _ _) =
  do
    Log.info $ "Request: " ++ (show path)
    case path of
      "push" : path' -> Push.request (withPath path' req) 
      "keys" : path' -> Keys.request (withPath path' req) 
      _              -> fail $ "Unsupported path: " ++ (show path)
    getResponseReceived

app :: AppConfig -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
app config waiRequest wRespond =
  do
    req     <- readRequest waiRequest 
    (st, r) <- runApp (processRequest req) $ AppState (Waiting wRespond) config
    case r of Ok x  -> return x
              Err e -> do logError $ "Error handling request: " ++ e
                          ensureReply st
  where
    ensureReply (AppState responseState _) = case responseState of
      Completed x -> x
      _ -> do logDebug "Request handler failed without sending a response."
              wRespond . Wai.responseLBS Http.status500 basicHeaders . Aeson.encode $ errorResponse "Error"
    logDebug e = runApp (Log.debug e) (AppState NoRequest config) >> return ()
    logError e = runApp (Log.error e) (AppState NoRequest config) >> return ()

daemonize :: App a -> App ()    
daemonize x =
  do st <- getState 
     let r = runApp x st >> return ()
     safe $ Daemonize.daemonize r
     return () 

initServer :: App ()
initServer =
  do c <- getConfig
     Log.info "Starting up..."
     sock <- createSocket (listenPort c)
     _ <- safe $ Signals.installHandler Signals.sigINT (handleSIGINT sock) Nothing 
     _ <- safe $ Signals.installHandler Signals.sigTERM (handleSIGTERM sock) Nothing 
     safe $ Warp.runSettingsSocket Warp.defaultSettings sock (app c)

runAsDaemon :: App ()
runAsDaemon = 
  do pidFile <- fmap pidFile getConfig
     prohibit (FS.isFile pidFile) $ 
       "PID file found at: " ++ pidFile ++ ". Is pure already running?"
     daemonize $ 
       (safe $ Proc.getProcessID >>= writeFile pidFile . show)
       >> (Log.trace $ "Wrote PID file to " ++ pidFile)
       >> initServer

appMain :: App ()
appMain = do
  readCliOpts
  loadConfig
  config <- getConfig
  if daemonMode config
  then runAsDaemon
  else initServer

handleSIGINT :: Socket.Socket -> Signals.Handler
handleSIGINT sock = Signals.Catch $ 
  do putStrLn "Process received SIGINT, shutting down."
     Socket.close sock

handleSIGTERM :: Socket.Socket -> Signals.Handler
handleSIGTERM sock = Signals.Catch $ 
  do putStrLn "Process received SIGTERM. Shutting down..."
     Socket.close sock

createSocket :: Int -> App Socket.Socket
createSocket port = 
  do sock <- safe $ NetStream.bindPortTCP port "*4"
     setRequestState $ Listening sock
     return sock

main :: IO ()
main = runApp appMain initState >>= \(_, r) ->
  case r of Ok _  -> return ()
            Err e -> putStrLn $ "Unrecoverable error: " ++ e
  where initState = AppState NoRequest appConfig

