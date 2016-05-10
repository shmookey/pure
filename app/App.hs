{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

import Prelude hiding (fail, lookup)
import qualified Data.Aeson as Aeson
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai 
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as O
import qualified System.Posix.Daemonize as Daemonize
import qualified System.Posix.Process as Proc
import Options.Applicative ((<>))
import Data.List (intercalate, sort)
import Data.List.Ordered (subset, minus')

import qualified Rel.Cmd as Cmd
import qualified Rel.FS as FS
import qualified Rel.Git as Git
import qualified Rel.Github as Github
import qualified Rel.Log as Log
import qualified Push
import qualified Keys
import Monad.Result
import Util.Request (Request(Request), readRequest, withPath, errorResponse)
import AppMonad


data CliOpts = CliOpts
  { configFilePath   :: String
  , enableDaemonMode :: Bool
  }

instance CT.Configured Log.Level where
  convert (CT.String x) = toMaybe . Log.parseLevel $ T.unpack x
  convert _             = Nothing


appConfig :: AppConfig
appConfig = AppConfig
  { configPath   = "/etc/pure.conf"
  , storageRoot  = "/var/run/pure/repos"
  , keystorePath = "/var/run/pure/keys"
  , pidFile      = "/var/run/pure/pure.pid"
  , listenPort   = 3000
  , daemonMode   = False
  , cmdConfig    = Cmd.Config 
    { Cmd.envVars = Map.fromList [ ("PATH", "/usr/local/bin:/usr/bin:/bin") ]
    }
  , gitConfig    = Git.Config
    { Git.identity = Nothing
    }
  , fsConfig     = FS.Config {}
  , githubConfig = Github.Config {}
  , logConfig    = Log.Config
    { Log.path  = Nothing
    , Log.level = Log.INFO
    , Log.print = True
    }
  }

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
    (st, r) <- runApp (processRequest req) (initState wRespond config)
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

loadConfig :: App ()
loadConfig = 
  do
    cfgPath      <- getConfigPath
    current      <- getConfig
    c            <- safe $ C.load [ C.Optional cfgPath ]
    keys         <- safe $ (sort . map T.unpack . HM.keys) `fmap` C.getMap c
    keystorePath <- getOr c "keystore.path"   $ keystorePath current 
    storageRoot  <- getOr c "repocache.path"  $ storageRoot current 
    listenPort   <- getOr c "server.port"     $ listenPort current 
    pidFile      <- getOr c "server.pidFile"  $ pidFile current 
    logPath      <- get   c "server.logFile"
    logLevel     <- getOr c "server.logLevel" . Log.level $ logConfig current
    setConfig $ current 
      { storageRoot, keystorePath, pidFile, listenPort
      , logConfig = (logConfig current) 
        { Log.path  = logPath
        , Log.level = logLevel } 
      }
    assert' (subset keys validKeys) $ "Unknown keys in configuration: " 
      ++ (intercalate " " $ minus' keys validKeys)
  where
    get cfg k      = safe $ C.lookup cfg k          -- no default
    getOr cfg k v  = safe $ C.lookupDefault v cfg k -- default
    invalidKeys ks = show $ minus' ks validKeys
    validKeys = sort
      [ "keystore.path"
      , "repocache.path"
      , "server.port"
      , "server.logFile"
      , "server.logLevel"
      , "server.pidFile" ]

readCliOpts :: App ()
readCliOpts = 
  do
    opts <- parseOpts
    setConfigFilePath $ configFilePath opts
    setDaemonMode     $ enableDaemonMode opts
  where
    parseOpts = safe . O.execParser $ O.info (O.helper <*> cliOpts)
      ( O.fullDesc
     <> O.progDesc "Automatically push Github commits to a remote repository."
     <> O.header   "pure - push relay for Github repositories" )
    cliOpts = CliOpts
     <$> O.strOption
         ( O.long    "config-file"
        <> O.short   'c'
        <> O.value   "/etc/pure.conf"
        <> O.metavar "FILE"
        <> O.help    "Path to pure.conf" )
     <*> O.switch
         ( O.long  "daemon"
        <> O.short 'd'
        <> O.help  "Run as a background process (daemon mode)" )

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
     safe $ Warp.run (listenPort c) (app c)

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

main :: IO ()
main = runApp appMain initState >>= \(_, r) ->
  case r of Ok _  -> return ()
            Err e -> putStrLn $ "Unrecoverable error: " ++ e
  where initState = AppState NoRequest appConfig

