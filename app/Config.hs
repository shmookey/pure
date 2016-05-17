{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Config where

import Prelude hiding (fail, lookup)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Options.Applicative as O
import Options.Applicative ((<>))
import Data.List (intercalate, sort)
import Data.List.Ordered (subset, minus')

import qualified Rel.Cmd as Cmd
import qualified Rel.FS as FS
import qualified Rel.Git as Git
import qualified Rel.Github as Github
import qualified Rel.Log as Log
import qualified Rel.User as User
import Monad.Result
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
    { Git.identity  = Nothing
    , Git.sshClient = "/usr/share/pure/git-ssh-compat"
    }
  , logConfig    = Log.Config
    { Log.path      = Nothing
    , Log.level     = Log.INFO
    , Log.print     = True
    }
  , fsConfig     = FS.defaultConfig
  , githubConfig = Github.defaultConfig
  , userConfig   = User.defaultConfig
  }

loadConfig :: App ()
loadConfig = 
  do
    cfgPath      <- getConfigPath
    current      <- getConfig
    c            <- safe $ C.load [ C.Optional cfgPath ]
    keys         <- safe $ (sort . map T.unpack . HM.keys) `fmap` C.getMap c
    keystorePath <- getOr c "keystore.path"    $ keystorePath current 
    storageRoot  <- getOr c "repocache.path"   $ storageRoot current 
    listenPort   <- getOr c "server.port"      $ listenPort current 
    pidFile      <- getOr c "server.pidFile"   $ pidFile current 
    logLevel     <- getOr c "server.logLevel"  . Log.level     $ logConfig current
    gitSsh       <- getOr c "server.sshClient" . Git.sshClient $ gitConfig current 
    logPath      <- get   c "server.logFile"
    setConfig $ current 
      { storageRoot, keystorePath, pidFile, listenPort
      , logConfig = (logConfig current) 
        { Log.path      = logPath
        , Log.level     = logLevel }
      , gitConfig = (gitConfig current)
        { Git.sshClient = gitSsh }
      }
    assert' (subset keys validKeys) $ "Unknown keys in configuration: " 
      ++ (intercalate " " $ minus' keys validKeys)
  where
    get cfg k      = safe $ C.lookup cfg k          -- no default
    getOr cfg k v  = safe $ C.lookupDefault v cfg k -- default
    validKeys = sort
      [ "keystore.path"
      , "repocache.path"
      , "server.port"
      , "server.logFile"
      , "server.logLevel"
      , "server.pidFile"
      , "server.sshClient"
      ]

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

