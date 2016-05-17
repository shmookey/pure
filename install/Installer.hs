import Prelude hiding (fail)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Options.Applicative as O
import Options.Applicative ((<>))

import qualified Rel.Cmd as Cmd
import qualified Rel.FS as FS
import qualified Rel.Log as Log
import qualified Rel.User as User
import Monad.Result
import InstallerMonad


data Options = Options
  { optBinDir      :: FilePath
  , optShareDir    :: FilePath
  , optConfDir     :: FilePath
  , optRepoDir     :: FilePath
  , optKeyDir      :: FilePath
  , optRootDir     :: FilePath
  , optServiceUser :: String
  , optSkipCopy    :: Bool
  , optSkipSetup   :: Bool
  }

data InitSystem = 
    Systemd 
  | SysVInit 
  | Upstart 
  | Launchd 
  | OtherInit

install :: Installer ()
install = readCliOpts >>= \options ->
  let root      = optRootDir options
      binDir    = root ++ optBinDir   options
      shareDir  = root ++ optShareDir options
      confDir   = root ++ optConfDir  options
      repoDir   = root ++ optRepoDir  options
      keyDir    = root ++ optKeyDir   options
      user      = optServiceUser options
      skipCopy  = optSkipCopy    options
      skipSetup = optSkipSetup   options
  in do
    uid <- User.getUID
    if not (uid == 0)
    then Log.warning "Not running as root. Setup may require elevated privileges."
    else return ()

    if not skipCopy
    then do
      currentDir <- FS.cwd
      mainBinary <- Cmd.run "stack" ["exec", "which", "pure"]
      sshCompat  <- return $ currentDir ++ "/util/git-ssh-compat"
      initSystem <- guessInitSystem

      assert' (List.isPrefixOf currentDir mainBinary)
        "Couldn't find `pure` executable. Have you run `stack build`?"
      ensureDirectory binDir
      ensureDirectory shareDir
      ensureDirectory confDir
      ensureDirectory repoDir
      ensureDirectory keyDir
      FS.copy mainBinary $ binDir   ++ "/pure"
      FS.copy sshCompat  $ shareDir ++ "/git-ssh-compat"

      case initSystem of 
        Systemd   -> systemdInstallUnit "pure.service"
                  >> systemdInstallUnit "pure.socket"
        Launchd   -> Log.info "Init config not available for launchd!" 
        SysVInit  -> Log.info "Init config not available for sysvinit!" 
        Upstart   -> Log.info "Init config not available for upstart!" 
        OtherInit -> Log.info "Unable to determine init system."
 
    else return ()
    
    if not skipSetup
    then do
      ensure (User.exists user) (User.createSystemUser user) $
        "Failed to create service user: " ++ user
    else return ()

systemdInstallUnit :: FilePath -> Installer ()
systemdInstallUnit x =
  FS.copy from to >> (Log.info $ "Installed systemd unit: " ++ x)
  where from = "service/" ++ x
        to   = "/etc/systemd/system/" ++ x

ensureDirectory :: FilePath -> Installer ()
ensureDirectory dir = 
  ensure (FS.isDirectory dir)
         (FS.createDirectory' dir)
         ("Failed to create directory: " ++ dir)

guessInitSystem :: Installer InitSystem
guessInitSystem = 
  do
    hasSystemd  <- FS.isDirectory "/usr/lib/systemd"
    hasUpstart  <- FS.isDirectory "/usr/share/upstart"
    hasSysVInit <- FS.isDirectory "/etc/init.d"
    hasLaunchd  <- FS.isFile "/sbin/launchd"
    return $
      if hasSystemd then Systemd
      else if hasLaunchd then Launchd
      else if hasUpstart then Upstart
      else if hasSysVInit then SysVInit
      else OtherInit

main :: IO ()
main = runInstaller install defaultSettings >>= \(_, r) ->
  case r of Ok _  -> return ()
            Err e -> putStrLn $ "Unrecoverable error: " ++ e

defaultSettings = State
  { fsConfig   = FS.Config {}
  , userConfig = User.Config {}
  , cmdConfig  = Cmd.Config
    { Cmd.envVars = Map.fromList [("PATH", "/usr/local/bin:/usr/bin:/bin")]
    }
  , logConfig  = Log.Config
    { Log.path    = Nothing
    , Log.level   = Log.INFO
    , Log.print   = True
    }
  }
    
readCliOpts :: Installer Options
readCliOpts =
  safe . O.execParser $ O.info (O.helper <*> cliOpts)
    ( O.fullDesc
   <> O.progDesc "Install and configure the pure daemon."
   <> O.header   "pure-install - installer for pure, push relay for Github repositories" )
  where 
    cliOpts = Options
      <$> O.strOption
          ( O.long    "bin-dir"
         <> O.value   "usr/bin"
         <> O.metavar "DIRECTORY"
         <> O.help    "Directory to store `pure` executable." )
      <*> O.strOption
          ( O.long    "share-dir"
         <> O.value   "usr/share"
         <> O.metavar "DIRECTORY"
         <> O.help    "Directory to store shared files (as in /usr/share)." )
      <*> O.strOption
          ( O.long    "conf-dir"
         <> O.value   "etc"
         <> O.metavar "DIRECTORY"
         <> O.help    "Directory to store `pure.conf`." )
      <*> O.strOption
          ( O.long    "repo-dir"
         <> O.value   "var/run/pure/repos"
         <> O.metavar "DIRECTORY"
         <> O.help    "Directory to store cached repositories." )
      <*> O.strOption
          ( O.long    "key-dir"
         <> O.value   "var/run/pure/keys"
         <> O.metavar "DIRECTORY"
         <> O.help    "Directory to store managed keystore." )
      <*> O.strOption
          ( O.long    "root-dir"
         <> O.value   "/"
         <> O.metavar "DIRECTORY"
         <> O.help    "Override root directory (e.g. for packaging)" )
      <*> O.strOption
          ( O.long    "service-user"
         <> O.value   "pure"
         <> O.metavar "NAME"
         <> O.help    "Name to use for service user." )
      <*> O.switch
          ( O.long    "skip-copy"
         <> O.help    "Skip installing files (setup only)" )
      <*> O.switch
          ( O.long    "skip-setup"
         <> O.help    "Skip post-install setup (install only)" )

