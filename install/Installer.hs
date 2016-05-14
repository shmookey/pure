import Prelude hiding (fail)

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
  , optConfDir     :: FilePath
  , optRepoDir     :: FilePath
  , optKeyDir      :: FilePath
  , optRootDir     :: FilePath
  , optServiceUser :: String
  , optSkipCopy    :: Bool
  , optSkipSetup   :: Bool
  }


install :: Installer ()
install = readCliOpts >>= \options ->
  let root      = optRootDir options
      binDir    = root ++ optBinDir      options
      confDir   = root ++ optConfDir     options
      repoDir   = root ++ optRepoDir     options
      keyDir    = root ++ optKeyDir      options
      user      = root ++ optServiceUser options
      skipCopy  = root ++ optSkipCopy    options
      skipSetup = root ++ optSkipSetup   options
  in do
    if not skipCopy
    then do
      assert (FS.isFile "pure")
        "pure executable must be in the current working directory."
      ensureDirectory binDir
      ensureDirectory confDir
      ensureDirectory repoDir
      ensureDirectory keyDir
      FS.copy "pure" (binDir ++ "/pure")
    else return ()
    
    if not skipSetup
    then do
      ensure (User.exists user) (User.createSystemUser user) $
        "Failed to create service user: " ++ user
    else return ()

    return ()

ensureDirectory :: FilePath -> Installer ()
ensureDirectory dir = 
  ensure (FS.isDirectory dir)
         (FS.createDirectory' dir)
         ("Failed to create directory: " ++ dir)

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

