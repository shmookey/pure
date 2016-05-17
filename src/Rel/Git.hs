{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rel.Git
  ( Config(..)
  , Git(runGit)
  , Protocol(Local, Ssh, Https, GitP)
  , Ref
  , Remote(RemoteAlias, RemoteUrl)
  , Repository(BareRepository, WorkingRepository)
  , RepoUrl(RepoUrl)
  , clone
  , fetch
  , formatRef
  , formatRepoUrl
  , getIdentity
  , isBare
  , isRepo
  , getRoot
  , getRemotes
  , mirror
  , open
  , origin
  , parseProtocol
  , parseRef
  , parseSshUrl
  , push
  , setIdentity
  , usingIdentity
  , withConfig
  ) where

import Prelude hiding (fail)
import Data.List (elemIndex, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (maybe)

import qualified Rel.Cmd as Cmd
import qualified Rel.FS as FS
import qualified Rel.Log as Log
import qualified Rel.User as User
import Monad.Result
import Monad.RelMonad


-- ---------------------------------------------------------------------
-- Git monad

data Config = Config
  { identity  :: Maybe FilePath
  , sshClient :: FilePath
  }

data Git a = Git { runGit :: Config -> IO (Config, Result a) }

instance Functor Git where
  fmap f ma = Git $ \st -> fmap2 (fmap f) $ runGit ma st

instance Applicative Git where
  pure x   = Git $ \st -> return (st, return x)
  ff <*> g = Git $ \st -> do 
    (st',  rf) <- runGit ff st
    (st'', rx) <- runGit g st'
    return (st'', rf <*> rx)

instance Monad Git where
  return x = pure x
  ma >>= f = Git $ \st -> do
    (st', rx) <- runGit ma st
    runGit (unwrap $ fmap f rx) st'

instance ResultantMonad Git where
  point x        = Git $ \st -> fmap ((,) st) x
  mapResult f ma = Git $ \st -> do
    (st', rx) <- runGit ma st
    return (st', f rx)

type Rel m = 
  ( ResultR Git       m
  , ResultR Log.Log   m
  , ResultR Cmd.Cmd   m
  , ResultR FS.FS     m
  , ResultR User.User m
  )


-- ---------------------------------------------------------------------
-- Git-specific data types

data Repository = 
    BareRepository FilePath
  | WorkingRepository FilePath

data Protocol = Local | Ssh | Https | GitP deriving (Show)

data RepoUrl = RepoUrl
  { protocol    :: Protocol
  , hostname    :: String
  , path        :: FilePath
  , user        :: Maybe String
  } deriving (Show)

data Remote =
    RemoteAlias String
  | RemoteUrl String

instance Show Remote where
  show (RemoteAlias x) = x
  show (RemoteUrl x)   = x

data Ref =
    LocalBranch  String
  | LocalTag     String
  | RemoteBranch String String -- RemoteBranch "origin" "master"
  | OtherRef     String

-- ---------------------------------------------------------------------
-- State monad operations

getIdentity :: Rel m => m (Maybe FilePath)
getIdentity = withConfig $ return . identity

setIdentity :: Rel m => Maybe FilePath -> m ()
setIdentity x = updateConfig (\c -> c { identity = x })

withConfig :: Rel m => (Config -> Git a) -> m a
withConfig f = rPoint . Git $ \c -> runGit (f c) c

getConfig :: Rel m => m Config
getConfig = rPoint . Git $ \c -> return (c, return c)

updateConfig :: Rel m => (Config -> Config) -> m ()
updateConfig f = rPoint . Git $ \c -> return (f c, return ())

usingIdentity :: Rel m => FilePath -> m a -> m a
usingIdentity x task = bracket' task before after
  where before   = do { old <- getIdentity ; setIdentity (Just x) ; return old }
        after xs = setIdentity xs

-- ---------------------------------------------------------------------
-- Local repository operations

-- | Check if a directory is within a bare Git repository
isBare :: Rel m => FilePath -> m Bool
isBare dir =
  f `fmap` (Cmd.runIn "git" ["rev-parse", "--is-bare-repository"] dir)
  where f "true" = True
        f _      = False

-- | Check if a directory is within a Git repository
isRepo :: Rel m => FilePath -> m Bool
isRepo dir =
  let checkDir = Cmd.runIn "git" ["rev-parse"] dir
  in recover (const False) (const True `fmap` checkDir)

-- | Get the `.git` directory for the repository (same as root for bare repos)
getGitDir :: Rel m => Repository -> m FilePath
getGitDir (BareRepository x)    = return $ x
getGitDir (WorkingRepository x) = return $ x ++ "/.git"

-- | Get a list of the configured remotes
getRemotes :: Rel m => Repository -> m [String]
getRemotes repo =
  git repo ["remote", "-v"] >>= \xs -> return (lines xs)

-- | Get the root directory of the repo (different for bare vs. non-bare repos!)
getRoot :: Rel m => Repository -> m FilePath
getRoot (BareRepository x)    = return x
getRoot (WorkingRepository x) = return x

-- | Get a Repository reference for a given path
open :: Rel m => FilePath -> m Repository
open dir =
  isBare dir >>= \x -> return $
    case x of True  -> BareRepository dir
              False -> WorkingRepository dir

-- ---------------------------------------------------------------------
-- Remote repository operations

-- | Clone a repository to a new directory
clone :: Rel m => RepoUrl -> FilePath -> m Repository
clone url dir =
  do
    FS.createDirectory' dir
    git' repo ["clone", formatRepoUrl url, "."]
    return repo
  where
    repo = WorkingRepository dir
  
-- | Clone a bare mirror repository to a new directory
mirror :: Rel m => RepoUrl -> FilePath -> m Repository
mirror url dir =
  do
    FS.createDirectory' dir
    git' repo ["clone", "--mirror", formatRepoUrl url, "."]
    return repo
  where
    repo = BareRepository dir
  
-- | Fetch updates from a remote.
fetch :: Rel m => Repository -> Remote -> m ()
fetch repo remote =
  git' repo ["fetch", show remote]

-- | Push updates from a local ref to a remote one.
push :: Rel m => Repository -> Remote -> Ref -> Ref -> m ()
push repo remote srcRef dstRef =
  git' repo ["push", show remote, formatRef srcRef ++ ":" ++ formatRef dstRef]

-- ---------------------------------------------------------------------
-- Utility functions

-- | Run a git command.
git :: Rel m => Repository -> [String] -> m String
git repo args = getConfig >>= \config ->
  let
    key    = maybe "" ("-i " ++) (identity config)
    client = sshClient config
    opts   = "-oBatchMode=yes -oStrictHostKeyChecking=no" -- TODO: fix me!!
    cmd    = intercalate " " ["ssh", opts, key]
    env    = [ ( "GIT_TERMINAL_PROMPT", "0"    )
             , ( "GIT_SSH_COMMAND"    , cmd    )
             , ( "GIT_SSH"            , client ) ]
  in getRoot repo >>= Cmd.usingEnv env . Cmd.runIn "git" args

-- | Run a git command, discarding the output.
git' :: Rel m => Repository -> [String] -> m ()
git' repo args = git repo args >> return ()

-- | Alias for the "origin" remote
origin :: Remote
origin = RemoteAlias "origin"

-- | Parse a ref string
parseRef :: String -> Ref
parseRef x = case splitOn "/" x of
  ["refs", "heads",   name]         -> LocalBranch name
  ["refs", "tags",    name]         -> LocalTag name
  ["refs", "remotes", remote, name] -> RemoteBranch remote name
  _                                 -> OtherRef x

parseProtocol :: String -> Result Protocol
parseProtocol x = case x of
 "file"  -> Ok Local
 "ssh"   -> Ok Ssh  
 "https" -> Ok Https
 "git"   -> Ok GitP
 _       -> Err $ "Unrecognised protocol: " ++ x

parseSshUrl :: String -> Result RepoUrl
parseSshUrl x =
  do 
    (hostname, path) <- parseRemainder remainder
    return $ RepoUrl { protocol = Ssh, hostname, path, user }
  where
    (user, remainder) = 
      case splitOn "@" x of u:r:[] -> (Just u, r)
                            _      -> (Nothing, x)
    parseRemainder url = 
      case elemIndex ':' url of
        Nothing -> Err "Missing ':' in SSH URL"
        Just n  -> let host = take n url
                       path = drop (n+1) url
                   in return (host, path)

formatRepoUrl :: RepoUrl -> String
formatRepoUrl x =
  concat
    [ (formatProtocol $ protocol x) ++ "://"
    , formatUser $ user x
    , hostname x
    , "/" ++ (path x)
    ]
  where 
    formatUser (Just user) = user ++ "@"
    formatUser Nothing     = ""

formatProtocol :: Protocol -> String
formatProtocol Local = "file"
formatProtocol Ssh   = "ssh"
formatProtocol Https = "https"
formatProtocol GitP  = "git"

formatRef :: Ref -> String
formatRef (LocalBranch x)    = "refs/heads/" ++ x
formatRef (LocalTag x)       = "refs/tags/" ++ x
formatRef (RemoteBranch r x) = "refs/remotes/" ++ r ++ "/" ++ x
formatRef (OtherRef x)       = x

