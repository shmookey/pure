{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rel.Cmd 
  ( Config(..)
  , Cmd(runCmd)
  , command
  , getEnvVars
  , setEnvVars
  , run
  , runIn
  , setConfig
  , setEnv
  , unsetEnv
  , updateConfig
  , usingEnv
  , withConfig
  ) where

import Prelude hiding (fail)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

import System.IO.Strict (hGetContents)
import qualified System.Process as Proc
import System.Exit (ExitCode(..))

import qualified Rel.Log as Log
import Monad.Result
import Monad.RelMonad

-- Cmd monad

data Config = Config { envVars :: Map.Map String String }

data Cmd a = Cmd { runCmd :: Config -> IO (Config, Result a) }

instance Functor Cmd where
  fmap f ma = Cmd $ \st -> fmap2 (fmap f) $ runCmd ma st

instance Applicative Cmd where
  pure x   = Cmd $ \st -> return (st, return x)
  ff <*> g = Cmd $ \st -> do 
    (st',  rf) <- runCmd ff st
    (st'', rx) <- runCmd g st'
    return (st'', rf <*> rx)

instance Monad Cmd where
  return x = pure x
  ma >>= f = Cmd $ \st -> do
    (st', rx) <- runCmd ma st
    runCmd (unwrap $ fmap f rx) st'

instance ResultantMonad Cmd where
  point x        = Cmd $ \st -> fmap ((,) st) x
  mapResult f ma = Cmd $ \st -> do
    (st', rx) <- runCmd ma st
    return (st', f rx)

type Rel m = (ResultR Cmd m, ResultR Log.Log m)


setEnv :: Rel m => String -> String -> m ()
setEnv k v = updateConfig $ \c -> 
  c { envVars = Map.insert k v (envVars c) }

unsetEnv :: Rel m => String -> m ()
unsetEnv k = updateConfig $ \c -> 
  c { envVars = Map.delete k (envVars c) }

-- | Perform an action with some environment variables set
usingEnv :: Rel m => [(String, String)] -> m a -> m a
usingEnv vars task = bracket' task before after
  where before   = do { xs <- getEnvVars ; for_ vars $ uncurry setEnv ; return xs }
        after xs = setEnvVars xs

setConfig :: Rel m => Config -> m ()
setConfig c = rPoint . Cmd $ \_ -> return (c, return ())

getEnvVars :: Rel m => m (Map.Map String String)
getEnvVars = withConfig $ return . envVars

setEnvVars :: Rel m => Map.Map String String -> m ()
setEnvVars x = updateConfig (\c -> c { envVars = x })

withConfig :: Rel m => (Config -> Cmd a) -> m a
withConfig f = rPoint . Cmd $ \c -> runCmd (f c) c

updateConfig :: Rel m => (Config -> Config) -> m ()
updateConfig f = rPoint . Cmd $ \c -> return (f c, return ())

run :: Rel m => String -> [String] -> m String
run cmd args =
  command (Proc.RawCommand cmd args) "/"

runIn :: Rel m => String -> [String] -> FilePath -> m String
runIn cmd args dir =
  command (Proc.RawCommand cmd args) dir

command :: Rel m => Proc.CmdSpec -> FilePath -> m String
command cmd dir =
  do
    logCommand cmd dir
    env          <- Map.toList `fmap` getEnvVars
    (hR, hW)     <- safe $ Proc.createPipe
    (_,_,_,proc) <- safe $ runCommand env hW
    retVal       <- safe $ Proc.waitForProcess proc
    output       <- safe $ hGetContents hR
    case retVal of 
      ExitSuccess   -> return output
      ExitFailure n -> let msg = errorMessage n output
                       in do Log.debug $ "Error: " ++ msg
                             fail msg
  where
    runCommand env handle = Proc.createProcess $ Proc.CreateProcess
      { Proc.cmdspec            = cmd
      , Proc.cwd                = Just dir
      , Proc.env                = Just env
      , Proc.std_in             = Proc.CreatePipe
      , Proc.std_out            = Proc.UseHandle handle
      , Proc.std_err            = Proc.UseHandle handle
      , Proc.close_fds          = True
      , Proc.create_group       = False
      , Proc.delegate_ctlc      = False
      , Proc.detach_console     = False
      , Proc.create_new_console = False
      , Proc.new_session        = False
      , Proc.child_group        = Nothing
      , Proc.child_user         = Nothing
      }
    errorMessage n output = concat
      ["Command `", formatCmdSpec cmd, "`  returned ", show n, "\nOutput: ", output]

logCommand :: Rel m => Proc.CmdSpec -> FilePath -> m ()
logCommand cmd dir =
  Log.trace $ (formatCmdSpec cmd) ++ "  [" ++ dir ++ "] "

formatCmdSpec :: Proc.CmdSpec -> String
formatCmdSpec (Proc.ShellCommand cmd) = "ShellCommand " ++ cmd
formatCmdSpec (Proc.RawCommand exe args) =
    intercalate " " $ "ShellCommand" : exe : args'
    where escape  = intercalate "\\'" . splitOn "'"
          quote x = "'" ++ x ++ "'"
          args'   = map (quote . escape) args

