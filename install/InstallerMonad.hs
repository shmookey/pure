{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module InstallerMonad where

import Prelude hiding (fail)

import qualified Rel.Cmd as Cmd
import qualified Rel.FS as FS
import qualified Rel.Log as Log
import qualified Rel.User as User
import Monad.Result
import Monad.RelMonad
import Util.Request


-- ---------------------------------------------------------------------
-- State & configuration

data State = State
  { cmdConfig  :: Cmd.Config
  , fsConfig   :: FS.Config
  , logConfig  :: Log.Config
  , userConfig :: User.Config
  }


-- ---------------------------------------------------------------------
-- Installer monad

data Installer a = Installer { runInstaller :: State -> IO (State, Result a) }

instance Functor Installer where
  fmap f ma = Installer $ \st -> fmap2 (fmap f) $ runInstaller ma st

instance Applicative Installer where
  pure x   = Installer $ \st -> return (st, return x)
  ff <*> g = Installer $ \st -> do 
    (st',  rf) <- runInstaller ff st
    (st'', rx) <- runInstaller g st'
    return (st'', rf <*> rx)

instance Monad Installer where
  return x = pure x
  ma >>= f = Installer $ \st -> do
    (st', rx) <- runInstaller ma st
    runInstaller (unwrap $ fmap f rx) st'

instance ResultantMonad Installer where
  point x        = Installer $ \st -> fmap ((,) st) x
  mapResult f ma = Installer $ \st -> do
    (st', rx) <- runInstaller ma st
    return (st', f rx)


-- ---------------------------------------------------------------------
-- Relative instances

instance RelMonad Log.Log Installer where
  rPoint x = Installer . statePass $ Log.runLog x . logConfig

instance RelMonad FS.FS Installer where
  rPoint x = Installer . statePass $ FS.runFS x . fsConfig

instance RelMonad User.User Installer where
  rPoint x = Installer . statePass $ User.runUser x . userConfig

instance RelMonad Cmd.Cmd Installer where
  rPoint x = Installer $ \st -> Cmd.runCmd x (cmdConfig st) >>=
    \(c, rx) -> return (st { cmdConfig = c }, rx)


-- ---------------------------------------------------------------------
-- State and configuration utilities

getState :: Installer State
getState = Installer $ \st -> return (st, return st)

setState :: State -> Installer ()
setState st = Installer $ \_ -> return (st, return ())

updateState :: (State -> State) -> Installer ()
updateState f = getState >>= setState . f

statePass :: (c -> IO (Result a)) -> c -> IO (c, Result a)
statePass f c = (,) c `fmap` f c

