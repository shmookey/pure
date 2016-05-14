{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rel.User 
  ( Config(Config)
  , User(runUser)
  , createSystemUser
  , exists
  , getUID
  ) where

import Prelude hiding (fail)
import qualified System.Posix.User as PUser

import qualified Rel.Cmd as Cmd
import qualified Rel.Log as Log
import Monad.Result


data Config = Config {}

data User a = User { runUser :: Config -> IO (Result a) }

instance Functor User where
  fmap f ma = User $ \c -> fmap f `fmap` runUser ma c

instance Applicative User where
  pure x  = User $ \_ -> return $ pure x
  f <*> g = User $ \c -> runUser f c `mapAp` runUser g c

instance Monad User where
  ma >>= f = User $ \c -> runUser ma c >>= flatten . fmap (flip runUser c . f)
  return x = User $ \_ -> return $ pure x

instance ResultantMonad User where
  point x        = User $ \_ -> x
  mapResult f ma = User $ \c -> f `fmap` runUser ma c

type Rel m = (ResultR User m, ResultR Cmd.Cmd m, ResultR Log.Log m)


exists :: Rel m => String -> m Bool
exists x = recover (const False) $
  safe (PUser.getUserEntryForName x >> return True)

createSystemUser :: Rel m => String -> m ()
createSystemUser x = 
  Cmd.run "useradd" ["--system", x] >> return ()

-- | Get the effective UID of this process.
getUID :: Rel m => m Int
getUID = safe $ fromIntegral `fmap` PUser.getEffectiveUserID

