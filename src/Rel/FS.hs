{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rel.FS 
  ( Config(Config)
  , FS(runFS)
  , copy
  , copyDirectory
  , copyDirectoryContents
  , isFile
  , remove
  , mkdir
  , mkdirs
  , removeDirectory
  ) where

import Prelude hiding (fail)
import qualified System.Directory as Directory

import qualified Rel.Log as Log
import qualified Rel.Cmd as Cmd
import Monad.Result

-- FS monad

data Config = Config {}

data FS a = FS { runFS :: Config -> IO (Result a) }

instance Functor FS where
  fmap f ma = FS $ \c -> fmap f `fmap` runFS ma c

instance Applicative FS where
  pure x  = FS $ \_ -> return $ pure x
  f <*> g = FS $ \c -> runFS f c `mapAp` runFS g c

instance Monad FS where
  ma >>= f = FS $ \c -> runFS ma c >>= flatten . fmap (flip runFS c . f)
  return x = FS $ \_ -> return $ pure x

instance ResultantMonad FS where
  point x        = FS $ \_ -> x
  mapResult f ma = FS $ \c -> f `fmap` runFS ma c

type Rel m = (ResultR FS m, ResultR Log.Log m, ResultR Cmd.Cmd m)

-- File ops

copy :: Rel m => FilePath -> FilePath -> m ()
copy from to =
  Cmd.run "cp" [from, to] >> return ()

copyDirectory :: Rel m => FilePath -> FilePath -> m ()
copyDirectory from to =
  Cmd.run "cp" ["-r", from, to] >> return ()

copyDirectoryContents :: Rel m => FilePath -> FilePath -> m ()
copyDirectoryContents from to =
  Cmd.run "cp" ["-r", from ++ "/", to] >> return ()

isFile :: Rel m => FilePath -> m Bool
isFile = safe . Directory.doesFileExist 

mkdir :: Rel m => FilePath -> m ()
mkdir dir =
  Cmd.run "mkdir" [dir] >> return ()

mkdirs :: Rel m => FilePath -> m ()
mkdirs dir =
  Cmd.run "mkdir" ["-p", dir] >> return ()

remove :: Rel m => FilePath -> m ()
remove target =
  Cmd.run "rm" [target] >> return ()

removeDirectory :: Rel m => FilePath -> m ()
removeDirectory target =
  Cmd.run "rm" ["-r", target] >> return ()
