{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rel.Log
    ( Log(runLog)
    , Config(Config, path, level, print)
    , Level(ERROR, WARN, INFO, DEBUG, TRACE)
    , error, warning, info, debug, trace
    , parseLevel
    )
  where

import Prelude hiding (log, error, print)
import Data.List (intercalate)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

import Monad.Result
import Monad.RelMonad


data Level = ERROR | WARN | INFO | DEBUG | TRACE
  deriving (Eq, Ord, Show)

data Config = Config
  { path  :: Maybe FilePath
  , level :: Level
  , print :: Bool
  }

data Log a = Log { runLog :: Config -> IO (Result a) }

instance Functor Log where
  fmap f ma = Log $ \c -> fmap f `fmap` runLog ma c

instance Applicative Log where
  pure x  = Log $ \_ -> return $ pure x
  f <*> g = Log $ \c -> runLog f c `mapAp` runLog g c

instance Monad Log where
  ma >>= f = Log $ \c -> runLog ma c >>= flatten . fmap (flip runLog c . f)
  return x = Log $ \_ -> return $ pure x

instance ResultantMonad Log where
  point x        = Log $ \_ -> x
  mapResult f ma = Log $ \c -> f `fmap` runLog ma c

-- Log ops

getConfig :: ResultR Log m => m Config
getConfig = rPoint . Log $ return . return

getLogFile :: ResultR Log m => m (Maybe FilePath)
getLogFile = path `fmap` getConfig

getLevel :: ResultR Log m => m Level
getLevel = level `fmap` getConfig

getPrint :: ResultR Log m => m Bool
getPrint = print `fmap` getConfig

message :: ResultR Log m => String -> m ()
message msg =
  do 
    file    <- getLogFile
    doPrint <- getPrint
    time    <- safe getZonedTime
    line    <- formatMsg time
    if doPrint then safe $ putStr line else return ()
    case file of Just x  -> safe $ appendFile x line
                 Nothing -> return ()
  where
    formatMsg t = return $ intercalate " " [dispTime t, msg] ++ "\n"
    dispTime    = sqbr . formatTime defaultTimeLocale "%FT%T%z"

debug :: ResultR Log m => String -> m ()
debug msg = log DEBUG msg

error :: ResultR Log m => String -> m ()
error msg = log ERROR msg

info :: ResultR Log m => String -> m ()
info msg = log INFO msg

trace :: ResultR Log m => String -> m ()
trace msg = log TRACE msg

warning :: ResultR Log m => String -> m ()
warning msg = log WARN msg

-- Utility functions

log :: ResultR Log m => Level -> String -> m ()
log l msg = inLevel l >>= \r ->
  case r of True ->  message $ (sqbr $ padShow l) ++ " " ++ msg
            False -> return ()

inLevel :: ResultR Log m => Level -> m Bool
inLevel x = getLevel >>= return . (>= x)

-- Surround in square brackets
sqbr :: String -> String
sqbr x = "[" ++ x ++ "]"

parseLevel :: String -> Result Level
parseLevel "error" = Ok ERROR
parseLevel "warn"  = Ok WARN
parseLevel "info"  = Ok INFO
parseLevel "debug" = Ok DEBUG
parseLevel "trace" = Ok TRACE
parseLevel x       = Err $ "Invalid log level: " ++ x

padShow :: Level -> String
padShow WARN  = " WARN"
padShow INFO  = " INFO"
padShow x     = show x
