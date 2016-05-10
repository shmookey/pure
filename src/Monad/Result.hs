{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Monad.Result where

import Prelude hiding (fail)
import Control.Exception (SomeException, try)

import Monad.RelMonad


data Result a = Ok a | Err String

instance Functor Result where
  fmap f (Ok x)  = Ok $ f x
  fmap _ (Err e) = Err e

instance Applicative Result where
  pure x      = Ok x
  Ok f <*> g  = fmap f g
  Err e <*> _ = Err e

instance Monad Result where
  Ok x  >>= f = f x
  Err e >>= _ = Err e
  return x    = Ok x

-- Constraint alias for relative monadic functions
type ResultR r m = (RelMonad r m, ResultantMonad m)

class Monad m => ResultantMonad m where
  point     :: IO (Result a) -> m a
  mapResult :: (Result a -> Result b) -> m a -> m b

  -- | Extend an operation with a before and after step. `after` is always run.
  bracket :: m a -> m before -> m after -> m a
  bracket ma before after = 
    do
      _ <- before 
      r <- mapResult Ok ma
      _ <- after
      fromResult r 

  -- | Extend an operation with a before and after step. `after` is always run.
  bracket' :: m a -> m before -> (before -> m after) -> m a
  bracket' ma before after = 
    do
      x <- before
      r <- mapResult Ok ma
      _ <- after x
      fromResult r 

  -- Lift an IO value. Use `safe` for an exception-handling alternative.
  liftIO :: IO a -> m a
  liftIO = point . fmap return
  
  -- Bring an `Either` into the monad, mapping `Right` to `Ok`
  fromEither :: Show e => Either e a -> m a
  fromEither (Right x) = return x
  fromEither (Left e)  = fail $ show e

  -- Bring a Result into the monad
  fromResult :: Result a -> m a
  fromResult = point . return

  fromMaybe :: Maybe a -> String -> m a
  fromMaybe (Just x) _ = return x
  fromMaybe Nothing e  = fail e

  fail :: String -> m a
  fail = fromResult . Err

  assert :: m Bool -> String -> m ()
  assert x msg = x >>= \r -> if r then return () else fail msg

  assert' :: Bool -> String -> m ()
  assert' True _ = return ()
  assert' _ msg  = fail msg

  prohibit :: m Bool -> String -> m ()
  prohibit x msg = x >>= \r -> if r then fail msg else return ()

  -- Safely lift an IO value, converting exceptions to `Err`s.
  safe :: IO a -> m a
  safe x =
    liftIO (try x :: IO (Either SomeException _)) >>= fromEither

  -- Wrap an operation to recover from an error
  recover :: (String -> a) -> m a -> m a
  recover f ma =
    mapResult f' ma
    where f' (Err e) = Ok $ f e
          f' x       = x

  recover' :: (String -> m a) -> m a -> m a
  recover' f ma = 
    do r <- mapResult Ok ma
       case r of Ok x  -> return x
                 Err e -> f e

  unwrap :: Result (m a) -> m a
  unwrap (Ok x)  = x
  unwrap (Err e) = fail e

mapAp :: (Monad m, Applicative r) => m (r (a -> b)) -> m (r a) -> m (r b)
mapAp mf mx =
  do f <- mf ; x <- mx
     return $ f <*> x

flatten :: Applicative m => Result (m (Result a)) -> m (Result a)
flatten (Ok x)  = x
flatten (Err e) = pure $ Err e

integrate :: (c -> c') -> IO (Result (c, a)) -> IO (Result (c', a))
integrate f =
  fmap $ fmap1 f

fmap1 :: Functor f => (a -> c) -> f (a, b) -> f (c, b)
fmap1 f = fmap $ \(x, y) -> (f x, y)

fmap2 :: Functor f => (b -> c) -> f (a, b) -> f (a, c)
fmap2 f = fmap $ \(x, y) -> (x, f y)

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap fn = fmap (fmap fn)

toMaybe :: Result a -> Maybe a
toMaybe (Ok x) = Just x
toMaybe _      = Nothing
  
