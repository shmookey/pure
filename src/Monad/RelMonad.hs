{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Monad.RelMonad where


class (Functor r, Monad m) => RelMonad r m where
  rPoint :: r a -> m a

instance Monad m => RelMonad m m where
  rPoint = id

