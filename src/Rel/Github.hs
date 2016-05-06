{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel.Github where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text
import Data.Aeson (FromJSON(parseJSON), eitherDecode, withObject, withText, (.:))

import qualified Rel.Git as Git
import Monad.Result

-- Github monad

data Github a = Github { runGithub :: Config -> IO (Result a) }

data Config = Config {}

instance Functor Github where
  fmap f ma = Github $ \c -> fmap f `fmap` runGithub ma c

instance Applicative Github where
  pure x  = Github $ \_ -> return $ pure x
  f <*> g = Github $ \c -> runGithub f c `mapAp` runGithub g c

instance Monad Github where
  ma >>= f = Github $ \c -> runGithub ma c >>= flatten . fmap (flip runGithub c . f)
  return x = Github $ \_ -> return $ pure x

instance ResultantMonad Github where
  point x        = Github $ \_ -> x
  mapResult f ma = Github $ \c -> f `fmap` runGithub ma c

-- Webhook data types

data PushEvent = PushEvent
  { ref        :: Git.Ref
  , before     :: String
  , after      :: String
  , repository :: Repository
  }

data Repository = Repository
  { name     :: String
  , fullName :: String
  , url      :: String
  , cloneUrl :: String
  , gitUrl   :: String
  , sshUrl   :: String
  }

instance FromJSON PushEvent where
  parseJSON = withObject "PushEvent" $ \x ->
    PushEvent <$> x .: "ref"
              <*> x .: "before"
              <*> x .: "after"
              <*> x .: "repository"

instance FromJSON Repository where
  parseJSON = withObject "Repository" $ \x ->
    Repository <$> x .: "name"
               <*> x .: "full_name"
               <*> x .: "url"
               <*> x .: "clone_url"
               <*> x .: "git_url"
               <*> x .: "ssh_url"

instance FromJSON Git.Ref where
  parseJSON = withText "ref" (return . Git.parseRef . Text.unpack)

decodeRequest :: ResultR Github m => ByteString -> m PushEvent
decodeRequest = fromEither . eitherDecode

