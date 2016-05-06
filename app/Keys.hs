{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Keys where

import Prelude hiding (fail, readFile)
import GHC.Generics
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad (replicateM)
import System.IO.Strict (readFile)
import System.Random (randomRIO)
import Data.Digest.Pure.SHA (hmacSha1, showDigest)

import qualified Rel.Cmd as Cmd
import qualified Rel.Log as Log
import Util.Request (Request)
import Monad.Result
import AppMonad


data KeySet = KeySet
  { privKey :: FilePath
  , pubKey  :: String
  , name    :: String
  , secret  :: String
  } deriving (Show, Generic)

instance ToJSON KeySet

request :: Request -> App ()
request _ =
  do
    key <- create
    _   <- Log.debug $ "Created keyset: " ++ (show key)
    ok key

create :: App KeySet
create =
  do
    name        <- randomName
    secret      <- randomSecret
    privKey     <- (++ "/" ++ name) `fmap` getKeystorePath
    _           <- Cmd.run "ssh-keygen" ["-t", "rsa", "-f", privKey, "-q", "-N", "", "-C", secret]
    (pubKey, _) <- readPubKey $ privKey ++ ".pub" 
    return $ KeySet { privKey, pubKey, name, secret }

load :: String -> App KeySet
load name =
  do
    privKey          <- (++ "/" ++ name) `fmap` getKeystorePath
    (pubKey, secret) <- readPubKey $ privKey ++ ".pub"
    return $ KeySet { privKey, pubKey, name, secret }

validSig :: String -> KeySet -> BSL.ByteString -> App Bool
validSig sig key text =
  return $ sig == "sha1=" ++ expected
  where expected = showDigest $ hmacSha1 secret' text
        secret'  = BSL.pack $ secret key

getSecret :: String -> App String
getSecret keyName = snd `fmap` readPubKey keyName

-- | Read a public key, returning the key part separately from the comment
readPubKey :: FilePath -> App (String, String)
readPubKey path = (safe $ readFile path) >>= parts . words
  where parts ["ssh-rsa", k, desc] = return ("ssh-rsa " ++ k, desc)
        parts _                    = fail $ "Parse error in pubkey: " ++ path

pick :: [a] -> IO a
pick xs = (xs !!) `fmap` randomRIO (0, length xs - 1)

pickN :: Int -> [a] -> IO [a]
pickN n = replicateM n . pick


randomName :: App String
randomName = safe $ pickN 24 nameChars

randomSecret :: App String
randomSecret = safe $ pickN 64 hmacChars

nameChars :: [Char]
nameChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"

hmacChars :: [Char]
hmacChars = nameChars ++ "`~!@#$%^&*()-_=+[{]}|;:'<,>.?/"

