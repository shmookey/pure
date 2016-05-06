module Push (request) where

import Prelude hiding (fail)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.List.Split (splitOn)

import qualified Rel.Log as Log
import qualified Rel.Git as Git
import qualified Rel.Github as Github
import Monad.Result
import Util.Request (Request(Request))
import AppMonad
import qualified Keys


request :: Request -> App ()
request (Request path query body headers) =
  do
    event   <- Github.decodeRequest body
    source  <- cloneSource event
    target  <- parsePath path
    storage <- localStorage event
    ref     <- return $ Github.ref event
    keyId   <- getOrFail "key" query "Key required in query"
    sig     <- getOrFail "X-Hub-Signature" headers "No signature in headers!"

    Log.debug $ "Source:    " ++ (Git.formatRepoUrl source)
    Log.debug $ "Target:    " ++ (Git.formatRepoUrl target)
    Log.debug $ "Ref:       " ++ (Git.formatRef ref)
    Log.debug $ "Identity:  " ++ keyId
    Log.debug $ "Signature: " ++ sig
    
    key <- Keys.load keyId
    mandatory (Keys.validSig sig key body) $ "Incorrect signature: " ++ sig

    ok "OK"
    
    fork . Git.usingIdentity (Keys.privKey key) $ do
      cached <- Git.isRepo storage
      repo   <- case cached of
        True  -> do Log.debug $ "Updating cached repository " ++ storage
                    repo <- Git.open storage
                    Git.fetch repo Git.origin
                    Log.debug $ "Fetch complete for " ++ storage
                    return repo
        False -> do Log.debug $ "Cloning into " ++ storage
                    repo <- Git.mirror source storage
                    Log.debug $ "Clone complete for " ++ storage
                    return repo
      
      Log.debug $ "Pushing " ++ (Git.formatRef ref) 
               ++ " to "     ++ (Git.formatRepoUrl target)

      Git.push repo (Git.RemoteUrl $ Git.formatRepoUrl target) ref ref
      Log.info $ "Finished processing push for " ++ (Git.formatRepoUrl source)
  where
    cloneSource     = fromResult . Git.parseSshUrl . Github.sshUrl . Github.repository
    localStorage ev = let url = drop 7 . Github.url $ Github.repository ev
                      in (++ url) `fmap` getStorageRoot
    parsePath []    = fail "Path must be of form /user@host/a/b/c"
    parsePath (f:r) = 
      let rPath = intercalate "/" r
      in case splitOn "@" f of
        user : host : [] -> return $ Git.RepoUrl Git.Ssh host rPath (Just user)
        _                -> fail "Destination must start with user@host"

    getOrFail k kvs e = case Map.lookup k kvs of
      Just v  -> return v
      Nothing -> fail e

