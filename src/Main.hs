{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Git
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
import Filesystem.Path.CurrentOS
import Network (withSocketsDo)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Scotty

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Gittens v0.1"
    scotty 30090 app

app :: ScottyM ()
app = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/refs" $ do
        repo <- liftIO getRepo
        refs <- liftIO (listRefNames repo allRefsFlag)
        json refs

    get (regex "^/commits/([^/]+)/(.*)$") $ do
        limit   <- param "1"
        ref     <- param "2"
        commits <- liftIO (getCommits (toStrict ref) limit)
        json commits

------------------------------------------------------------------------

getRepo :: IO Repository
getRepo = openRepository (fromText "c:/development/ng/starfix-ng")

resolveRef' :: Repository -> Text -> IO (Maybe Oid)
resolveRef' repo ref = do
    xs <- sequence [
        resolveRef repo ref
      , resolveRef repo ("refs/heads/" <> ref)
      , resolveRef repo ("refs/tags/" <> ref)
      , resolveRef repo ("refs/remotes/" <> ref)
      ]
    return $ listToMaybe $ catMaybes xs

getCommits :: Text -> Int -> IO [Commit]
getCommits ref limit = do
    repo <- getRepo
    moid <- resolveRef' repo ref
    case moid of
      Nothing  -> error ("Cannot resolve: " ++ unpack ref)
      Just oid -> do
        mcommit <- lookupCommit repo oid
        case mcommit of
          Nothing     -> error ("Cannot find ref: " ++ show oid)
          Just commit -> getHistory limit commit

getHistory :: Int -> Commit -> IO [Commit]
getHistory 0 _ = return []
getHistory n c = do
    ps <- getCommitParents c
    case ps of
      []    -> return [c]
      (p:_) -> do
        ps' <- getHistory (n-1) p
        return (c:ps')

instance ToJSON Commit where
  toJSON c = object [
        "hash"      .= show (getId c)
      , "author"    .= commitAuthor c
      , "committer" .= commitCommitter c
      , "log"       .= commitLog c
      ]

instance ToJSON Signature where
  toJSON s = object [
        "name"  .= signatureName s
      , "email" .= signatureEmail s
      , "when"  .= signatureWhen s
      ]

------------------------------------------------------------------------
