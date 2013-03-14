{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Applicative
import           Control.Arrow (first, second)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON(..), object, (.=))
import           Data.Attoparsec.Text.Lazy
import           Data.Git
import qualified Data.HashSet as H
import qualified Data.Map as M
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (toStrict, fromStrict)
import           Filesystem.Path.CurrentOS hiding (concat)
import           Network (withSocketsDo)
import           Network.Wai (rawPathInfo)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static hiding ((<|>))
import           Web.Scotty hiding (body, files)

import           Prelude hiding (FilePath, log)

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Gittens v0.1"
    scotty 30090 app

app :: ScottyM ()
app = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ file "static/index.html"

    get anyPath $ do
        path <- param "path"
        case parseOnly pGitRequest path of
            Left _    -> next
            Right req -> process req

------------------------------------------------------------------------

process :: GitRequest -> ActionM ()

process (GitRefs path) =
    (json =<<) . liftIO $ do
        repo <- openRepository path
        listRefNames repo allRefsFlag

process (GitCommits path ref limit) = do
    (json =<<) . liftIO $ do
        repo   <- openRepository path
        commit <- getCommit repo ref
        getHistory limit commit

process (GitTree path ref) = do
    (json =<<) . liftIO $ do
        repo  <- openRepository path
        hashBlobs <$> getCommitFiles repo ref

process (GitDiff path refA refB) = do
    (json =<<) . liftIO $ do
        repo   <- openRepository path
        filesA <- hashBlobs <$> getCommitFiles repo refA
        filesB <- hashBlobs <$> getCommitFiles repo refB
        return $ H.map fst (H.difference filesA filesB)

hashBlobs :: [(Text, ObjRef Blob)] -> H.HashSet (Text, Text)
hashBlobs = H.fromList . map (second $ T.pack . showId)
  where
    showId (IdRef oid) = show oid
    showId (ObjRef b)  = show (getId b)

------------------------------------------------------------------------

type RepoPath = FilePath
type Revision = Text

data GitRequest
    = GitRefs    RepoPath
    | GitCommits RepoPath Revision Int
    | GitTree    RepoPath Revision
    | GitDiff    RepoPath Revision Revision
    deriving (Eq, Show)

pGitRequest :: Parser GitRequest
pGitRequest =
      GitRefs    <$> pRepoPath <*. "refs"
  <|> GitCommits <$> pRepoPath <*> pRevision <*> "commits/" .*> decimal
  <|> GitTree    <$> pRepoPath <*> pRevision <*. "tree"
  <|> GitDiff    <$> pRepoPath <*> pRevision <*> pRevision <*. "diff"

pRepoPath :: Parser RepoPath
pRepoPath = fromText <$> (windows <|> unix)
  where
    path = takeWhile1 (/= ':')
    unix = path <* char ':'
    windows = do
      char '/'
      d <- T.singleton <$> letter
      char ':'
      p <- path
      char ':'
      return (d <> ":" <> p)

pRevision :: Parser Revision
pRevision = takeWhile1 (/= ':') <* char ':'

------------------------------------------------------------------------

instance Parsable Text where
    parseParam = Right . toStrict

anyPath :: RoutePattern
anyPath = function $ \rq -> Just [("path", textPath rq)]
  where
    textPath = fromStrict . decodeUtf8 . rawPathInfo

------------------------------------------------------------------------

resolveRef' :: Repository -> Text -> IO (Maybe Oid)
resolveRef' repo ref = do
    xs <- sequence [
        resolveRef repo ref
      , resolveRef repo ("refs/heads/" <> ref)
      , resolveRef repo ("refs/tags/" <> ref)
      , resolveRef repo ("refs/remotes/" <> ref)
      , parseOid ref
      ]
    return $ listToMaybe $ catMaybes xs

getCommit :: Repository -> Text -> IO Commit
getCommit repo ref = do
    mOid <- resolveRef' repo ref
    case mOid of
      Nothing  -> error ("Cannot resolve: " ++ unpack ref)
      Just oid -> do
        mCommit <- lookupCommit repo oid
        case mCommit of
          Nothing     -> error ("Cannot find ref: " ++ show oid)
          Just commit -> return commit

getHistory :: Int -> Commit -> IO [Commit]
getHistory 0 _ = return []
getHistory n c = do
    ps <- getCommitParents c
    case ps of
      []    -> return [c]
      (p:_) -> do
        ps' <- getHistory (n-1) p
        return (c:ps')

getCommitFiles :: Repository -> Text -> IO [(Text, ObjRef Blob)]
getCommitFiles repo ref = do
    commit <- getCommit repo ref
    tree   <- loadObject' (commitTree commit) commit
    getFiles tree

getFiles :: Tree -> IO [(Text, ObjRef Blob)]
getFiles tree =
    liftM concat . mapM go $ M.toList $ treeContents tree
  where
    go (name, BlobEntry ref _) = return [(name, ref)]
    go (name, TreeEntry ref)   = do
        subTree <- loadObject' ref tree
        files   <- getFiles subTree
        return (map prefix files)
      where
        prefix = first (name `T.append` "/" `T.append`)

instance ToJSON Commit where
  toJSON c = object [
        "hash"      .= show (getId c)
      , "author"    .= commitAuthor c
      , "committer" .= commitCommitter c
      , "subject"   .= subject
      , "body"      .= body
      ]
    where
      log     = commitLog c
      subject = T.takeWhile (/= '\n') log
      body    = T.strip $ T.dropWhile (/= '\n') log

instance ToJSON Signature where
  toJSON s = object [
        "name"  .= signatureName s
      , "email" .= signatureEmail s
      , "when"  .= signatureWhen s
      ]

