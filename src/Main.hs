{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON(..), object, (.=))
import           Data.Attoparsec.Text.Lazy
import           Data.Git
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (toStrict, fromStrict)
import           Filesystem.Path.CurrentOS
import           Network (withSocketsDo)
import           Network.Wai (rawPathInfo)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static hiding ((<|>))
import           Web.Scotty

import           Prelude hiding (FilePath)

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Gittens v0.1"
    scotty 30090 app

app :: ScottyM ()
app = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    get anyPath $ do
        path <- param "path"
        case parseOnly pGitRequest path of
            Left _    -> next
            Right req -> process req

------------------------------------------------------------------------

process :: GitRequest -> ActionM ()

process (Refs path) = do
    repo <- liftIO (openRepository path)
    refs <- liftIO (listRefNames repo allRefsFlag)
    json refs

process (Commits path ref limit) = do
    repo    <- liftIO (openRepository path)
    commits <- liftIO (getCommits repo ref limit)
    json commits

------------------------------------------------------------------------

type RepoPath = FilePath
type OidOrRef = Text

data GitRequest
    = Refs    RepoPath
    | Commits RepoPath OidOrRef Int
    deriving (Eq, Show)

pGitRequest :: Parser GitRequest
pGitRequest =
      Refs    <$> pRepoPath <*. "refs"
  <|> Commits <$> pRepoPath <*> pOidOrRef <*> "commits/" .*> decimal

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

pOidOrRef :: Parser OidOrRef
pOidOrRef = takeWhile1 (/= ':') <* char ':'

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

getCommits :: Repository -> Text -> Int -> IO [Commit]
getCommits repo ref limit = do
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
