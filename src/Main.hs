{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Git
import Data.Text (Text)
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

    get "/commits" $ do
        commits "HEAD" 100

    get "/commits/:limit" $ do
        limit <- param "limit"
        commits "HEAD" limit

commits :: Text -> Int -> ActionM ()
commits ref limit = liftIO (getCommits ref limit) >>= json

------------------------------------------------------------------------

getCommits :: Text -> Int -> IO [Commit]
getCommits ref limit = do
    repo <- openRepository (fromText "c:/development/ng/starfix-ng")
    (Just commit) <- lookupRefCommit repo ref
    getHistory limit commit

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

-- prerntKermert :: Commit -> IO [Commit]
-- prerntKermert c = do
--     let msg = encodeUtf8 $ commitLog c
--     B.putStrLn $ B.concat ["<div>", msg, "</div>"]
--     parents <- getCommitParents c
--     case parents of
--         []    -> return ()
--         (p:_) -> prerntKermert p

-- fromStatus :: Monad m => Status -> m Response
-- fromStatus s = fromStatus' s msg
--   where
--     msg = LT.decodeUtf8 (LB.fromChunks [statusMessage s])
-- 
-- fromStatus' :: Monad m => Status -> LT.Text -> m Response
-- fromStatus' s msg = return (responseLazyText s hdr msg)
--   where
--     hdr = [("Content-Type", "text/plain")]
-- 
-- responseLazyText :: Status -> ResponseHeaders -> LT.Text -> Response
-- responseLazyText s h t = ResponseBuilder s h (fromLazyText t)
-- 
-- responseJSON :: Status -> ResponseHeaders -> Value -> Response
-- responseJSON s hs x = responseLazyText s hs' (enc x)
--   where
--     hs' = [("Content-Type", "application/json")] ++ hs
--     enc = toLazyText . fromValue
