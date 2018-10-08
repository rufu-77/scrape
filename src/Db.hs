
{-# LANGUAGE OverloadedStrings #-}
-- | Persistent storage
--
-- We persist queued URLs, fetched content, search results plus some
-- administrative information like timestamps, content hash for deduplication
-- etc.
--
module Db
  ( UrlText
  , UrlString
  , Content
  , ContentRow
  , Hash
  , Limit
  , LikeRow
  , Note
  , MonadDb(..)
  )
where

import           Database.SQLite.Simple
import           Control.Exception
import           Data.List
import           Data.Text                                ( Text )

-- Synonims for readability

type UrlText = Text
type UrlString = String
type Content = Text
type ContentRow = (UrlText, Content)
type Hash = Int
type Note = Text
type LikeRow = (Hash, UrlText, Content, Note)

-- | Database actions
class Monad m => MonadDb m where
  loadContent :: Int  -> m [ContentRow]
  loadDupeSignatures :: Int -> m [(Hash, UrlText)]
  recentlyFetched :: Limit -> m [UrlText]
  recentlyAdded :: Limit -> m [UrlText]
  storeLiked :: LikeRow -> m ()
  storeContentRow :: ContentRow -> m ()
  storeDupeSignature :: (Hash, UrlText) -> m ()
  storeUrls :: [UrlString] -> m ()
  markVisited :: [UrlText] -> m [Int]

-- TODO: keep connection(s) in the environment
instance MonadDb IO where
  loadContent = loadContent_
  loadDupeSignatures = loadDupeSignatures_
  storeLiked = storeLiked_
  recentlyFetched = recentlyFetched_
  recentlyAdded = recentlyAdded_
  storeContentRow = storeContentRow_
  storeUrls =  storeUrls_
  storeDupeSignature = storeDupeSignature_
  markVisited = markVisited_

withConn :: (Connection -> IO c) -> IO c
withConn = bracket (open "scrape.db") close

storeContentRow_ :: ContentRow -> IO ()
storeContentRow_ row = withConn
  (\conn -> execute
    conn
    "INSERT OR IGNORE INTO content (url, content) VALUES (?, ?)"
    row
  )

loadDupeSignatures_ :: Int -> IO [(Hash, UrlText)]
loadDupeSignatures_ limit = withConn
  (\conn ->
    query conn
          "SELECT hash, recent_url from dupe order by timestamp desc limit ?"
          (Only (show limit)) :: IO [(Hash, UrlText)]
  )

storeDupeSignature_ :: (Hash, UrlText) -> IO ()
storeDupeSignature_ sig = withConn $ \conn -> execute
  conn
  "INSERT OR REPLACE INTO dupe (hash, recent_url) VALUES (?, ?)"
  sig

storeLiked_ :: LikeRow -> IO ()
storeLiked_ row = withConn
  (\conn -> execute
    conn
    "INSERT OR REPLACE INTO like (hash, url, content, note) VALUES (?, ?, ?, ?)"
    row
  )

loadContent_ :: Int -> IO [ContentRow]
loadContent_ hours = withConn
  (\conn ->
    query conn
          "SELECT url, content from content where timestamp > date('now', ?)"
          (Only ("-" ++ show hours ++ " hours")) :: IO [ContentRow]
  )

type Limit = Int
recentlyFetched_ :: Limit -> IO [UrlText]
recentlyFetched_ limit = map fromOnly <$> withConn
  (\conn ->
    query conn
          "SELECT url from content order by timestamp desc limit ?"
          (Only (show limit)) :: IO [Only UrlText]
  )

storeUrls_ :: [String] -> IO ()
storeUrls_ urls = withConn $ \conn -> do
  updates <- mapM (dbAction conn) urls
  let insertedCount = foldl' (+) 0 updates
  putStrLn $ "New urls: " ++ show insertedCount ++ " from input of " ++ show
    (length urls)
 where
  dbAction conn url = do
    execute conn
            "INSERT OR IGNORE INTO url (url) VALUES (?)"
            (Only (url :: String))
    changes conn

markVisited_ :: [UrlText] -> IO [Int]
markVisited_ urls = withConn $ \conn -> mapM (dbAction conn) urls
 where
  dbAction conn url = do
    execute conn
            "UPDATE OR IGNORE url SET visited = 1 WHERE url = ?"
            (Only (url :: UrlText))
    changes conn

recentlyAdded_ :: Limit -> IO [UrlText]
recentlyAdded_ limit = map fromOnly <$> withConn
  (\conn ->
    query
      conn
      "SELECT DISTINCT url.url from url left outer join content on url.url = content.url where content.url is null and url.visited != 1 order by url.timestamp asc limit ?"
      (Only (show limit)) :: IO [Only UrlText]
  )
