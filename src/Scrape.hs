{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extract selected content from documents
--
module Scrape
  ( collect
  , list
  , match
  , fetch
  )
where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List
import qualified Data.HashMap.Strict           as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                     as T
import           System.IO                         hiding ( readFile
                                                          , getContents
                                                          )
import           Prelude                           hiding ( IO
                                                          , getContents
                                                          , readFile
                                                          )

-- Project modules

import qualified Db
import           Db                                       ( MonadDb )
import qualified HashContent
import           Infrastructure
import qualified Logger
import qualified Search

-- | Load stored urls, scrape, deduplicate and store interesting content
collect
  :: ( MonadReader Env m
     , MonadReduce m
     , MonadDb m
     , MonadLogger m
     , MonadConsole m
     , MonadHttp m
     )
  => m ()
collect = do
  knownUrls   <- Db.recentlyFetched 299
  backlogUrls <- Db.recentlyAdded 999
  dupes       <- Db.loadDupeSignatures 1000
  let newUrls = backlogUrls \\ knownUrls
      dupeMap = M.fromList dupes
  mapM_ printLnText newUrls
  if null newUrls
    then Logger.info "No new uls"
    else case comparing length newUrls backlogUrls of
      EQ -> Logger.info "All Urls are new - there may be more ..."
      _ ->
        Logger.info
          "Some Urls were already fetched - we will likely have them all this round"
  forkScrape (map T.unpack newUrls) $ \row@(url, content) ->
    let h = HashContent.hashContent content
    in
      if h `M.member` dupeMap
        then
          Logger.info
          $  "Hash "
          ++ show h
          ++ " already seen for "
          ++ T.unpack url
        else do
          Db.storeContentRow row
          Db.storeDupeSignature (HashContent.hashContent content, url)
  _ <- Db.markVisited backlogUrls
  return ()

-- | Print to stdout interesting content from a given url
fetch
  :: ( MonadReader Env m
     , MonadConsole m
     , MonadLogger m
     , MonadConsole m
     , MonadReduce m
     , MonadHttp m
     )
  => FilePath
  -> m ()
fetch url = forkScrape [url] (\(_, content) -> printLnText content)

type Hours = Int

-- | Print to stdout recently stored content
list :: (MonadConsole m, MonadDb m) => Maybe Hours -> m ()
list nHours = do
  scrapes <- Db.loadContent (fromMaybe 24 nHours) -- TODO: take args
  mapM_ (printLnText . fst) scrapes

-- | Print to stdout rules match results against stdin
match :: (MonadConsole m, MonadFiles m, MonadHttp m) => FilePath -> m ()
match rulesPath = do
  content <- getContents
  rules   <- readFile rulesPath
  let (likes, dislikes) = Search.parseRules rules
      result            = Search.search likes dislikes content
  printLn $ show result
