{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- User's command line interface for the scraper -}
module Main where

import           Control.Concurrent.STM
import           Control.Concurrent.Async.Lifted.Safe
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Maybe
import           Options.Generic
import qualified Network.HTTP.Client           as C
import           Network.HTTP.Client.TLS                  ( tlsManagerSettings )
import           Prelude                           hiding ( getContents
                                                          , readFile
                                                          )

-- Project modules

import qualified BrowseMode
import qualified Logger
import qualified Scrape
import qualified SearchMode
import           Db
import           Infrastructure

-- Types and aliases for readability

type Url = String
type RulesPath = String
type NHours = Int

data CmdLine =
               -- | Browse and store url of each ad listed at given aggregate urls
               Browse [Url]
               -- | Collect and store ad contents for urls which don't have them yet
             | Collect
             | Fetch Url      -- print scraped ad contents at url
             -- | Perform search using given rules on recently stored content
             | Search RulesPath (Maybe NHours)
             | Match RulesPath -- run match rules against textual content from stdin
             | List (Maybe NHours) -- print links of each ad stored
             | Links Url    -- print links of each ad at a given aggregate url
             deriving (Generic, Show)

-- TODO: improve help text (type synonim names don't propagate)
instance ParseRecord CmdLine

-- TODO: is it possible to avoid this boilerplate?
instance (MonadConsole IO) => MonadConsole (ReaderT env IO) where
  printLn = lift . printLn
  printLnText = lift . printLnText
  getContents  = lift getContents

instance (MonadDb IO) => MonadDb (ReaderT env IO) where
  loadContent = lift . loadContent
  loadDupeSignatures = lift . loadDupeSignatures
  storeLiked = lift . storeLiked
  recentlyFetched = lift . recentlyFetched
  recentlyAdded = lift . recentlyAdded
  storeContentRow = lift . storeContentRow
  storeUrls =  lift . storeUrls
  storeDupeSignature = lift . storeDupeSignature
  markVisited = lift . markVisited

instance (MonadFiles IO) => MonadFiles (ReaderT env IO) where
  readFile = lift . readFile

instance (MonadHttp IO) => MonadHttp (ReaderT env IO) where
  httpLbsM manager = lift . httpLbsM manager

instance (MonadReduce IO) => MonadReduce (ReaderT env IO) where
  reduce xs f = ReaderT $ \env -> reduce xs (`action` env)
   where
    action x = runReaderT (f x)

selectMode
  :: ( MonadReader Env m
     , MonadLogger m
     , MonadDb m
     , MonadFiles m
     , MonadReduce m
     , MonadConsole m
     , MonadHttp m
     )
  => CmdLine
  -> m ()
selectMode cmdLine = case cmdLine of
  Browse urls -> mapM_ BrowseMode.fetchAndStoreHrefs urls
  Links  url  -> BrowseMode.fetchAndPrintHrefs url
  Collect     -> Scrape.collect
  Fetch url   -> Scrape.fetch url
  Search rulesPath nHours ->
    SearchMode.searchMode rulesPath (fromMaybe 24 nHours)
  List  nHours    -> Scrape.list nHours
  Match rulesPath -> Scrape.match rulesPath

-- | Runs the scraper in selected mode
main :: IO ()
main = do
  cmdLine <- getRecord "Scrape - HTML scraping toolkit"
  Logger.setupLogger "scrape.log" -- TODO: add general option
  manager <- C.newManager $ tlsManagerSettings { C.managerConnCount = 4 }
  runReaderT (selectMode cmdLine) $ Env manager
