{-# LANGUAGE OverloadedStrings #-}

-- | Scraper's "Search" mode
module SearchMode where

import qualified Data.Text                     as T
import           Data.Text                                ( Text )
import           Data.Hashable
import           Control.Monad.Logger
import           Prelude                           hiding ( error
                                                          , readFile
                                                          )

-- Project

import           Db
import           Infrastructure
import qualified Search                                   ( parseRules
                                                          , search
                                                          )
type Url = Text
type Liked = Text
type Disliked = Text

-- | Read rules, apply to recently "fetched" content and persist results
searchMode :: (MonadLogger m, MonadFiles m, MonadDb m) => String -> Int -> m ()
searchMode rulesPath nHours = do
  rules <- readFile rulesPath
  let (likes, dislikes) = Search.parseRules rules
  scrapes <- loadContent nHours
  logInfoN $ "analyzing " <> T.pack (show (length scrapes)) <> " urls"
  -- TODO: find less clever implementation
  let results = (fmap . fmap) (Search.search likes dislikes)
                              (map (\(x, y) -> ((x, y), y)) scrapes)
  mapM_ record results

-- | Persists positive matches, logs negative
record
  :: (MonadLogger m, MonadDb m)
  => ((Url, Content), Either [Liked] [Disliked])
  -> m ()
record ((url, content), result) = case result of
  Right liked ->
    Db.storeLiked (hash content, url, content, T.intercalate " " liked)
  Left disliked ->
    logInfoN
      $  "found negative match: "
      <> T.pack (show disliked)
      <> " in "
      <> url
