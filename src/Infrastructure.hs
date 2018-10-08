{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Scraper infrastructure
--
-- Provides effect abstractions and their implementations in IO
--
module Infrastructure where

import           Control.Concurrent.TokenBucket
import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Char8
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Network.HTTP.Client           as C
import qualified Network.HTTP.Types            as HTTP
import qualified Network.URI                   as URI
import           Network.URI                              ( URI )
import           Options.Generic
import           Prelude                           hiding ( error
                                                          , readFile
                                                          )
import           System.Log.Logger
import qualified Text.HTML.DOM                 as DOM
import qualified Text.XML.Cursor               as Cursor
import           Text.XML.Cursor                          ( Cursor )

-- Project modules

import qualified Logger
import qualified ScrapeGumtreeAd

-- | Abstracts filesystem access
class Monad m => MonadFiles m where
  readFile :: String  -> m Text

instance MonadFiles IO where
  readFile = TIO.readFile

newtype Env = Env { httpClientManager :: C.Manager }
type App = ReaderT Env IO

-- | Abstracts HTTP protocol
class Monad m => MonadHttp m where
  -- | GET HTTP resource using given connection manager
  httpLbsM :: C.Manager -> URI -> m (C.Response LBS.ByteString)

instance MonadHttp IO where
  httpLbsM manager uri = do
   rq <- C.requestFromURI uri
   logDebugN $ T.pack $ "fetching " ++ insecureUriToString uri
   C.httpLbs rq manager

-- | Abstracts console I/O
class Monad m => MonadConsole m where
  printLn :: String -> m ()
  printLnText :: Text -> m ()
  getContents :: m Text

instance MonadConsole IO where
  printLn = putStrLn
  printLnText = TIO.putStrLn
  getContents  = TIO.getContents

-- TODO: suppress orphan warning in this module
instance MonadLogger IO where
  -- TODO: try fast-logger
  monadLoggerLog _ _ lvl msg = logM Logger.rootLogger (toPriority lvl) (show $ toLogStr msg)

toPriority :: LogLevel -> Priority
toPriority level = case level of
  LevelDebug   -> DEBUG
  LevelInfo    -> INFO
  LevelWarn    -> WARNING
  LevelError   -> ERROR
  LevelOther _ -> ERROR

-- | URI to String potentially exposing authentication information
--
-- TODO: Use ShowS as intended
--
insecureUriToString :: URI -> String
insecureUriToString uri = URI.uriToString id uri ""

type UrlText = Text
type UrlString = String

-- | Abstracts common concurrency pattern
class Monad m => MonadReduce m where
    reduce :: (Monoid b) => [String] -> (String -> m b) -> m b

instance MonadReduce IO where
  reduce xs f = do
   bucket <- newTokenBucket
   bs <- mapConcurrently (action bucket) xs
   return $ mconcat $ catMaybes bs
    where
      action bucket url = catch
        -- TODO: more general
        (action' bucket url)
        (\(e :: C.HttpException) -> do
          Logger.warn $ "Failed to fetch " ++ url ++ " with exception: " ++ show e
          return Nothing
        )
      action' bucket urlString = do
        tokenBucketWait bucket burstSize inverseRate
        b <- f urlString
        return $ Just b
       where
        burstSize   = 1
        inverseRate = 200 * 1000 -- usec/token

-- | Simple example for MonadReduce in IO
--
-- >>> reduce ["a", "b"] (\s -> simpleIOaction  s )
-- start a
-- start b
-- done a
-- done b
-- "ab"
--
simpleIOaction :: String -> IO String
simpleIOaction msg = do
  putStrLn $ "start " ++ msg
  threadDelay $ 2 * 1000 * 1000
  putStrLn $ "done " ++ msg
  return msg

-- | Scrape Web pages concurrently with throttling. Run handler on each.
--
-- TODO: respect robots.txt
--
forkScrape
  :: (MonadReader Env m, MonadLogger m, MonadHttp m, MonadReduce m)
  => [UrlString]
  -> ((UrlText, Text) -> m ())
  -> m ()
forkScrape urls handler = do
  _ <- reduce urls action
  return ()
 where
  action urlString = case URI.parseAbsoluteURI urlString of
    Nothing  -> rejectUrlString $ T.pack urlString
    Just uri -> do
      env      <- ask
      response <- httpLbsM (httpClientManager env) uri
      let status = C.responseStatus response
      case HTTP.statusCode status of
        200 -> handler (T.pack urlString, ScrapeGumtreeAd.scrape cursor)
          where cursor = cursorFor response
        code -> logErrorN $ "HTTP status " <> T.pack (show code) <> T.pack
          (Data.ByteString.Char8.unpack (HTTP.statusMessage status))

-- | Get Cursor pointing to Response body
cursorFor :: C.Response LBS.ByteString -> Cursor
cursorFor response =
  Cursor.fromDocument (DOM.parseLBS (C.responseBody response))

-- | Reports bad absolute URIs
rejectUrlString :: (MonadLogger m) => Text -> m ()
rejectUrlString urlString =
  logErrorN ("Cannot parse as absolute URI: " <> urlString)
