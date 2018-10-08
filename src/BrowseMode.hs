{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sraper's "Browse" mode
-- 
-- In this mode, we collect hrefs for later use
--
module BrowseMode
  ( browse
  , allHrefs
  , fetchAndStoreHrefs
  , fetchAndPrintHrefs
  , scrapeHrefs -- | For testing purposes
  , parsePrefix -- | Deprecated. Use uriAuthority.
  )
where

import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.ByteString.Char8
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Network.HTTP.Client           as C
import qualified Network.HTTP.Types            as HTTP
import qualified Network.URI                   as URI
import           Network.URI                              ( URI
                                                          , uriScheme
                                                          , parseURI
                                                          , uriAuthority
                                                          , uriRegName
                                                          )
import           Prelude                           hiding ( error
                                                          , readFile
                                                          )
import           Text.HTML.Scalpel                        ( Selector
                                                          , scrapeStringLike
                                                          , hasClass
                                                          , chroots
                                                          , attr
                                                          , anySelector
                                                          , (@:)
                                                          , (//)
                                                          )
import           Text.StringLike


-- Project

import           Db
import           Infrastructure
import qualified ScrapeGumtreeAd

-- | Fetch classifieds page and persist links locally
fetchAndStoreHrefs
  :: (MonadReader Env m, MonadLogger m, MonadHttp m, MonadDb m)
  => String
  -> m ()
fetchAndStoreHrefs urlString = case URI.parseAbsoluteURI urlString of
  -- TODO: typically listing order is based on recency => stop when we caught up
  Nothing  -> rejectUrlString $ T.pack urlString
  Just uri -> do
    env      <- ask
    response <- httpLbsM (httpClientManager env) uri
    browse response uri (Db.storeUrls . map insecureUriToString)

-- | Fetch classifieds page and print links to stdout
fetchAndPrintHrefs
  :: (MonadReader Env m, MonadLogger m, MonadHttp m, MonadDb m, MonadConsole m)
  => String
  -> m ()
fetchAndPrintHrefs urlString = case URI.parseAbsoluteURI urlString of
  Nothing  -> rejectUrlString $ T.pack urlString
  Just uri -> do
    env      <- ask
    response <- httpLbsM (httpClientManager env) uri
    browse response uri (mapM_ $ printLn . insecureUriToString)

-- | Extract links from document and pass to custom action
--
-- TODO: let caller handle HTTP errors, redirects etc.
-- TODO: discuss CPS usefullness
--
browse
  :: (MonadLogger m, StringLike body, Ord body, Show body)
  => C.Response body
  -> URI
  -> ([URI] -> m ())
  -> m ()
browse response uri urlsAction = case HTTP.statusCode status of
  200  -> urlsAction absoluteHRefs
  code -> logErrorN $ "HTTP status " <> T.pack (show code) <> T.pack
    (Data.ByteString.Char8.unpack (HTTP.statusMessage status))
 where
  status        = C.responseStatus response
  body          = C.responseBody response
  hrefs         = mapMaybe URI.parseURIReference $ allHrefs body
  absoluteHRefs = map (`URI.relativeTo` uri) hrefs

-- | Given content and selector, returns matching hrefs
scrapeHrefs
  :: (Ord content, Show content, StringLike content)
  => content
  -> Selector
  -> [content]
scrapeHrefs content selector = (concat . maybeToList)
  $ scrapeStringLike content (chroots selector (attr "href" anySelector))

-- | Extracts links from szybko.pl
-- TODO: move to support module
szybkoHrefSelector :: Selector
szybkoHrefSelector = "div" @: [hasClass "asset-list"] // "a"

-- | Extract links using implied document structure
allHrefs :: (StringLike a, Ord a, Show a) => a -> [String]
allHrefs content = concatMap
  (map toString . scrapeHrefs content)
  [szybkoHrefSelector, ScrapeGumtreeAd.gumtreeHrefSelector]

type Prefix = String

-- | Parse URI and split prefix from String
-- | Deprecated. Use uriAuthority.
--
-- >>> parsePrefix "https://foo.com/bar#baz"
-- Just ("https://foo.com",https://foo.com/bar#baz)
--
-- >>> parsePrefix "./bar"
-- Nothing
--
parsePrefix :: String -> Maybe (Prefix, URI)
parsePrefix url = do
  uri       <- parseURI url
  authority <- uriAuthority uri
  let prefix = uriScheme uri ++ "//" ++ uriRegName authority
  Just (prefix, uri)
