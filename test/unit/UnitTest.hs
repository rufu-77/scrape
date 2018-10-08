{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.FilePath.Posix                    ( joinPath )
import           System.IO                                ( Handle
                                                          , withFile
                                                          , IOMode(..)
                                                          , hGetContents
                                                          )
import           Test.Hspec

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text.IO                  as TIO
import           Text.HTML.DOM                            ( parseLBS )
import           Text.XML.Cursor                          ( fromDocument )


import qualified BrowseMode
import qualified Search
import qualified ScrapeGumtreeAd

-- | Note: We assume resources are relative to project root
--
-- TODO: Detect CWD relative to root. If outside, require explicit path.
resourcesPath :: String
resourcesPath = "test/resources"

resolvePath :: String -> String
resolvePath p = joinPath [resourcesPath, p]

-- Fixtures

data Fixture = Snapshot { sourceUrl :: String, contentSnapshotPath :: String }

gumtreeListing :: Fixture
gumtreeListing = Snapshot
  "https://www.gumtree.pl/s-mieszkania-i-domy-sprzedam-i-kupie/krakow/v1c9073l3200208p1"
  "v1c9073l3200208p1"

gumtreeAd :: Fixture
gumtreeAd = Snapshot
  "https://www.gumtree.pl/a-mieszkania-i-domy-sprzedam-i-kupie/krakow/nowoczesny-i-przestronny-dom-276m2-2018-rok-luksusowy-standard-energooszczedny-krakow-wegrzce/1003150974170911459345409"
  "1003150974170911459345409"

-- Tests

type HContent = Handle
type HGolden = Handle
type HRules = Handle
type HScrape = Handle

scrapeHrefsTest :: HContent -> HGolden -> IO ()
scrapeHrefsTest hContent hGolden = do
  content <- hGetContents hContent
  golden  <- hGetContents hGolden
  hspec
    $          describe "BrowseMode.scrapeHrefs"
    $ it "extracts hrefs from an HTML content assmuming certain structure"
    $          BrowseMode.scrapeHrefs content ScrapeGumtreeAd.gumtreeHrefSelector
    `shouldBe` read golden

scrapeGumtreeTest :: HContent -> HGolden -> IO ()
scrapeGumtreeTest hContent hGolden = do
  content <- LBS.hGetContents hContent
  golden  <- TIO.hGetContents hGolden
  hspec
    $          describe "ScrapeGumtreeAd.scrape"
    $ it "extracts interesting text from HTML assuming certain structure"
    $          ScrapeGumtreeAd.scrape (fromDocument $ parseLBS content)
    `shouldBe` golden

rulesTest :: HRules -> HScrape -> IO ()
rulesTest hRules hScrape = do
  rules <- TIO.hGetContents hRules
  let (white, black) = Search.parseRules rules
  scrape <- TIO.hGetContents hScrape
  hspec
    $          describe "Search.search"
    $ it "accepts any of whitelist patterns provided no match on blacklist"
    $          Search.search white black scrape
    `shouldBe` Right ["Zielonki"]

-- Main and infrastructure

main :: IO ()
main = do
  runGolden gumtreeListing scrapeHrefsTest
  runGolden gumtreeAd      scrapeGumtreeTest
  runRulesGolden gumtreeAd rulesTest

-- TODO: consider Managed
runGolden :: Fixture -> (HContent -> HGolden -> IO ()) -> IO ()
runGolden fixture test = withFile contentPath ReadMode $ \hContent ->
  withFile goldenPath ReadMode $ \hGolden -> test hContent hGolden
 where
  -- TODO: report source
  contentPath = resolvePath $ contentSnapshotPath fixture
  goldenPath  = contentPath ++ ".golden" -- convention

runRulesGolden :: Fixture -> (HRules -> HScrape -> IO ()) -> IO ()
runRulesGolden fixture test = withFile scrapePath ReadMode
  $ \hScrape -> withFile rulesPath ReadMode $ \hRules -> test hRules hScrape
 where
  -- TODO: report source
  rulesPath  = resolvePath "krakow.rules" -- TODO: configurable
  -- TODO: don't depend on another test's fixture
  scrapePath = resolvePath $ contentSnapshotPath fixture ++ ".golden"
