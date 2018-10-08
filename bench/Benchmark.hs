import           Criterion
import           Criterion.Main

import           System.FilePath.Posix                    ( joinPath )
import           System.IO                                ( withFile
                                                          , IOMode(..)
                                                          )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Text.HTML.DOM                            ( parseLBS )
import           Text.XML.Cursor                          ( fromDocument )

import qualified BrowseMode
import qualified ScrapeGumtreeAd
import qualified Search

main :: IO ()
main = defaultMain
  [ env (setup gumtreeListing) $ \ ~content -> bgroup
      "BrowseMode.scrapeHrefs"
      [ bench "v1c9073l3200208p1"
          $ nf (`BrowseMode.scrapeHrefs` ScrapeGumtreeAd.gumtreeHrefSelector) content
      ]
  , env (setup gumtreeAd) $ \ ~content -> bgroup
      "ScrapeGumtreeAd.scrape"
      [ bench "1003150974170911459345409"
          $ nf (ScrapeGumtreeAd.scrape . fromDocument . parseLBS) content
      ]
  , env (setupRules gumtreeAd) $ \ ~(white, black, scrape) -> bgroup
      "Search.search"
      [ bench "krakow.rules 1003150974170911459345409"
          $ nf (Search.search white black) scrape
      ]
  ]

data Fixture = Snapshot { sourceUrl :: String, contentSnapshotPath :: String }

gumtreeListing :: Fixture
gumtreeListing = Snapshot
  "https://www.gumtree.pl/s-mieszkania-i-domy-sprzedam-i-kupie/krakow/v1c9073l3200208p1"
  "v1c9073l3200208p1"

gumtreeAd :: Fixture
gumtreeAd = Snapshot
  "https://www.gumtree.pl/a-mieszkania-i-domy-sprzedam-i-kupie/krakow/nowoczesny-i-przestronny-dom-276m2-2018-rok-luksusowy-standard-energooszczedny-krakow-wegrzce/1003150974170911459345409"
  "1003150974170911459345409"


-- | Note: We assume resources are relative to project root
--
-- TODO: Detect CWD relative to root. If outside, require explicit path.
resourcesPath :: String
resourcesPath = "test/resources"

resolvePath :: String -> String
resolvePath p = joinPath [resourcesPath, p]

setup :: Fixture -> IO LBS.ByteString
setup fixture = withFile (contentPath fixture) ReadMode $ \hContent -> do
  content <- LBS.hGetContents hContent
  LBS.length content `seq` return content

contentPath :: Fixture -> String
contentPath = resolvePath . contentSnapshotPath

setupRules :: Fixture -> IO ([Text], [Text], Text)
setupRules fixture = withFile scrapePath ReadMode
  $ \hScrape -> withFile rulesPath ReadMode
  $ \hRules -> do
    rules <- TIO.hGetContents hRules
    let (white, black) = Search.parseRules rules
    scrape <- TIO.hGetContents hScrape
    return (white, black, T.length scrape `seq` scrape)
   where
        rulesPath  = resolvePath "krakow.rules" -- TODO: configurable
        scrapePath = resolvePath $ contentSnapshotPath fixture ++ ".golden"
