{-# LANGUAGE TemplateHaskell #-}

module ScrapeTests where

import qualified Hedgehog.Gen                  as Gen
import           Data.List                                ( isPrefixOf )
import           Data.Maybe                               ( isJust )
import qualified Hedgehog.Range                as Range
import           Hedgehog

import qualified BrowseMode

-- | Random characters that tend to appear in HTTP URLs
genUrlChar :: (MonadGen m) => m Char
genUrlChar = Gen.frequency
  [ (90, Gen.alphaNum)
  , (10, Gen.constant '/')
  , (5 , Gen.constant '.')
  , (2 , Gen.constant '#')
  ]

data Proto = Http | Https deriving (Bounded, Enum, Ord, Eq)

instance Show Proto where
  show Http = "http"
  show Https = "https"

-- | Gen supported URI schemes
genProto :: (MonadGen m) => m Proto
genProto = Gen.element [minBound .. maxBound]

-- | Gen String resembling HTTP URL
genUrlString :: (MonadGen m) => Proto -> m String
genUrlString proto = do
  pseudoUrl <- Gen.list (Range.linear 1 150) genUrlChar
  return $ show proto ++ "://" ++ pseudoUrl

canParse :: String -> Bool
canParse = isJust . BrowseMode.parsePrefix

-- | parsePrefix allows us later construct URLs from relative hrefs
prop_parsePrefix :: Property
prop_parsePrefix = property $ do
  proto     <- forAll genProto
  urlString <- forAll $ Gen.filter canParse $ genUrlString proto -- TODO: avoid useless work
  case BrowseMode.parsePrefix urlString of
    Just (prefix, _) -> assert $ (show proto ++ "://") `isPrefixOf` prefix
    _ -> fail $ "parsePrefix did not recognize '" ++ urlString ++ "'"

tests :: IO Bool
tests = checkParallel $$discover
