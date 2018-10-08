{-# LANGUAGE OverloadedStrings #-}

-- | Support module for Gumtree.pl
module ScrapeGumtreeAd
  ( scrape
  , gumtreeHrefSelector
  )
where

import           Control.Monad
import qualified Data.Text                     as T
import           Data.Text                                ( Text )
import           Text.HTML.Scalpel                        ( Selector
                                                          , hasClass
                                                          , (@:)
                                                          )
import           Text.XML.Cursor                          ( Cursor
                                                          , attributeIs
                                                          , content
                                                          , element
                                                          , child
                                                          , descendant
                                                          , ($//)
                                                          , (&|)
                                                          )
import           Text.XML                                 ( Node(..) )
import           Data.List                                ( )
import qualified Text.XML.Cursor.Generic       as CG
import           Data.Maybe                               ( )

-- | Matches links to clasifieds in the form of <a href-link="...">
--
-- TODO: consider settling with a single library
gumtreeHrefSelector :: Selector
gumtreeHrefSelector = "a" @: [hasClass "href-link"]

findNodes :: Cursor -> [Cursor]
findNodes = element "span" >=> attributeIs "class" "pre"

findTitle :: Cursor -> [Cursor]
findTitle = element "head" >=> child >=> element "title"

findAttributeTable :: Cursor -> [Cursor]
findAttributeTable = element "table" >=> attributeIs "id" "attributeTable"

-- TODO: move to Szybko support module
szybkoContent :: Cursor -> [Cursor]
szybkoContent = element "div" >=> attributeIs "class" "col-md-8"

extractData :: Cursor -> Text
extractData = T.concat . concatMap (filterContent . content) . descendant

filterContent :: [Text] -> [Text]
filterContent = filter $ (> 3) . T.length

processData :: [Text] -> [Text]
processData = removeBlanks . map T.strip . concatMap T.lines

removeBlanks :: [Text] -> [Text]
removeBlanks = filter $ not . T.null

-- | Given document cursor, navigate and extract content using implied
-- knowledge about document structure
scrape :: Cursor -> Text
scrape cursor =
  -- TODO: run only one scraper given expected structure based on Url
  let axes = [findTitle, findNodes, findAttributeTable, szybkoContent]
      axisProc :: CG.Axis Node -> Text
      axisProc axis = T.unlines $ processData $ cursor $// axis &| extractData
  in  T.unlines $ map axisProc axes
