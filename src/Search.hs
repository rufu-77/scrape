{-# LANGUAGE FlexibleContexts #-}

-- | Core classifier logic
module Search
  ( parseRules
  , search
  )
where

import           Data.Bifunctor
import qualified Data.Text                     as T
import           Data.Text                                ( Text )
import           Text.Regex.TDFA.Text
import           Text.Regex.TDFA

-- | Rudimentary parser using 'read'
parseRules :: Text -> ([Text], [Text])
parseRules = read . T.unpack

-- | Given 'likes' 'dislikes' regex patterns, classify content
--
-- TODO: benchmark and ensure compiled regex is shared
search :: [Text] -> [Text] -> Text -> Either [Text] [Text]
search likes dislikes content =
  case bimap (matches content) (matches content) (likes, dislikes) of
    ([]   , _       ) -> Left []
    (liked, []      ) -> Right liked
    (_    , disliked) -> Left disliked

-- TODO: remove
matches :: Text -> [Text] -> [Text]
matches content = concatMap (\expr -> matchM (caseInsensitive expr) content)

caseInsensitive
  :: RegexMaker Regex CompOption ExecOption source => source -> Regex
caseInsensitive =
  makeRegexOpts (defaultCompOpt { caseSensitive = False }) defaultExecOpt

