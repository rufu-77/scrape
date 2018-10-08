-- | Heuristic near-duplicate detection
module HashContent where

import           Data.Char
import           Data.Hashable
import qualified Data.Text                     as T
import           Data.Text                                ( Text )

hashContent :: Text -> Int
hashContent = hash . purify
  where
    purify :: Text -> Text
    purify = T.filter isAlpha
