-- | Logging abstraction
module Logger
  ( debug
  , info
  , warn
  , error
  , rootLogger
  , setupLogger
  )
where

import           Control.Monad.Logger
import qualified Data.Text                     as T
import           System.IO ( FilePath )
import           System.Log.Logger
import           System.Log.Handler                       ( setFormatter )
import           System.Log.Formatter                     ( simpleLogFormatter )
import           System.Log.Handler.Simple                ( fileHandler )
import           Prelude                           hiding ( error )

debug :: (MonadLogger m) => String -> m ()
debug = logDebugN . T.pack

info :: (MonadLogger m) => String -> m ()
info = logInfoN . T.pack

warn :: (MonadLogger m) => String -> m ()
warn = logWarnN . T.pack

error :: (MonadLogger m) => String -> m ()
error = logErrorN . T.pack

-- | TODO: support ReaderT and maybe ResourceT
setupLogger :: FilePath -> IO ()
setupLogger path = do
  h <- fileHandler path DEBUG >>= \lh -> return $ setFormatter
    lh
    (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLogger (addHandler h)
  updateGlobalLogger rootLogger (setLevel INFO)

rootLogger :: String
rootLogger = ""
