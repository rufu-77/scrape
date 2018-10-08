import Control.Monad
import System.IO
import System.Exit
import ScrapeTests

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    testResutls <- sequence [
        ScrapeTests.tests
      ]

    unless (and testResutls) System.Exit.exitFailure
