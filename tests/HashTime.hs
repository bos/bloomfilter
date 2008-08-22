import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.BloomFilter.Hash
import System.Environment
import Data.List

main = do
  args <- getArgs
  forM_ args $ \f ->
    print =<< (foldl' hashSalt64 1 . B.lines) `fmap` B.readFile f
