import Control.Monad (mapM_)
import Data.BloomFilter
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock

main = do
  a <- getCurrentTime
  words <- B.lines `fmap` B.readFile "words"
  putStrLn $ {-# SCC "length words" #-} show (length words) ++ " words"
  b <- getCurrentTime
  putStrLn $ show (diffUTCTime b a) ++ "s for word count"
  let filt = {-# SCC "construct" #-} easyList 0.01 words
  print filt
  c <- getCurrentTime
  putStrLn $ show (diffUTCTime c b) ++ "s for construct"
  {-# SCC "query" #-} mapM_ print $ filter (not . (`elemB` filt)) words
  d <- getCurrentTime
  putStrLn $ show (diffUTCTime d c) ++ "s for query"
  
