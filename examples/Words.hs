-- This program is intended for performance analysis.  It simply
-- builds a Bloom filter from a list of words, one per line, and
-- queries it exhaustively.

module Main () where

import Control.Monad (forM_, mapM_)
import Data.BloomFilter (Bloom, fromListB, elemB, lengthB)
import Data.BloomFilter.Hash (cheapHashes)
import Data.BloomFilter.Easy (easyList, suggestSizing)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

conservative, aggressive :: Double -> [B.ByteString] -> Bloom B.ByteString
conservative = easyList

aggressive fpr xs
    = let (size, numHashes) = suggestSizing (length xs) fpr
          k = 3
      in fromListB (cheapHashes (numHashes - k)) (size * k) xs

testFunction = conservative

main = do
  args <- getArgs
  let files | null args = ["/usr/share/dict/words"]
            | otherwise = args
  forM_ files $ \file -> do
    a <- getCurrentTime
    words <- B.lines `fmap` B.readFile file
    putStrLn $ {-# SCC "words/length" #-} show (length words) ++ " words"
    b <- getCurrentTime
    putStrLn $ show (diffUTCTime b a) ++ "s to count words"
    let filt = {-# SCC "construct" #-} testFunction 0.01 words
    print filt
    c <- getCurrentTime
    putStrLn $ show (diffUTCTime c b) ++ "s to construct filter"
    {-# SCC "query" #-} mapM_ print $ filter (not . (`elemB` filt)) words
    d <- getCurrentTime
    putStrLn $ show (diffUTCTime d c) ++ "s to query every element"
