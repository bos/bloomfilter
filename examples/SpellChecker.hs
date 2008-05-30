import Data.BloomFilter.Easy (easyList, notElemB)

main = do
  filt <- (easyList 0.01 . words) `fmap` readFile "/usr/share/dict/words"
  interact (unlines . filter (`notElemB` filt) . lines)
