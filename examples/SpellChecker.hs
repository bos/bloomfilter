import qualified Data.BloomFilter.Easy as BFE

main = do
  filt <- (BFE.easyList 0.01 . words) `fmap` readFile "/usr/share/dict/words"
  interact (unlines . filter (`BFE.notElem` filt) . lines)
