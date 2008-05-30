import Data.BloomFilter.Easy (easyList, elemB)

main = do
  filt <- (easyList 0.01 . words) `fmap` readFile "/usr/share/dict/words"
  let check word | word `elemB` filt = ""
                 | otherwise         = word ++ "\n"
  interact (concat . map check . lines)
